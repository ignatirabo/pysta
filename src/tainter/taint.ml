module SSet = String_helper.SSet
module SMap = String_helper.SMap

module L = Location

type telem = { name: string ; locs: L.t list }
type source = Source of telem
type sink = Sink of telem
type rule  = { name: string; code: int; sources: string list; sinks: string list }
type taints = { sources: source list; sinks: sink list; rules: rule list }
let empty_config : taints = { sources=[] ; sinks=[] ; rules=[] }
let taint_config = ref empty_config

type t = { sources: telem SMap.t; sinks: telem SMap.t }

let empty = { sources=SMap.empty ; sinks=SMap.empty }

let telem_add_loc (loc: L.location) (tlm: telem) =
  let locs = loc::tlm.locs in
  { tlm with locs }
let sink_add_loc (loc: L.location) (sink: sink) =
  match sink with
  | Sink tlm -> Sink (telem_add_loc loc tlm)
let source_add_loc (loc: L.location) (source: source)  =
  match source with
  | Source tlm -> Source (telem_add_loc loc tlm)

let sink_2t sink = match sink with
  | Sink s ->
    let sources = SMap.empty in
    let sinks = SMap.add s.name s SMap.empty in
    { sources ; sinks }
let source_2t source = match source with
  | Source s ->
    let sources = SMap.add s.name s SMap.empty in
    let sinks = SMap.empty in
    { sinks ; sources }

let get_sink = function
  | Sink t -> t.name
let get_source = function
  | Source t -> t.name

let json_string = function
  | `String str -> str
  | _ -> failwith "Unexpected"

let json_source (j: Yojson.Safe.t) : source =
  match j with
  | `Assoc [(_,name);(_,comment)] ->
    let name = json_string name in
    (* let comment = json_string comment in *)
    Source { name ; locs=[] }
  | _ -> failwith "Unexpected"

let json_sources (j: string * Yojson.Safe.t) : source list =
  match j with
  | "sources", `List ls -> List.map json_source ls
  | _ -> failwith "Unexpected"

let json_sink (j: Yojson.Safe.t) : sink=
  match j with
  | `Assoc [(_,name);(_,comment)] ->
    let name = json_string name in
    (* let comment = json_string comment in *)
    Sink { name ; locs=[] }
  | _ -> failwith "Unexpected"

let json_sinks (j: string * Yojson.Safe.t) : sink list =
  match j with
  | "sinks", `List ls -> List.map json_sink ls
  | _ -> failwith "Unexpected"

let json_int (j: Yojson.Safe.t) : int = match j with
  | `Int i -> i
  | _ -> failwith "Unexpected"

let json_rule (j: Yojson.Safe.t) : rule =
  match j with
  | `Assoc [(_,name);(_,code);(_,sources);(_,sinks);_] ->
    let name = json_string name in
    let code = json_int code in
    let sources : string list = match sources with
      | `List ls -> List.map json_string ls
      | _ -> failwith "Unexpected" in
    let sinks : string list = match sinks with
      | `List ls -> List.map json_string ls
      | _ -> failwith "Unexpected" in
    { name ; code ; sources ; sinks }
  | _ -> failwith "Unexpected"

let json_rules (j: string * Yojson.Safe.t) : rule list = match j with
  | "rules", `List ls -> List.map json_rule ls
  | _ -> failwith "Unexpected"

let json (j: Yojson.Safe.t) : taints =
  match j with
  | `Assoc [so;si;_;ru;_] ->
    let sources = json_sources so in
    let sinks = json_sinks si in
    let rules =  json_rules ru in
    { sources ; sinks ; rules }
  | _ -> failwith "Taint.json: unexpected"

let is_rule (sources: string list) (sinks: string list) : rule option =
  let rec check_rules : rule list -> rule option = function
    | [] -> None
    | r::ls ->
      if List.exists (fun so -> List.exists (fun so' -> so = so') sources) r.sources && List.exists (fun si -> List.exists (fun si' -> si = si') sinks) r.sinks then
        Some r
      else
        check_rules ls in
  check_rules !taint_config.rules

let is_t_rule t =
  let sources: string list = SMap.bindings t.sources |> List.split |> fst in
  let sinks: string list = SMap.bindings t.sinks |> List.split |> fst in
  is_rule sources sinks

let telem_union (tlm1: telem) (tlm2: telem) =
  if tlm1.name = tlm2.name then
    let name = tlm1.name in
    (* It's not really union... *)
    let locs = tlm1.locs @ tlm2.locs in
    { name ; locs }
  else
    failwith "telem_union: different names"

let tmap_lub t1 t2 =
  SMap.filter_map (fun k tlm1 ->
    try
      let tlm2 = SMap.find k t2 in
      Some (telem_union tlm1 tlm2)
    with _ ->
      None) t1

let tmap_glb t1 t2 =
  SMap.fold (fun k tlm1 acc ->
    SMap.update k (fun tlm_o ->
      match tlm_o with
      | None -> Some tlm1
      | Some tlm2 -> Some (telem_union tlm1 tlm2)) acc) t1 t2

(* let tset_add t = function
  | None -> SSet.add t SSet.empty
  | Some tS -> SSet.add t tS *)

(* let subset t1 t2 = SSet.subset t1 t2 *)
let subset t1 t2 =
  let t1 = SMap.bindings t1 |> List.split |> fst |> SSet.of_list in
  let t2 = SMap.bindings t2 |> List.split |> fst |> SSet.of_list in
  SSet.subset t1 t2

(* let lub t0 t1 =
  let sources = tset_lub t0.sources t1.sources in
  let sinks = tset_lub t0.sinks t1.sinks in
  { sources ; sinks } *)

let glb t0 t1 =
  let sources = tmap_glb t0.sources t1.sources in
  let sinks = tmap_glb t0.sinks t1.sinks in
  { sources ; sinks }

let comb = glb

let comb_option to0 to1 =
  match to0, to1 with
  | Some t0, Some t1 -> Some (comb t0 t1)
  | None, Some t | Some t, None -> Some t
  | _ -> None

let telem_2str (telem: telem) =
  "{" ^ telem.name ^ "," ^ L.startlocs_2str telem.locs ^ "}"

let map_2str (s: telem SMap.t) =
  let tlms = SMap.bindings s |> List.split |> snd in
  let rec aux (tlms: telem list) acc = match tlms with
    | [] -> ""
    | [tlm] -> acc ^ telem_2str tlm
    | tlm1::tlm2::tlms -> aux (tlm2::tlms) (telem_2str tlm1 ^ " , " ^ acc) in
  aux tlms ""

let map_2json (s: telem SMap.t) =
  let tlms = SMap.bindings s |> List.split |> snd in
  let rec aux (tlms: telem list) acc = match tlms with
    | [] -> ""
    | [tlm] -> acc ^ telem_2str tlm
    | tlm1::tlm2::tlms -> aux (tlm2::tlms) (telem_2str tlm1 ^ " , " ^ acc) in
  aux tlms ""

let t_2str t =
  let str_sources =
      if SMap.cardinal t.sources > 0 then
        let str_so = map_2str t.sources in
        (* "TaintSource[" ^ str_so  ^ "]" in *)
        "SO[" ^ str_so  ^ "]"
      else
        "" in
  let str_sinks =
      if SMap.cardinal t.sinks > 0 then
        let str_si = map_2str t.sinks in
        (* "TaintSinks[" ^ str_si  ^ "]" in *)
        "SI[" ^ str_si  ^ "]"
      else
        "" in
  if str_sources <> "" && str_sinks <> "" then
    str_sources ^ "," ^ str_sinks
  else if str_sources <> "" then
    str_sources
  else if str_sinks <> "" then
    str_sinks
  else
    ""
