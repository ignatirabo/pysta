open Yojson.Basic

module SSet = String_helper.SSet
module SMap = String_helper.SMap

(* let from_zsolver (zs: Smt.zsolver) : t =
  match zs with
  |  *)

let from_telem (telem: Taint.telem) : t =
  `String telem.name

let from_telems (telems: Taint.telem SMap.t) : t =
    let tlms = SMap.bindings telems |> List.split |> snd in
    let rec aux (tlms: Taint.telem list) = match tlms with
      | [] -> []
      | tlm::tlms -> from_telem tlm :: (aux tlms) in
    `List (aux tlms)

let from_taint (taint: Taint.t) : t =
  let sources = from_telems taint.sources in
  let sinks = from_telems taint.sinks in
  `Assoc [("sources",sources); ("sinks",sinks) ]

let from_const (c: Language.Expr.const) : t =
  match c with
  | Cempty -> `String "empty list"
  | Cint i -> `Int i
  | Cbool b -> `Bool b 
  | Cstr s -> `String s

let from_uop (uop: Language.Ops.uop) : t =
  match uop with
  | Onot -> `String "Onot"

let from_bop (bop: Language.Ops.op) : t =
  match bop with
  | Oadd -> `String "Oadd"
  | Osub -> `String "Osub"
  | Omul -> `String "Omul"
  | Odiv -> `String "Odiv"
  | Omod -> `String "Omod"
  | Ole  -> `String "Ole"
  | Olt  -> `String "Olt"
  | Oeq  -> `String "Oeq"
  | One  -> `String "One"
  | Oge  -> `String "Oge"
  | Ogt  -> `String "Ogt"
  | Oand -> `String "Oand"
  | Oor  -> `String "Bop"
  | Opow -> `String "Pow"

let rec from_expr (e: Language.Expr.expr) : string * t =
  match e with
  | Econst c ->
    "Econst", from_const c
  | Evar v ->
    "Evar", `String v
  | Euop (uop, e) ->
    "Euop", `List [ from_uop uop ; `Assoc [ from_expr e ] ]
  | Eop (e0, op, e1) ->
    "Ebop", `List [ from_bop op ; `Assoc [ from_expr e0 ] ; `Assoc [ from_expr e1 ] ]
  | Ejoined ls ->
    "Ejoined", `Assoc (List.map from_expr ls)
  | Edict d ->
    let ls = SMap.bindings d in
    let ls = List.map (fun (str,e) -> str, `Assoc [ from_expr e ]) ls in
    "Edict", `Assoc ls
  | Elist ls ->
    let jls = List.map from_expr ls in
    "Elist", `Assoc jls
  | _ -> failwith "Impossible to find such expressions when using this function."

let rec from_texpr (te: Language.Expr.texpr) : string * t =
  match te with
  | Tconst (taint,c) ->
    "Tconst", `List [ from_taint taint; from_const c ]
  | Tvar (taint,v) ->
    "Tvar", `List [ from_taint taint; `String v ]
  | Tuop (taint,uop,e) ->
    "Tuop", `List [ from_taint taint; from_uop uop; `Assoc [ from_expr e ] ]
  | Tbop (taint,e0,bop,e1) ->
    "Tbop", `List [ from_taint taint; from_bop bop ; `Assoc [ from_expr e0 ] ; `Assoc [ from_expr e1 ] ]
  | Tlist tls ->
    let jls = List.map from_texpr tls in
    "Tlist", `Assoc jls
  | Tdict d ->
    let ls = SMap.bindings d in
    let ls = List.map (fun (str,te) -> str, `Assoc [ from_texpr te ]) ls in
    "Tdict", `Assoc ls
  | Tobj (cno, d) ->
    let cn =
      try
        let cn = Option.get cno in
        [ `String cn ]
      with _ ->
        [] in
    let ls = SMap.bindings d in
    let ls = List.map (fun (str,te) -> str, `Assoc [ from_texpr te ]) ls in
    "Tobj", `List (cn @ [ `Assoc ls ])

let from_return (teo: Language.Expr.texpr option) : string * t =
  match teo with
  | Some te -> "return", `Assoc [ from_texpr te ]
  | _ -> "return", `Assoc []

(* type config = { block: L.block_ext ; state: D.state ; flag: Flag.t ; id: Id.t ; counter: C.t } *)
let from_svm (svm: Svm.svm) : string * t =
  let ls = SMap.bindings svm in
  let ls = List.map (fun (str,te) -> str, `Assoc [ from_texpr te ]) ls in
  "svm", `Assoc ls

let from_sp (sp: Symbolic_path.t) : string * t =
  let ls = Symbolic_path.to_list sp in
  let ls = List.map (fun (_,te) -> `Assoc [ from_texpr te ]) ls in
  "sp", `List ls

let from_state (state: Domain.state) : string * t =
  "state", `Assoc [ from_svm state.svm ; from_sp state.sp ]

let from_trace (_,teo,cnf: Smt.zsolver * Language.Expr.texpr option * Semantics.config) : t =
  let json_teo = from_return teo in
  let json_state = from_state cnf.state in
  `Assoc [ json_teo ; json_state ]

let from_traces (trcs: (Smt.zsolver * Language.Expr.texpr option * Semantics.config) list) : t =
  let rec aux trcs: (string * t) list =
    match trcs with
    | [] -> []
    | trc::trcs -> ("trace", from_trace trc) :: aux trcs in
  `Assoc (aux trcs)