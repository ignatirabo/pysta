type arg = { name: string ; taint: Taint.sink option ; annotation: string option }

type sanitize = All | Parameters | TaintInTaintOut

type sig_fun = {
  name: string ;
  input: arg list ;
  self: bool ;
  output: Taint.source option ;
  sanitize: sanitize option
}

type sig_attr = {
  name: string ;
  output: Taint.source option
}

type t = SigFun of sig_fun | SigAttr of sig_attr

let str_of_arg (arg: arg) =
  let taint =
    if Option.is_some arg.taint then Option.get arg.taint |> Taint.get_sink
    else "None" in
  let annotation =
    if Option.is_some arg.annotation then Option.get arg.annotation
    else "" in
  arg.name ^ ": (" ^ taint ^ ") " ^ annotation

let pp_arg chan (arg: arg) = Printf.fprintf chan "%s" (str_of_arg arg)

let str_of_args (args: arg list) =
  List.fold_left (fun acc arg -> str_of_arg arg ^ ";" ^ acc) "" args 

let str_of_sanitize (sanitize: sanitize option) =
  match sanitize with
  | Some All -> "@Sanitize\n"
  | Some Parameters -> "@Sanitize(Parameters)\n"
  | Some TaintInTaintOut -> "@Sanitize(TaintInTaintOut)\n"
  | None -> ""

let pp_t chan (t: t) =
  match t with
  | SigFun t ->
    let output =
      if Option.is_some t.output then "(" ^ (Option.get t.output |> Taint.get_source) ^ ")"
      else "None" in
    let inputs = str_of_args t.input in
    let sanitize = str_of_sanitize t.sanitize in
    Printf.fprintf chan "%sdef %s (%s) : %s }\n" sanitize t.name inputs output
  | SigAttr t ->
    let output =
      if Option.is_some t.output then Option.get t.output |> Taint.get_source
      else "None" in
    Printf.fprintf chan "%s : %s = ...\n" t.name output

let get_name (t: t) =
  match t with
  | SigFun t -> t.name
  | SigAttr t -> t.name

let get_input (t: t) =
  match t with
  | SigFun t -> t.input
  | SigAttr _ -> failwith "get_input: is SigAttr."

let get_output (t: t) =
  match t with
  | SigFun t -> t.output
  | SigAttr t -> t.output

let get_sanitize (t: t) =
  match t with
  | SigFun t -> t.sanitize
  | SigAttr _ -> failwith "get_sanitize: is SigAttr."

let get_self (t: t) =
  match t with
  | SigFun t -> t.self
  | SigAttr _ -> failwith "get_self: is SigAttr."

let find (ss: t list) name : t option =
  try
    Some (List.find (fun t -> name = get_name t) ss)
  with Not_found ->
    None

let is_fun t : bool =
  match t with
  | SigFun _ -> true
  | _ -> false
let is_attr t : bool =
  is_fun t |> not