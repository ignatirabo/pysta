module C = PyreAst.Concrete

let debug = ref false

let get_line (loc: C.Location.t) : int =
  (* Printf.printf "start.line=%d, start.column=%d; stop.line=%d, stop.column=%d\n"
    loc.start.line loc.start.column loc.stop.line loc.stop.column; *)
  loc.start.line

let get_location (loc: C.Location.t) : Ir_sig.location =
  { start=loc.start.line ; stop=loc.stop.line }

let cop_transfer (cop: C.ComparisonOperator.t) : Ir_sig.cop =
  match cop with
  | Eq -> Eq
  | NotEq -> NotEq
  | Lt -> Lt
  | Lte -> LtE
  | Gt -> Gt
  | Gte -> GtE
  | _ -> failwith "cop_transfer: Not implemented"

let bop_cast ?(location=None) (bop: C.BinaryOperator.t) : Ir_sig.bop =
  match bop with
  | Add -> Add
  | Sub -> Sub
  | Mult -> Mult
  | Div -> Div
  | Pow -> Pow
  | _ ->
    begin
      match (location: C.Location.t option) with
      | Some loc -> Printf.printf "bop_cast: start.line=%d, stop.line=%d\n" loc.start.line loc.stop.line
      | None -> Printf.printf "bop_cast: no location\n"
    end;
    failwith "bop_transfer: Binary operator not supported"

let identifier_to_id (idtf: C.Identifier.t) : Ir_sig.id =
  C.Identifier.to_string idtf

let constant_cast (c: C.Constant.t) : Ir_sig.const =
  match c with
  | False -> Ir_sig.False
  | True -> Ir_sig.True
  | Integer n -> Ir_sig.Integer n
  | String str -> Ir_sig.String str
  | _ -> failwith "constant_to_expr: Not implemented"

let str_expr (e: C.Expression.t) : string =
  match e with
  | NamedExpr _      -> "NamedExpr"
  | BoolOp _         -> "BoolOp"
  | UnaryOp _        -> "UnaryOp"
  | BinOp _          -> "BinOp"
  | Yield _          -> "Yield"
  | Lambda _         -> "Lambda"
  | IfExp _          -> "IfExpr"
  | Set _            -> "Set"
  | ListComp _       -> "ListComp"
  | SetComp _        -> "SetComp"
  | DictComp _       -> "DictComp"
  | Dict _           -> "Dict"
  | Await _          -> "Await"
  | YieldFrom _      -> "YieldFrom"
  | Compare _        -> "Compare"
  | Call _           -> "Call"
  | FormattedValue _ -> "FormattedValue"
  | GeneratorExp _   -> "GeneratorExp"
  | JoinedStr _      -> "JointedStr"
  | Constant _       -> "Constant"
  | Attribute _      -> "Attrbute"
  | Subscript _      -> "Subscript"
  | Starred _        -> "Starred"
  | Name _           -> "Name"
  | Tuple _          -> "Tuple"
  | Slice _          -> "Slice"
  | List _           -> "List"

let rec get_id (e: C.Expression.t) : Ir_sig.id =
  match e with
  | Name { id; _ } -> identifier_to_id id
  | Attribute { value ; attr ; _} ->
    let name = get_id value in
    let attr = identifier_to_id attr in
    name ^ "." ^ attr
  | Constant { value; _ } ->
    let value = constant_cast value in
    begin
      match value with
      | String value -> value
      | _ -> failwith "get_id: Constant is not a string."
    end
  | _ ->
    let str = str_expr e in
    failwith ("get_id: Unexpected case " ^ str)

let rec cexpr_cast ?(location: C.Location.t option = None) (e: C.Expression.t) : Ir_sig.expr =
  let open Ir_sig in
  match e with
  | Name {id; _} ->
    begin if !debug then try Printf.printf "case Name; line=%d\n" ((Option.get location).start.line) with _ -> () end;
    Name (identifier_to_id id)
  | Constant {value; _} -> Const (constant_cast value)
  | List {elts; _} ->
    let elts = List.map (cexpr_cast ~location) elts in
    List elts
  | Dict {keys; values; _} ->
    begin if !debug then try Printf.printf "case Dict; line=%d\n" ((Option.get location).start.line) with _ -> () end;
    let counter = ref 1 in
    let keys = List.map (fun e -> if !debug then Printf.printf "keys counter = %d\n" !counter; counter := !counter + 1; Option.get e |> cexpr_cast) keys in
    counter := 1;
    let values = List.map (cexpr_cast ~location) values in
    Dict (keys, values)
  | BoolOp {op; values; _} ->
    let op : bop = match op with
    | And -> And
    | Or -> Or in
    let values = List.map (cexpr_cast ~location) values in
    List.fold_left (fun acc value -> BOp (op,acc,value)) (List.hd values) (List.tl values)
  | BinOp {left; op; right; _} ->
    let bop = bop_cast ~location op in
    let left = cexpr_cast ~location left in
    let right = cexpr_cast ~location right in
    BOp (bop,left,right)
  | UnaryOp {op; operand; _} ->
    begin
      (* C.UnaryOperator.t *)
      match op with
      | Not -> UOp (Not, cexpr_cast ~location operand)
      | Invert -> failwith "cexpr_to_expr: Unexpected case Invert."
      | USub ->
        let expr = cexpr_cast ~location operand in
        BOp (Mult, expr, Const (Integer (-1)))
      (* failwith "cexpr_to_expr: Unexpected case USub." *)
      | _ -> failwith "cexpr_to_expr: Unexpected case."
    end
  | Call {func; args; _} ->
    begin if !debug then try Printf.printf "case Call; line=%d\n" ((Option.get location).start.line) with _ -> () end;
    let func = cexpr_cast ~location func in
    (* let rec aux (func: C.Expression.t) = match func with
      | Name {id; _} -> identifier_to_id id
      | Attribute {value; attr; _} -> aux value ^ "." ^ identifier_to_id attr
      | _ -> failwith "cexpr_cast: bad function call." in *)
    let args = List.map (cexpr_cast ~location) args in
    Call (func, args)
  | Attribute {value; attr; _} ->
    let value = cexpr_cast ~location value in
    let id = C.Identifier.to_string attr in
    Attr (value, id)
  | Compare {left; ops; comparators; _} ->
    let left = cexpr_cast ~location left in
    let ops = List.map cop_transfer ops in
    if (List.length ops != 1) then failwith "cexpr_cast: Too many comparisons";
    let comparators = List.map (cexpr_cast ~location) comparators in
    if (List.length comparators != 1) then failwith "cexpr_cast: Too many comparisons";
    Comp (List.hd ops, left, List.hd comparators)
  | Slice { upper; lower; step; _ } ->
    let upper = if Option.is_some upper then Some (cexpr_cast ~location (Option.get upper)) else None in
    let lower = if Option.is_some lower then Some (cexpr_cast ~location (Option.get lower)) else None in
    let step = if Option.is_some step then Some (cexpr_cast ~location (Option.get step)) else None in
    Slice (upper, lower, step)
  | Subscript { value; slice; _ } ->
    let value = cexpr_cast ~location value in
    let slice = cexpr_cast ~location slice in
    Subscript (value, slice)
  | FormattedValue { value ; _ } ->
    cexpr_cast ~location value
  | JoinedStr { values; _ } ->
    let values = List.map (cexpr_cast ~location) values in
    Joined values
  | Lambda { args; body; _ } ->
    (* let args = List.map (fun (a: C.Argument.t) -> identifier_to_id a.identifier) args.args in *)
    (* let body = cexpr_cast ~location body in *)
    (* Lambda (args, body) *)
    Printf.printf "cexpr_cast: ignoring Lambda\n";
    Const (String "Lambda")
  | _ ->
    begin
      match (location: C.Location.t option) with
      | Some loc -> Printf.printf "cexpr_cast: start.line=%d, stop.line=%d\n" loc.start.line loc.stop.line
      | None -> Printf.printf "cexpr_cast: no location\n"
    end;
    failwith "cexpr_cast: Not implemented"

let lexpr_cast (le: C.Expression.t) : Ir_sig.lexpr =
  match le with
  | Name { id; _ } -> Name (identifier_to_id id)
  | Subscript { value; slice; _ } ->
    let value = get_id value in
    let slice = cexpr_cast slice in
    Subscript (value, slice)
  | Attribute { value; attr; _ } ->
    let value = get_id value in
    let attr : Ir_sig.id = identifier_to_id attr in
    Attrib (value, attr)
  | _ -> failwith "lexpr_cast: unexpected"

let rec annotation_cast (annot: C.Expression.t) : Ir_sig.typ =
  match annot with
  | Name {id; _} ->
    begin match identifier_to_id id with
    | "int" -> Int
    | "str" -> String
    | _ -> failwith "Just have Int."
    end
  | Subscript {value; slice; _} ->
    (* Treat slice recursively *)
    let annot = annotation_cast slice in
    begin match value with
    | Name {id; _} ->
      begin match identifier_to_id id with
      | "list" -> List annot
      | _ -> failwith "Must be list right?"
      end
    | _ -> failwith "Supposed to be Name."
    end
  | _ -> failwith "Unexpected annotation."


exception IgnoreStatement of C.Statement.t

let str_stat (s: C.Statement.t) : string =
  match s with
  | Assign _ -> "Assign"
  | AsyncFunctionDef _ -> "AssyncFunctionDef"
  | AsyncFor _ -> "AsyncFor"
  | AsyncWith _ -> "AsyncWith"
  | While _ -> "While"
  | If _ -> "If"
  | Return _ -> "Return"
  | FunctionDef _ -> "FunctionDef"
  | ClassDef _ -> "ClassDef"
  | Delete _ -> "Delete"
  | AnnAssign _ -> "AnnAssign"
  | For _ -> "For"
  | With _ -> "With"
  | Match _ -> "Match"
  | Raise _ -> "Raise"
  | Try _ -> "Try"
  | TryStar _ -> "TryStar"
  | Assert _ -> "Assert"
  | Import _ -> "Import"
  | ImportFrom _ -> "ImportFrom"
  | Global _ -> "Global"
  | Nonlocal _ -> "Nonlocal"
  | Expr _ -> "Expr"
  | Pass _ -> "Pass"
  | Break _ -> "Break"
  | Continue _ -> "Continue"
  | AugAssign _ -> "AugAssign"

let pp_stat (s: C.Statement.t) : unit =
  let str = str_stat s in
  Printf.printf "%s" str

(* LVL 3*)
let rec stmt3_cast (s: C.Statement.t) : Ir_sig.stmt_ln =
  match s with
  | Assign {targets; value; location; _} ->
    if !debug then Printf.printf "case Assign; line=%d\n" location.start.line;
    let target = List.hd targets |> lexpr_cast in
    let stmt = Ir_sig.Assign (target, None, cexpr_cast ~location:(Some location) value) in
    let location = get_location location in
    { location ; stmt }
  | AnnAssign {target; annotation; value; location; _} ->
    if !debug then Printf.printf "case Assign; line=%d\n" location.start.line;
    let target = lexpr_cast target in
    let annot: Ir_sig.typ = annotation_cast annotation in
    let stmt = Ir_sig.Assign (target, Some annot, Option.get value |> cexpr_cast ~location:(Some location)) in
    let location = get_location location in
    { location ; stmt }
  | If {test; body; orelse; location; _} ->
    if !debug then Printf.printf "case If; line=%d\n" location.start.line;
    let test = cexpr_cast ~location:(Some location) test in
    let body = body3_cast body in
    let orelse = body3_cast orelse in
    let stmt = Ir_sig.If (test,body,orelse) in
    let location = get_location location in
    { location ; stmt }
  | While {test; body; location; _} ->
    if !debug then Printf.printf "case While; line=%d\n" location.start.line;
    let test = cexpr_cast ~location:(Some location) test in
    let body = body3_cast body in
    let stmt = Ir_sig.While (test,body) in
    let location = get_location location in
    { location ; stmt }
  | Return { value; location; _} ->
    if !debug then Printf.printf "case Return; line=%d\n" location.start.line;
    if Option.is_none value then
      failwith "stmt3_cast: Return with no value"
    else
      let value = Option.get value |> cexpr_cast ~location:(Some location) in
      let stmt = Ir_sig.Return value in
      let location = get_location location in
      { location ; stmt }
  | Expr { value; location } ->
    let value = cexpr_cast ~location:(Some location) value in
    let stmt = Ir_sig.Expr value in
    let location = get_location location in
    { location ; stmt }
  | For { target; iter; body; location; _ } ->
    let target = get_id target in
    let iter = cexpr_cast ~location:(Some location) iter in
    let body = body3_cast body in
    let location = get_location location in
    let stmt = Ir_sig.For (target,iter,body) in
    { location ; stmt }
  | Import _ | ImportFrom _ ->
    raise (IgnoreStatement s)
  | _ ->
    let str = str_stat s in
    failwith ("stmt3_cast: unhandled statement " ^ str ^ ".\n")

and body3_cast (body: C.Statement.t list) : Ir_sig.block =
  List.map stmt3_cast body |> List.rev

let get_annot (annot: C.Expression.t) : string =
  let rec aux (annot: C.Expression.t) acc =
  match annot with
  | Name { id ; _ } -> acc ^ identifier_to_id id
  | Attribute { value ; attr ; _ } ->
    let value = aux value "" in
    value ^ "." ^ (identifier_to_id attr)
  | _ -> failwith "get_annot: impossible." in
  let annot = aux annot "" in
  (* Printf.printf "annot = %s\n" annot; *)
  annot 

(* LVL 2*)
let rec fun_cast (name: C.Identifier.t) (args: C.Arguments.t) (body: C.Statement.t list)
  (returns: C.Expression.t option) (location: C.Location.t) : Ir_sig.proc =
  let id = identifier_to_id name in
  let args : C.Argument.t list = args.args in
  let inp : (Ir_sig.id * Ir_sig.annotation option) list = List.map (fun a ->
    let id = identifier_to_id a.C.Argument.identifier in
    let annotation =
      try
        let annot = Option.get a.C.Argument.annotation in
        let annot = get_annot annot in
        Some annot
      with Invalid_argument _ ->
        None in
    id, annotation) args in
    (* identifier_to_id a.C.Argument.identifier) args in *)
  let block = body3_cast body in
  let location = get_location location in
  { id; inp; block; location }

and clss_cast (name: C.Identifier.t) (body: C.Statement.t list) (location: C.Location.t) : Ir_sig.clss =
  let name = identifier_to_id name in
  let p = body0_cast ~depth:2 body Ir_sig.empty_prog in
  let block = p.main in
  let methods = p.procs in
  let location = get_location location in
  { name; block; methods; location }

and body0_cast ?(depth = 1) (b: C.Statement.t list) (p: Ir_sig.prog): Ir_sig.prog =
  match depth, b with
  | _, [] -> p
  | 1, (ClassDef {location; name; body; _})::b ->
    let clss = clss_cast name body location in
    let p = { p with clss=(clss::p.clss) } in
    body0_cast ~depth:1 b p
  | 1, (FunctionDef {name; args; body; returns; location; _})::b
  | 2, (FunctionDef {name; args; body; returns; location; _})::b ->
    let proc = fun_cast name args body returns location in
    let p = { p with procs=(proc::p.procs) } in
    body0_cast ~depth:2 b p
  | 2, (ClassDef _)::b' ->
    failwith "body0_cast: ClassDef at level 2 not allowed."
  | 3, (ClassDef _)::b' ->
    failwith "body0_cast: ClassDef at level 3 not allowed."
  | 3, (FunctionDef _)::b' ->
    failwith "body0_cast: FunctionDef at level 3 not allowed."
  | _, s::b ->
    let p =
      try
        let stmt = stmt3_cast s in 
        { p with main=stmt::p.main }
      with IgnoreStatement s ->
        p in
    body0_cast ~depth:depth b p

let body_cast (b: C.Statement.t list) : Ir_sig.prog =
  let prog : Ir_sig.prog = Ir_sig.empty_prog in
  body0_cast b prog

(** Conversion of main language components *)
let modl_to_prog (m: C.Module.t) : Ir_sig.prog = body_cast m.body
let modl_to_library (m: C.Module.t) : Ir_sig.proc list =
  (* let rec aux (p: Ir_sig.prog) : C.Statement.t list -> Ir_sig.prog =  function
  | [] -> p
  | f::fs ->
    let p = body0_cast ~depth:2 (f::fs) p in
    aux p fs in
  let p = aux Ir_sig.empty_prog m.body in *)
  let p = body0_cast ~depth:2 m.body Ir_sig.empty_prog in
  p.procs