module SSet = String_helper.SSet
module SMap = String_helper.SMap

module L = Language
module Lo = Location
module C = Class
module E = L.Expr
module F = Function
module S = Signature

(** Procedures *)
(* type proc = { pname:     string ;  (** procedure name *)
              pbody:     L.block ; (** procedure body *)
              pinput:    (string * Taint.sink option) list;
              poutput:   Taint.source option;
              location:  L.location} *)

(** Programs *)
type prog = { pglobs: string list ; (** global variables *)
              funcs: F.t ;          (** procedures *)
              lib: F.t ;            (** library of procs *)
              classes: C.t ;        (** defined classes *)
              pmain:  L.block ;     (** body of main *) }

let empty_prog = { pglobs=[] ; pmain=[] ; funcs=F.empty ; lib = F.empty ; classes=C.empty }
type t = prog

let lc = ref 0

(* Binding variables *)
(* - Fixes the type of each variable occurrence
* - Rejects programs with undefined variables
* - Rejects programs with calls to undefined procedures *)
let binding (prog: prog) : prog =
  prog
  (* lc := 0;
  let rec liner ((_,s): L.ln_stmt) : L.ln_stmt =
    lc := !lc + 1;
    match s with
    | If (e,b0,b1) ->
      let lc_s = !lc in
      let b0 = List.map liner b0 in
      let b1 = List.map liner b1 in
      (lc_s, If (e,b0,b1))
    | While (e,b) ->
      let lc_s = !lc in
      let b = List.map liner b in
      (lc_s, While (e,b))
    | _ -> !lc, s in
  let pmain = List.map liner prog.pmain in
  { prog with pmain } *)
  (* let do_procname pname body =
    let rec do_stat env s =
      let rec do_vname n = function
        | [ ] ->
            failwith (Printf.sprintf "var %s not found in proc %s" n pname)
        | x :: env ->
            if n = Var.get_vname x then x else do_vname n env in
      let do_var x = do_vname (Var.get_vname x) env in
      let rec do_expr e =
        match e with
        | L.Ecsti _ | L.Ecstb _ -> e
        | L.Evar x -> L.Evar (do_var x)
        | L.Euop (o, e0) -> L.Euop (o, do_expr e0)
        | L.Eop (e0, o, e1) -> L.Eop (do_expr e0, o, do_expr e1) in
      match s with
      | lc, L.Sassign (x, e) ->
          env, (lc, L.Sassign (do_var x, do_expr e))
      | lc, L.Sinput x ->
          env, (lc, L.Sinput (do_var x))
      | lc, L.Sif (e, b0, b1) ->
          env, (lc, L.Sif (do_expr e, do_block env b0, do_block env b1))
      | lc, L.Swhile (e, b) ->
          env, (lc, L.Swhile (do_expr e, do_block env b))
      | lc, L.Sfun (n,v) ->
          env, (lc, L.Sfun (n, v))
    and do_block env block =
      let _, block =
        List.fold_left
          (fun (env,b) stat ->
            let env, stat = do_stat env stat in
            env, stat :: b
          ) (env, [ ]) block in
      List.rev block in
    { pname = pname ;
      pbody = do_block prog.pglobs body } in
  let do_proc p = do_procname p.pname p.pbody in
  { prog with
    funcs = List.map do_proc prog.funcs;
    pmain  = (do_procname "main" prog.pmain).pbody } *)

let reverse (prog: prog) : prog =
  let pmain = L.reverse prog.pmain in
  let funcs = SMap.map
                (fun func -> { func with F.body =
                  (match func.F.body with
                  | Some b -> Some (L.reverse b)
                  | None -> None) }) prog.funcs in
  { prog with pmain ; funcs } 

(** Try to get global function or library function or class method *)
let get_func n (p: prog) : F.func =
  try
    SMap.find n p.funcs
  with Not_found ->
    (* try *)
      SMap.find n p.lib
    (* with Not_found ->
      (* It has to be a class constructor *)
      let clss = SMap.find n p.classes in
      V *)

let get_func_body (func: F.func) = func.body

(* Source is the line of code where the taint spawned.                     *
* Source should be found in main, and it should be a function assignment. *)
let get_taint_ln source p =
  let rec aux block =
  match (block : L.block) with
  | (ln, If (_,b0,b1))::block -> begin
    if source = ln then failwith "get_taint_ln: line is not a function assignment."
    else
      try aux b0 with _ ->
        try aux b1 with _ ->
          aux block end
  | (ln, While (_,b))::block -> begin
    if source = ln then failwith "get_taint_ln: line is not a function assignment."
    else
      try aux b with _ ->
        aux block end
  | (ln, Assign _)::block -> 
    if source = ln then failwith "get_taint_ln: line is not a function assignment."
    else aux block
  | _ -> failwith "get_taint_ln: not found" in
  aux p.pmain

(* Pretty-printing *)
let rec pp_stat indent chan s = match (s: L.stmt) with
  | Assign (x, typ, e) ->
    let typ_str =
      match typ with
      | Some typ -> ": " ^ L.typ_to_str typ
      | None -> "" in
    Printf.fprintf chan "%s%a%s = %a\n" indent E.pp_lexpr x typ_str E.pp_expr e
  | If (e, b0, b1) ->
    let indentn = indent^"    " in
    if List.length b1 = 0 then
      Printf.fprintf chan "%sif ( %a ):\n%a%s\n" indent E.pp_expr e
        (pp_block indentn) b0 indent
    else
      Printf.fprintf chan "%sif ( %a ):\n%a%selse:\n%a%s\n" indent E.pp_expr e
        (pp_block indentn) b0 indent (pp_block indentn) b1 indent
  | For (v, e, b) ->
    let indentn = indent ^ "    " in
    Printf.fprintf chan "%sfor %s in ( %a ):\n%a%s\n" indent v E.pp_expr e
      (pp_block indentn) b indent
  | While (e, b) ->
    let indentn = indent ^ "    " in
    Printf.fprintf chan "%swhile ( %a ):\n%a%s\n" indent E.pp_expr e
      (pp_block indentn) b indent
  | Return e ->
    Printf.fprintf chan "%sreturn %a\n" indent E.pp_expr e
  | Expr e ->
    Printf.fprintf chan "%s%a\n" indent E.pp_expr e
  | SymbolicNew v ->
    Printf.fprintf chan "%sSymbolic.new(%s)" indent v

and pp_block indent chan = function
  | [ ] -> ( )
  | s :: b -> Printf.fprintf chan "%a: %a%a" Lo.pp_location_start (fst s) (pp_stat indent) (snd s) (pp_block indent) b

let rec pp_stmt_ext indent chan : L.stmt_ext -> unit = function
  | Assign (x, typ, e) ->
    let typ_str =
      match typ with
      | Some typ -> ": " ^ L.typ_to_str typ
      | None -> "" in
    Printf.fprintf chan "%s%a%s = %a\n" indent E.pp_lexpr x typ_str E.pp_expr e
  | If (e, b0, b1) ->
    let indentn = indent^"    " in
    if List.length b1 = 0 then
      Printf.fprintf chan "%sif( %a ){\n%a%s}\n" indent E.pp_expr e
        (pp_block_ext indentn) b0 indent
    else
      Printf.fprintf chan "%sif( %a ){\n%a%s} else {\n%a%s}\n" indent E.pp_expr e
        (pp_block_ext indentn) b0 indent (pp_block_ext indentn) b1 indent
  | For (v, e, b) ->
    let indentn = indent ^ "    " in
    Printf.fprintf chan "%sfor %s in ( %a ):\n%a%s\n" indent v E.pp_expr e
      (pp_block_ext indentn) b indent
  | While (e, b) ->
    let indentn = indent^"    " in
    Printf.fprintf chan "%swhile( %a ){\n%a%s}\n" indent E.pp_expr e
      (pp_block_ext indentn) b indent
  | Loop (e, b) ->
    let indentn = indent^"    " in
    Printf.fprintf chan "%sloop( %a ){\n%a%s}\n" indent E.pp_expr e
      (pp_block_ext indentn) b indent
  | Constr e ->
    Printf.fprintf chan "%sconstraint( %a );\n" indent E.pp_expr e 
  | Fun (f, inps, out) ->
    let rec aux ils : string = match ils with
      | [] -> ""
      | [v] -> v
      | v::ils -> v ^ "," ^ (aux ils) in
    Printf.fprintf chan "%s(%s)" f (aux inps)
  | Return e ->
      Printf.fprintf chan "%sreturn %a\n" indent E.pp_expr e
  | Expr e ->
      Printf.fprintf chan "%s%a\n" indent E.pp_expr e
  | SymbolicNew v ->
      Printf.fprintf chan "%sSymbolic.new(%s)\n" indent v
  | Clear v ->
      Printf.fprintf chan "%sClear(%s)\n" indent v
and pp_block_ext indent chan = function
  | [ ] -> ( )
  | s :: b -> Printf.fprintf chan "%a%a" (pp_stmt_ext indent) (snd s) (pp_block_ext indent) b

let pp_decl indent chan (v: string) =
  Printf.fprintf chan "%s%s;\n" indent v
let pp_security chan (sec : int SMap.t) =
  let aux indent chan sec = SMap.iter
    (fun v i ->
      Printf.fprintf chan "%s%s -> %d\n" indent v i)
    sec in
  Printf.fprintf chan "security:\n  {\n%a  }\n" (aux "    ") sec
let pp_func chan (func: F.func) =
  Printf.fprintf chan "%s" (S.get_name func.sign);
  (* let rec inp_aux = function 
    | [] -> ""
    | [(v,t)] ->
      if Option.is_some t then
        v ^ ":" ^ (Option.get t |> Taint.get_sink)
      else
        v ^ ":_"
    | (v,t)::inp ->
      let acc = if Option.is_some t then
        v ^ ":" ^ (Option.get t |> Taint.get_sink) ^ ","
      else
        v ^ ":_," in
      acc ^ inp_aux inp in *)
  Printf.fprintf chan "(%s)" (S.str_of_args (S.get_input func.sign));
  (* begin try
    Printf.fprintf chan " (%s) :" (Taint.taint_2string (Option.get p.pinput));
  with _ -> Printf.fprintf chan " :" end; *)
  begin try
    Printf.fprintf chan ": %s" (Option.get (S.get_output func.sign) |> Taint.get_source);
  with _ ->
    () end;
  begin try
    Printf.fprintf chan "\n {\n%a }\n" (pp_block "    ") (Option.get func.body)
  with _ ->
    Printf.fprintf chan "\n { }\n"
  end

let pp_iflow chan (x,i1,i2) = Printf.fprintf chan "(%s,%d,%d)\n" x i1 i2
let pp_prog chan p =
  (* List.iter (pp_decl "" chan) p.pglobs; *)
  (* List.iter (pp_iflow chan) p.piflow; *)
  SMap.iter (fun _ func -> pp_func chan func) p.funcs;
  if List.length p.pmain > 0 then
    let sign : S.t = SigFun { name="main" ; input=[] ; output=None ; sanitize=None ; self=false } in
    pp_func chan { sign ; body=Some p.pmain ; location={start=1;stop=1} }
let pp_globs chan p =
  Printf.fprintf chan "{";
  Printf.fprintf chan " %s" (List.hd p.pglobs);
  List.iter
    (fun v -> Printf.printf "; %s" v)
    (List.tl p.pglobs);
  Printf.fprintf chan " }"

(* let link_sig p signatures : prog =
  let aux p (signature: Signature.t) =
    let name = signature.name in
    try
      let proc, procs = Utils.list_extract (fun (proc: proc) -> (* Printf.printf "proc.pname=%s\n" proc.pname; *) proc.pname = name) p.funcs in
      let pinput = List.map (fun (arg: Signature.arg) -> (arg.name,arg.taint)) signature.input in
      let poutput = signature.output in
      let proc = { proc with pinput ; poutput } in
      { p with funcs=proc::procs }
    with _ ->
      p in
  List.fold_left aux p signatures *)

(* type proc = { pname:     string ;  (** procedure name *)
              pbody:     L.block ; (** procedure body *)
              pinput:    (string * Taint.taint option) list;
              poutput:   Taint.taint option;
              location:  L.location} *)

let symbolic_lib : F.func list =
  let symbolic_new : F.func =
    begin
      let sign: S.t = SigFun { name="Symbolic.new" ; input=[] ; output=None ; sanitize=None ; self=false } in
      let body: L.block option = Some [ Lo.location_temp, L.SymbolicNew "symb" ] in
      let location = Lo.location_temp in
      { sign ; body ; location }
    end in
  (* let symbolic_int : proc =
    begin
      let pname = "Symbolic.int" in
      let location = L.location_temp in
      let pbody : L.block = [ L.location_temp, L.SymbolicNew "new" ] in
      let pinput = [] in
      let poutput = None in
      { pname ; pbody ; pinput ; poutput ; location }
    end in *)
  (* [ symbolic_new; symbolic_int ] *)
  [ symbolic_new ]