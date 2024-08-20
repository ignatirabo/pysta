open Pyreir

module SSet = String_helper.SSet
module SMap = String_helper.SMap

module P = Prog
module L = Language
module Lo = Location
module C = Class
module E = L.Expr
module O = L.Ops
module F = Function
module S = Signature
type const = E.const
type expr = E.expr
type lexpr = E.lexpr
type stmt = L.stmt
type ln_stmt = L.ln_stmt
type block = L.block
type func = F.func
type clss  = C.clss
type prog = P.prog

let from_bop (bop: Ir_sig.bop) : O.op =
  match bop with
  | And  -> Ologic Oand
  | Or   -> Ologic Oor
  | Add  -> Oarith Oadd
  | Sub  -> Oarith Osub
  | Mult -> Oarith Omul
  | Div  -> Oarith Odiv
  | Pow  -> Oarith Opow

let from_cop (cop: Ir_sig.cop) : O.op =
  match cop with
  | Eq    -> Obool Oeq
  | NotEq -> Obool One
  | Lt    -> Obool Olt
  | LtE   -> Obool Ole
  | Gt    -> Obool Ogt
  | GtE   -> Obool Oge

let from_const (c: Ir_sig.const) : const =
  match c with
  | True -> Cbool true
  | False -> Cbool false
  | Integer i -> Cint i
  | String s -> Cstr s

let expr_2str (e: Ir_sig.expr) : string =
  match e with
  | Const (String s) -> s
  | _ -> failwith "expr_2str: Not a string."

let from_slice (slice: Ir_sig.expr) : const =
  match slice with
  | Const c -> from_const c
  | _ -> failwith "from_slice: slice is not a constant."

let rec from_expr (e: Ir_sig.expr) : expr * SSet.t =
  match e with
  | Const c -> Econst (from_const c), SSet.empty
  | List ls ->
    let ls, set = List.map from_expr ls |> List.split in
    Elist ls, List.fold_left (fun acc set -> SSet.union acc set) SSet.empty set
  | Dict (ks, es) ->
    let ks = List.map expr_2str ks in
    let es, vs = List.map from_expr es |> List.split in
    let ls = List.combine ks es in
    let d = List.fold_left (fun acc (k,v) -> SMap.add k v acc) SMap.empty ls in
    let vset = List.fold_left (fun acc vset -> SSet.union vset acc) SSet.empty vs in
    Edict d, vset
  | Name s -> Evar s, SSet.add s SSet.empty
  | UOp (uop,e) ->
    begin
      match uop with
      | Not ->
        let e, vars = from_expr e in
        (* Printf.printf "neg of e: %a\n" E.pp_expr e; *)
        (* E.expr_neg e, vars *)
        E.Euop (O.Onot, e), vars
    end
  | BOp (bop,e0,e1) ->
    begin
      let e0, vars0 = from_expr e0 in
      let e1, vars1 = from_expr e1 in
      E.Eop (e0,from_bop bop,e1), SSet.union vars0 vars1
    end
  | Comp (cop,e0,e1) ->
    begin
      let e0, vars0 = from_expr e0 in
      let e1, vars1 = from_expr e1 in
      E.Eop (e0,from_cop cop,e1), SSet.union vars0 vars1
    end
  | Call (f,exprs) ->
    let f, set = from_expr f in
    let exprs, sets = List.map from_expr exprs |> List.split in
    let set = List.fold_left (fun acc set -> SSet.union acc set) set sets in
    Ecall (f,exprs), set
  | Subscript (value, slice) ->
    let var, vset = from_expr value in
    let index = from_slice slice in
    Esubscript (var, index), vset
  | Slice _ -> failwith "from_expr: Slice TODO."
  | Attr (expr, id) ->
    let expr, set = from_expr expr in
    (* begin match expr with
    | Ecall (f,exprs) ->
      if Utils.starts_with_capital id then
        Ecall (id ^ "." ^ f, exprs), set
      else *)
        Eattrib (expr, id), set
    (* | _ ->
      Eattrib (id, expr), set *)
    (* end *)
  | Joined values ->
    let ls, set = List.map from_expr values |> List.split in
    Ejoined ls, List.fold_left (fun acc set -> SSet.union acc set) SSet.empty set

let from_lexpr (le: Ir_sig.lexpr) : lexpr * SSet.t =
  match le with
  | Name v -> Name v, SSet.singleton v
  | Subscript (v, e) ->
    let e, vset = from_expr e in
    Subscript (v, e), SSet.add v vset
  | Attrib (v, e) ->
    let e : E.expr = Econst (Cstr e) in
    Attrib (v, e), SSet.empty

let from_location (location: Ir_sig.location) : Lo.location =
  { start=location.start ; stop=location.stop }

let typ_transform (typ: Ir_sig.typ option) : L.typ option =
  match typ with
  | None -> None
  | Some typ ->
    let rec aux typ =
      match (typ: Ir_sig.typ) with
      | Int -> L.Int
      | String -> L.String
      | List typ -> L.List (aux typ) in
    Some (aux typ)

let rec from_stmt (s: Ir_sig.stmt_ln) (vars: SSet.t) : ln_stmt * SSet.t =
  match s.stmt with
  | Assign (le, typ, e) ->
    let e, vars_e = from_expr e in
    let le, vars_le = from_lexpr le in
    let location = from_location s.location in
    let typ = typ_transform typ in
    let stmt: ln_stmt = location, Assign (le, typ, e) in
    stmt, SSet.union vars_e vars_le
  | If (e,b0,b1) ->
    let b0, vars = from_block b0 [] vars in
    let b1, vars = from_block b1 [] vars in
    let e, vars_e = from_expr e in
    let location = from_location s.location in
    (location, If (e, b0, b1)), SSet.union vars_e vars
  | While (e,b) ->
    let b, vars = from_block b [] vars in
    let e, vars_e = from_expr e in
    let location = from_location s.location in
    (location, While (e, b)), SSet.union vars_e vars
  | Return e ->
    let e, vars_e = from_expr e in
    let location = from_location s.location in
    (location, Return e), SSet.union vars_e vars
  | Expr e ->
    let e, vars_e = from_expr e in
    let location = from_location s.location in
    (location, Expr e), SSet.union vars_e vars
  | For (target,iter,body) ->
    let b, vars = from_block body [] vars in
    let iter, vars_iter = from_expr iter in
    let location = from_location s.location in
    (location, For (target, iter, b)), SSet.union vars_iter vars

and from_block (b: Ir_sig.block) (bacc: block) (vars: SSet.t): block * SSet.t =
  match b with
  | [] -> bacc, vars
  | stmt::b ->
    let stmt, vars = from_stmt stmt vars in
    from_block b (stmt::bacc) vars

let from_proc ?(sign=[]) ?(clss=false) (p: Ir_sig.proc) : func =
  let open Signature in
  let body, _ = from_block p.block [] SSet.empty in
  let body = Some body in
  (* Try to find signarure of proc. *)
  let sign: S.t = try
      List.find (fun s -> let b = S.get_name s = p.id in if b && !Utils.debug then Printf.printf "Signature for %s found.\n" (S.get_name s); b) sign
    with Not_found ->
      let input: S.arg list = List.map (fun (name, annotation) -> { name ; taint=None ; annotation }) p.inp in
      SigFun { name=p.id ; input ; output=None ; sanitize=None ; self=(List.length input > 0 && clss) } in
  let location: Lo.location = { start=p.location.start ; stop=p.location.stop } in
  { sign ; body ; location }

let from_clss ?(sign=[]) (c: Ir_sig.clss) : clss =
  let name = c.name in
  let methods : F.t = List.fold_left
    (fun acc p -> SMap.add p.Ir_sig.id (from_proc ~sign:sign ~clss:true p) acc) SMap.empty c.methods in
  let body, _ = from_block c.block [] SSet.empty in
  { name; methods; body }

let from_prog (p: Ir_sig.prog) (sign: Signature.t list) : Prog.t =
  let main, vars = from_block p.main [] SSet.empty in
  if !Utils.debug then begin
    Printf.printf "from_prog: printing signatures.\n";
    List.iter (fun (s: Signature.t) -> Printf.printf "sign: %s\n" (S.get_name s)) sign
  end;
  let funcs : F.t = List.fold_left
    (fun acc p -> SMap.add p.Ir_sig.id (from_proc ~sign:sign p) acc) SMap.empty p.procs in
  let clss : C.t = List.fold_left
    (fun acc (c: Ir_sig.clss) -> SMap.add c.name (from_clss c) acc) SMap.empty p.clss in
  let vars = SSet.elements vars in
  { pmain=main ; funcs ; pglobs=vars ; lib=SMap.empty ; classes=clss }
