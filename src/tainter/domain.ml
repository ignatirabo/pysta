module SSet = String_helper.SSet
module SMap = String_helper.SMap

module SP = Symbolic_path
module L = Language
module E = L.Expr
module SMT = Smt

type sp    = SP.t
type svm   = Svm.t
type vname = string
type expr  = E.expr
type texpr = E.texpr
type elt = texpr

type state = {
  svm : svm; (** Symbolic map *)
  sp : sp; (** Symbolic path *)
  }

let empty = { svm=Svm.empty ; sp=SP.empty }

let pp (indent: string) chan a =
  Printf.fprintf chan "%sSVM: " indent;
  Svm.iter
    (fun v expr -> Printf.fprintf chan "%s->%a;%s" v E.pp_texpr expr indent)
    a.svm;
  Printf.fprintf chan "\n";
  let sp = a.sp in
  begin
    if SP.length sp > 0 then
      Printf.fprintf chan "%s SP: %a\n" indent SP.pp sp
    else
      Printf.fprintf chan "%s SP: empty\n" indent
    end

let get_sp k = k.sp
let get_svm k = k.svm
let set_sp sp k = {k with sp=sp}
let set_svm svm k = {k with svm=svm}

(** Assigns fresh value to v, ignoring previous value or creating v if necessary *)
let new_var ?(typ=None) (k: state) (v: vname) : state =
  let typ = try Some (L.typ_to_smt (Option.get typ)) with _ -> None in
  let sv = SMT.new_var ~typ v in
  let e: texpr = Tvar (Taint.empty,sv) in
  { k with svm = Svm.update v (fun _ -> Some e) k.svm }

let new_obj (k: state) (v: vname) (clssname: string option) : state =
  let e: texpr = Tobj (clssname, SMap.empty) in
  { k with svm = Svm.update v (fun _ -> Some e) k.svm }

let typ_to_smt (typ: Language.typ option) : SMT.typ option =
  let rec aux typ =
  match (typ: Language.typ) with
  | Int -> SMT.Int
  | String -> SMT.String
  | List t -> SMT.List (aux t)
  | Bool -> SMT.Bool
 in
  match typ with
  | None -> None
  | Some t -> Some (aux t)

(** Creates new symbolic, not related to any variable *)
let new_symbol ?(typ=None) (v: vname) : expr =
  let sv = SMT.new_var ~typ:(typ_to_smt typ) v in
  Evar sv
let new_tsymbol ?(typ=None) ?(taint=None) (v: vname) : texpr =
  let sv = SMT.new_var ~typ:(typ_to_smt typ) v in
  let taint = match taint with
    | None -> Taint.empty
    | Some taint -> taint in
  match typ with
  | None | Some L.Int | Some L.String ->
    Tvar (taint, sv)
  | Some L.List L.Int ->
    Tlist [Tvar (taint, sv)]
  | _ -> failwith "No lists of strings."

(** Update value of variable with option *)
let update_elt_opt (v: vname) (f: elt option -> elt option) k =
  { k with svm = Svm.update v f k.svm }

(** Update replaces the value if it exists, or adds it if it does not *)
let update_elt (v: vname) (elt: elt) (k: state) =
  let svm = Svm.update v (fun _ -> Some elt) k.svm in
  { k with svm }

(** Return value of variable if it exists in state, if not create a new value *)
let find ?(typ=None) (v: vname) (k: state) : elt * state =
  try
    Svm.find v k.svm, k
  with Not_found ->
    let k = new_var ~typ k v in
    Svm.find v k.svm, k

let find_obj ?(clssname=None) (v: vname) (k: state) : elt * state =
  try
    Svm.find v k.svm, k
  with Not_found ->
    let k = new_obj k v clssname in
    Svm.find v k.svm , k

let find_limited (v: vname) (k: state) : elt * state =
  try
    Svm.find v k.svm, k
  with Not_found ->
    failwith ("find_limited: element " ^ v ^ " doesn't exist.")

(** Adds constraint to the SP. No checks are done.
    Variables should already be replaced by symbols. *)
let add_constraint ?line_number:(line_number=0) ?important:(important=false) (e: E.texpr) (state: state) =
  let id = SP.make_id line_number important in
  { state with sp = SP.add (id,e) state.sp }

let new_state ?svm:(svm=None) (p: Prog.t option) : state =
  let k = { svm=SMap.empty ; sp=SP.empty } in
  match svm with
  | Some svm -> set_svm svm k
  | None -> k

let expr_to_zexpr (ls: expr list) : SMT.zexpr list =
  List.map E.expr_to_zexpr ls

let check (s: state) =
  let ls = SP.to_list s.sp |> List.map (fun (_,te) -> E.texpr_2expr te) in
  let zls = expr_to_zexpr ls in
  SMT.check zls

(* let rec expr_to_num (e: expr) : int =
  match e with
  | Econst (Cint i) -> i
  | _ -> failwith "Not a number." *)

(* let rec expression_simplify (e: expr) : expr =
  match e with
  | Econst _ -> e
  | Elist ls ->
    Elist (List.map expression_simplify ls)
  | Euop _ -> e
  | Eop (e0,op,e1) ->
    begin
      match op with
      | Oadd | Osub | Omul | Odiv | Omod ->
        let i0 = expression_simplify e0 |> expr_to_num in
        let i1 = expression_simplify e1 |> expr_to_num in
        begin
        match op with
          | Oadd -> Econst (Cint (i0 + i1))
          | Osub -> Econst (Cint (i0 - i1))
          | Omul -> Econst (Cint (i0 * i1))
          | Odiv -> Econst (Cint (i0 / i1))
          | Omod -> Econst (Cint (i0 mod i1))
          | _ -> failwith "Not possible"
        end
      | Oeq | One -> failwith "Not implemented"
      | Ole | Olt
      | Oge | Ogt | Oand | Oor -> failwith "Not implemented"
    end
  | _ -> failwith "expression_simplify: found unresolved term in expression." *)

(** Check whether an expression that has been already "evaluated"
    is true. If false, we ask the SMT solver. *)
(* let rec check_bool (e: expr) : bool =
  (* Assumption is: no function calls, no variables. Just constants. *)
  match e with
  | Econst (Cbool b) -> b
  | Euop (Onot,e) -> not (check_bool e)
  | Eop (e0,op,e1) ->
    match op with
    |  *)

let check_argument (s: state) (te: texpr) =
  let ls = SP.to_list s.sp |> List.map (fun (_,te) -> E.texpr_2expr te) in
  let te = E.texpr_2expr te in
  let zls = expr_to_zexpr (te::ls) in
  SMT.check zls
