type zexpr = Z3.Expr.expr
type zstatus = SAT | UNSAT | UNKNOWN
type zsymbol = Z3.Symbol.symbol
type zsolver = Z3.Solver.solver
let cfg = [("model", "true"); ("proof", "false")]
let ctx = Z3.mk_context cfg
let isort = Z3.Arithmetic.Integer.mk_sort ctx
let seq_isort = Z3.Seq.mk_seq_sort ctx isort

(* Helpers *)
(* let apply_pair f (e0,t0) (e1,t1) = f ctx e0 e1, Taint.comb t0 t1
let apply_list f zls =
  let (es,ts) = List.split zls in
  let e = f ctx es in
  let t = List.fold_left Taint.comb Taint.TNone ts in
  (e,t)
     *)

(* Expressions *)
let seq_empty = Z3.Seq.mk_seq_empty ctx seq_isort (* TODO: check whether we need isort or what sort. *)
let mk_seq_unit = Z3.Seq.mk_seq_unit ctx
let mk_true = Z3.Boolean.mk_true ctx
let mk_false = Z3.Boolean.mk_false ctx
let mk_numeral_i i = Z3.Arithmetic.Integer.mk_numeral_i ctx i
let mk_bool_const = Z3.Boolean.mk_const ctx
let mk_bool = Z3.Boolean.mk_val ctx
let mk_string str = Z3.Seq.mk_string ctx str
let mk_symbol s = Z3.Symbol.mk_string ctx s
let mk_const zs = Z3.Expr.mk_const ctx zs isort
let mk_add zls = Z3.Arithmetic.mk_add ctx zls
let mk_sub = Z3.Arithmetic.mk_sub ctx
let mk_mul = Z3.Arithmetic.mk_mul ctx
let mk_div = Z3.Arithmetic.mk_div ctx
let mk_mod ze0 ze1 = Z3.Arithmetic.Integer.mk_mod ctx ze0 ze1
let mk_pow ze0 ze1 = Z3.Arithmetic.mk_power ctx ze0 ze1
let mk_le ze0 ze1 = Z3.Arithmetic.mk_le ctx ze0 ze1
let mk_lt ze0 ze1 = Z3.Arithmetic.mk_lt ctx ze0 ze1
let mk_eq ze0 ze1 = Z3.Boolean.mk_eq ctx ze0 ze1
let mk_not ze = Z3.Boolean.mk_not ctx ze
let mk_ge ze0 ze1 = Z3.Arithmetic.mk_ge ctx ze0 ze1
let mk_gt ze0 ze1 = Z3.Arithmetic.mk_gt ctx ze0 ze1
let mk_and = Z3.Boolean.mk_and ctx
let mk_or = Z3.Boolean.mk_or ctx
let to_string (ze: zexpr) = (Z3.Expr.to_string ze)
let z3status_to_zstatus z3stat =
  match z3stat with
  | Z3.Solver.SATISFIABLE -> SAT
  | Z3.Solver.UNSATISFIABLE -> UNSAT
  | Z3.Solver.UNKNOWN -> UNKNOWN
(* Check *)
let check zls =
  let g = Z3.Goal.mk_goal ctx true false false in
  Z3.Goal.add g zls;
  let solver = Z3.Solver.mk_solver ctx None in
  List.iter (fun a -> (Z3.Solver.add solver [ a ])) (Z3.Goal.get_formulas g);
  let zstat = z3status_to_zstatus (Z3.Solver.check solver []) in
  (zstat, solver)
let get_string_model zsolv =
  let m = Z3.Solver.get_model zsolv in
  match m with
  | Some m -> Some (Z3.Model.to_string m)
  | None -> None
let get_args ze = Z3.Expr.get_args ze
let compare = Z3.Expr.compare

let print_expr chan ze = Printf.fprintf chan "%s" (to_string ze)
let rec print_prop chan xs : unit =
  match xs with
  | [] -> ()
  | [x] -> print_expr chan x
  | x::y::xs ->
    Printf.fprintf chan "%a /\\ " print_expr x;
    print_prop chan (y::xs)

let pp_zstatus chan = function
| SAT -> Printf.fprintf chan "SAT"
| UNSAT -> Printf.fprintf chan "UNSAT"
| UNKNOWN -> Printf.fprintf chan "UNKNOWN"

let set_taint (e,_) t = (e,t)
let get_taint = snd
let to_string_expr (e,_) =
  Z3.Expr.to_string e

module SSet = String_helper.SSet
module SMap = String_helper.SMap

(* module O = Language.Ops
module E = Language.Expr *)

(* SYMBOLS - START *************************)
(** Variable name *)
type vname = string

type valuation = zexpr SMap.t
let valua : valuation ref = ref SMap.empty
let used_vnames = ref SSet.empty
let clear_vnames : unit -> unit = fun _ -> used_vnames := SSet.empty
let new_name (v: vname) : vname =
  let rec aux vname = function
  | i0 ->
    let str0 = String.concat "" [vname; "_"; Int.to_string i0] in
    let op0 = (try Some (SSet.find str0 !used_vnames)
    with Not_found -> None) in
    if (op0 = None ) then (
      used_vnames := SSet.add str0 !used_vnames;
      str0)
    else
      aux vname (i0+1) in
  aux v 0

type typ = Int | String | List of typ | Bool

let new_var ?(typ=None) (v: vname) : vname =
  let v' = new_name v in
  let mk: string -> zexpr = begin match typ with
    | None ->
      (* Printf.printf "No type!\n\n\n\n"; *)
      (fun v -> mk_symbol v |> mk_const)
    | Some Int ->
      (* Printf.printf "Int type!\n\n\n\n"; *)
      (fun v -> mk_symbol v |> mk_const)
    | Some String ->
      (* Printf.printf "String type!\n\n\n\n"; *)
      mk_string
    | Some (List Int) ->
      (* Printf.printf "List type!\n\n\n\n"; *)
      (fun v -> mk_symbol v |> mk_const |> mk_seq_unit)
    | Some Bool ->
      (fun v -> mk_symbol v |> mk_bool_const)
    | _ -> failwith "We don't care"
  end in
  let zexp = mk v' in
  valua := SMap.add v' zexp !valua;
  v'

let get_valuation () = !valua
let get v = SMap.find v !valua
(* SYMBOLS - END *************************)

(* let uop_eval (uop : O.uop) (se : zexpr) =
  match uop with
  | Onot -> mk_not se

(** Evaluate binary operation op with Z3 expressions se0 and se1 *)
let op_eval (op : O.op) (ze0 : zexpr) (ze1: zexpr) : zexpr =
  match op with
  | Oadd -> mk_add [ze0; ze1]
  | Osub -> mk_sub [ze0; ze1]
  | Omul -> mk_mul [ze0; ze1]
  | Odiv -> mk_div ze0 ze1
  | Omod -> mk_mod ze0 ze1
  | Ole -> mk_le ze0 ze1
  | Olt -> mk_lt ze0 ze1
  | Oeq -> mk_eq ze0 ze1
  | One -> mk_not (mk_eq ze0 ze1)
  | Oge -> mk_ge ze0 ze1
  | Ogt -> mk_gt ze0 ze1
  | Oand -> mk_and [ze0; ze1]
  | Oor -> mk_or [ze0; ze1]

let rec expr_eval (e : E.expr): zexpr =
  match e with
  | Ecsti n ->
    let i = mk_numeral_i n in
    i
  | Ecstb b ->
    let b = mk_bool b in
    b
  | Evar v ->
    get v
  | Euop (op, e) ->
    let e' = expr_eval e in
    uop_eval op e'
  | Eop (e0, op, e1) ->
    let e0' = expr_eval e0 in
    let e1' = expr_eval e1 in
    op_eval op e0' e1'
  | _ -> failwith "SMT eval of Ecall"  *)
