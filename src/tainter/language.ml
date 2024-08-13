module SSet = String_helper.SSet
module SMap = String_helper.SMap

open Ppx_compare_lib.Builtin

module L = Location
module T = Taint

module Ops = struct
  (* unary *)
  type uop = Onot [@@deriving compare]
  (* binary *)
  type op_arith =
  | Oadd | Osub | Omul | Odiv | Omod | Opow [@@deriving compare]
  (* comparisons *)
  type op_bool =
  | Ole | Olt | Oeq | One | Oge | Ogt [@@deriving compare]
  (* logics *)
  type op_logic =
  | Oand | Oor [@@deriving compare]
  type op =
  | Oarith of op_arith | Obool of op_bool | Ologic of op_logic [@@deriving compare]
  let get_op_arith op = match op with
    | Oarith op -> op
    | _ -> failwith "get_op_arith: not an arithmetic operator."
  let get_op_bool op = match op with
    | Obool op -> op
    | _ -> failwith "get_op_bool: not a boolean operator."
  let get_op_logic op = match op with
    | Ologic op -> op
    | _ -> failwith "get_op_logic: not a logic operator."
  let uop_2str = function
    | Onot  -> "!"
  let op_arith_2str = function
    | Oadd  -> "+"
    | Osub  -> "-"
    | Omul  -> "*"
    | Odiv  -> "/"
    | Omod  -> "%"
    | Opow  -> "**"
  let op_bool_2str = function
    | Ole   -> "<="
    | Olt   -> "<"
    | Ogt   -> ">"
    | Oge   -> ">="
    | Oeq   -> "="
    | One   -> "!="
  let op_logic_2str = function
    | Oand  -> "&&"
    | Oor   -> "||"
  let op_2str = function
    | Oarith op -> op_arith_2str op
    | Obool op -> op_bool_2str op
    | Ologic op -> op_logic_2str op
  let op_arith_2fun (op: op_arith) : int -> int -> int =
    match op with
    | Oadd -> ( + )
    | Osub -> ( - )
    | Omul -> ( * )
    | Odiv -> ( / )
    | Omod -> ( mod )
    | Opow -> (fun x y -> int_of_float (float_of_int x ** float_of_int y))
  let op_bool_2fun (op: op_bool) : int -> int -> bool =
    match op with
    | Ole -> ( <= )
    | Olt -> ( < )
    | Oge -> ( >= )
    | Ogt -> ( > )
    | Oeq -> ( = )
    | One -> ( <> )
  let op_logic_2fun (op: op_logic) : bool -> bool -> bool =
    match op with
    | Oand -> ( && )
    | Oor -> ( || )
  let op_neg : op -> op = function
    | Oarith _ -> failwith "expr_neg: negation of arithmetic."
    | Ologic Oand -> Ologic Oor
    | Ologic Oor -> Ologic Oand
    | Obool Oeq -> Obool One
    | Obool One -> Obool Oeq
    | Obool Ole -> Obool Ogt
    | Obool Olt -> Obool Oge
    | Obool Oge -> Obool Olt
    | Obool Ogt -> Obool Ole
  let is_op_logic = function
    | Ologic _ -> true
    | _ -> false
end

module Expr = struct
  open Ops
  type vname = string [@@deriving compare]
  type cname = string [@@deriving compare]
  type const =
    | Cempty         (** empty list *)
    | Cint of int    (** integer constant *)
    | Cbool of bool  (** boolean constant *)
    | Cstr of string (** string constant  *)
    [@@deriving compare]
  type expr =
    | Econst     of const                (** constant       *)
    | Elist      of expr list            (** list           *)
    | Edict      of expr SMap.t          (** dictionary     *)
    | Evar       of vname                (** variable       *)
    | Euop       of Ops.uop * expr       (** unary negation *)
    | Eop        of expr * Ops.op * expr (** binary op      *)
    | Ecall      of expr * expr list    (** function call  *)
    | Esubscript of expr * const         (** list access    *)
    | Eattrib    of vname * expr         (** var name, attribute expr: possible name or fun call *)
    | Ejoined    of expr list            (** joined expressions (e.g., JoinedStr                 *)
    [@@deriving compare]
  type texpr =
    | Tconst     of Taint.t * const                (** constant       *)
    | Tlist      of texpr list                     (** list           *)
    | Tdict      of texpr SMap.t                   (** dictionary     *)
    | Tvar       of Taint.t * vname                (** variable       *)
    | Tuop       of Taint.t * Ops.uop * expr       (** unary negation *)
    | Tbop        of Taint.t * expr * Ops.op * expr (** binary op      *)
    | Tobj       of cname option * texpr SMap.t    (** object *)

  let rec texpr_2expr : texpr -> expr = function
    | Tconst (_,c) -> Econst c
    | Tlist ls ->
      let ls = List.map texpr_2expr ls in
      Elist ls
    | Tdict _ | Tobj _ ->
      failwith "texpr_2expr: TODO?"
    | Tvar (_,v) -> Evar v
    | Tuop (_,uop,e) -> Euop (uop,e)
    | Tbop (_,e0,op,e1) -> Eop (e0,op,e1)
    (* | Esubscript (_,v,i) -> Esubscript (v,i) *)
  let compare_texpr te0 te1 =
    let e0, e1 = texpr_2expr te0, texpr_2expr te1 in
    compare_expr e0 e1
  type lexpr =
    | Name of vname
    | Subscript of vname * expr (** Lists and dictionaries *)
    | Attrib of vname * expr
  let lexpr_get_name le = match le with
    | Name v | Subscript (v,_) | Attrib (v,_) -> v
  type t = expr

  let rec flatten_attr (e: expr) : string =
    match e with
    | Evar id -> id
    | Eattrib (id,e) -> id ^ "." ^ flatten_attr e
    | _ -> failwith "flatten_attr: unexpected expr."
  
  let rec get_last_attr (e: expr) : string =
    match e with
    | Evar id -> id
    | Eattrib (_,e) -> get_last_attr e
    | _ -> failwith "get_last_attr: unexpected expr."

  let rec get_attr_name (e: expr) : string =
    match e with
    | Evar id -> id
    | Eattrib (id,_) -> id
    | _ -> failwith "get_attr_name: unexpected expr."

  let expr_2str (e: expr) : string =
    match e with
    | Econst (Cstr s) -> s
    | Econst (Cint _) -> failwith "expr_2str: expression is integer."
    | Econst (Cbool _) -> failwith "expr_2str: expression is bool."
    | Evar _ -> failwith "expr_2str: expression is a variable."
    | _ -> failwith "expr_2str: expression is not a string."

  (* let compare te0 te1 =
  match te0, te1 with
  | Ecsti i, Ecsti j -> Int.compare i j
  | Ecstb b, Ecstb v -> Bool.compare b v
  | Evar v, Evar w -> String.compare v w
  | Euop uop *)

  let rec expr_2texpr (e:expr) (t: Taint.t) : texpr =
    match e with
    | Econst c -> Tconst (t,c)
    | Evar v -> Tvar (t,v)
    | Euop (uop,e) -> Tuop (t,uop,e)
    | Eop (e0,op,e1) -> Tbop (t,e0,op,e1)
    | Elist ls -> Tlist (List.map (fun e -> expr_2texpr e t) ls)
    | _ -> failwith "expr_2texpr: not supported."

  exception Not_proposition
  exception Empty_list
  let const_neg = function
    | Cint _ | Cstr _ -> raise Not_proposition
    | Cempty -> raise Empty_list
    | Cbool b -> Cbool (not b)
  let rec expr_neg e : expr =
    match e with
    | Econst c ->
      begin
        try
          Econst (const_neg c)
        with Empty_list ->
          Euop (Onot, Econst c)
      end
    | Evar _ | Ecall _ | Esubscript _ | Ejoined _ ->
      Euop (Onot, e)
      (* failwith "expr_neg: negation of unexpected expression." *)
    | Euop (Onot, e) -> e
    | Eop (e0, o, e1) ->
      let no = op_neg o in
      let e0 = if o = Ologic Oor || o = Ologic Oand then expr_neg e0 else e0 in
      let e1 = if o = Ologic Oor || o = Ologic Oand then expr_neg e1 else e1 in
      Eop (e0, no, e1)
    | Elist _ | Edict _ | Eattrib _ -> failwith "expr_neg: Negation of list, dict or obj."
  let texpr_neg : texpr -> texpr = function
    | Tconst (t,c) -> Tconst (t, const_neg c)
    | Tvar _ | Tlist _ | Tdict _ | Tobj _ -> failwith "texpr_neg: expression not supported."
    | Tuop (t,Onot,e) -> expr_2texpr e t
    | Tbop (t,e0, o, e1) ->
      let no = op_neg o in
      let e0 = if o = Ologic Oor || o = Ologic Oand then expr_neg e0 else e0 in
      let e1 = if o = Ologic Oor || o = Ologic Oand then expr_neg e1 else e1 in
      Tbop (t,e0, no, e1)

  let set_taint te t = match te with
    | Tconst (_,c) -> Tconst (t,c)
    | Tlist _ | Tdict _ | Tobj _ -> failwith "set_taint: does not apply? TODO"
    | Tvar (_,v) -> Tvar (t,v)
    | Tuop (_,uop,e) -> Tuop (t,uop,e)
    | Tbop (_,e0,op,e1) -> Tbop (t,e0,op,e1)
    (* | Esubscript (_,v,i) -> Esubscript (t,v,i) *)
  let rec add_taint (te: texpr) t' = match te with
    | Tconst (t,c) ->
      let t = T.comb t t' in
      Tconst (t,c)
    | Tvar (t,v) ->
      let t = T.comb t t' in
      Tvar (t,v)
    | Tlist ls ->
      let ls = List.map (fun te -> add_taint te t') ls in
      Tlist ls
    | Tdict td ->
      let td = SMap.map (fun te -> add_taint te t') td in
      Tdict td
    | Tobj (c, td) ->
      let td = SMap.map (fun te -> add_taint te t') td in
      Tobj (c, td)
    | Tuop (t, Onot, e) ->
      let t = T.comb t t' in
      Tuop (t, Onot, e)
    | Tbop (t,e0,op,e1) ->
      let t = T.comb t t' in
      Tbop (t,e0,op,e1)
  (* let set_expr (_,t) e = (e,t) *)
  let rec get_taint te = match te with
    | Tconst (t,_) | Tvar (t,_)
    | Tuop (t,_,_) | Tbop (t,_,_,_) -> t 
    | Tlist ls ->
      (* If there is at least one element, get taint of head, if not empty. *)
      begin try List.hd ls |> get_taint
      with _ -> Taint.empty end
    | Tdict _ | Tobj _ -> failwith "get_taint: TODO dict/obj case?"
  let get_expr = texpr_2expr
  let rec clear_taint = function
    | Tconst (_, e) -> Tconst (T.empty, e)
    | Tvar (_, v) -> Tvar (T.empty, v)
    | Tuop (_, uop, e) -> Tuop (T.empty, uop, e)
    | Tbop (_, e0, op, e1) -> Tbop (T.empty, e0, op, e1)
    | Tlist ls -> Tlist (List.map clear_taint ls)
    | Tdict td -> Tdict (SMap.map clear_taint td)
    | Tobj (c, td) -> Tobj (c, SMap.map clear_taint td)

  (** Turn list expression to list. *)
  let texpr_2list (te: texpr) : texpr list =
    match te with
    | Tlist ls -> ls
    | _ -> failwith "expr_2list: not a list."

  let vars_in_expr (e: expr) : SSet.t =
    let rec aux (e: expr) (vars: SSet.t) =
      match e with
      | Evar v -> SSet.add v vars
      | Euop (_, e0) -> aux e0 vars
      | Eop (e0, _, e1) ->
        let vars = aux e0 vars in
        let vars = aux e1 vars in
        vars
      | _ -> vars in
    aux e SSet.empty

  let pp_const chan : const -> unit = function
    | Cint i -> Printf.fprintf chan "%d" i
    | Cbool b -> Printf.fprintf chan "%b" b
    | Cstr str -> Printf.fprintf chan "\'%s\'" str
    | Cempty -> Printf.fprintf chan "[ ]"
  let rec pp_seq chan ls : unit =
    Printf.fprintf chan "[";
    let rec aux = function
    | [] -> Printf.fprintf chan ""
    | [x] -> Printf.fprintf chan "%a" pp_expr x
    | x::ls -> Printf.fprintf chan "%a," pp_expr x; aux ls in
    aux ls;
    Printf.fprintf chan "]"
  and pp_dict chan td =
    let ts = SMap.bindings td in
    Printf.fprintf chan "{ ";
    let rec aux = function
    | [] -> Printf.fprintf chan ""
    | [(k,v)] -> Printf.fprintf chan "'%s' : %a " k pp_expr v
    | (k,v)::ls -> Printf.fprintf chan "'%s' : %a , " k pp_expr v; aux ls in
    aux ts;
    Printf.fprintf chan "}";
  and pp_expr chan : expr -> unit = function
    | Econst c -> pp_const chan c
    | Elist s -> pp_seq chan s
    | Edict d -> pp_dict chan d
    | Evar x -> Printf.fprintf chan "%s" x
    | Euop (o, e0) ->
      Printf.fprintf chan "%s (%a)" (Ops.uop_2str o) pp_expr e0
    | Eop (e0, o, e1) ->
      Printf.fprintf chan "%a %s %a" pp_expr e0 (Ops.op_2str o) pp_expr e1
    | Ecall (f,exprs) ->
      Printf.fprintf chan "%s(" (flatten_attr f);
      let rec aux exprs : unit = match exprs with
        | [] -> ()
        | [v] -> pp_expr chan v
        | v::v'::exprs -> pp_expr chan v; Printf.fprintf chan ","; aux (v'::exprs) in
      aux exprs;
      Printf.fprintf chan ")"
    | Esubscript (Evar v, i) ->
      Printf.fprintf chan "%s[%a]" v pp_const i
    | Eattrib (v, attr) ->
      Printf.fprintf chan "%s.%a" v pp_expr attr
    | Esubscript (e,c) -> (* | _ -> *)
      Printf.fprintf chan "%a[%a]" pp_expr e pp_const c
      (* failwith "pp_expr: TODO implement subscript." *)
    | Ejoined values ->
      Printf.fprintf chan "Joined%a" pp_seq values

  let rec pp_tseq chan ls : unit =
    Printf.fprintf chan "[";
    let rec aux = function
    | [] -> Printf.fprintf chan ""
    | [x] -> Printf.fprintf chan "%a" pp_texpr x
    | x::ls -> Printf.fprintf chan "%a," pp_texpr x; aux ls in
    aux ls;
    Printf.fprintf chan "]"
  and pp_tdict chan td =
    let ts = SMap.bindings td in
    Printf.fprintf chan "{ ";
    let rec aux = function
    | [] -> Printf.fprintf chan ""
    | [(k,v)] -> Printf.fprintf chan "'%s' : %a " k pp_texpr v
    | (k,v)::ls -> Printf.fprintf chan "'%s' : %a , " k pp_texpr v; aux ls in
    aux ts;
    Printf.fprintf chan "}";
  and pp_texpr chan te =
    match te with
    | Tlist ls -> pp_tseq chan ls
    | Tdict td -> pp_tdict chan td
    | Tobj (clss, o) ->
      let clss = match clss with
        | None -> " Vague :"
        | Some clss -> " " ^ clss ^ " :" in
      Printf.fprintf chan "%s { " clss;
      SMap.iter (fun v expr -> Printf.fprintf chan "%s->%a; " v pp_texpr expr) o;
      Printf.fprintf chan "}"
    | _ ->
      let e, t = get_expr te, get_taint te in
      if Taint.t_2str t = "" then
        Printf.fprintf chan "(%a)" pp_expr e
      else
        Printf.fprintf chan "(%a,%s)" pp_expr e (Taint.t_2str t)
  
  let rec pp_prop chan = function
    | [] -> Printf.fprintf chan "_"
    | [e] -> Printf.fprintf chan  "%a" pp_expr e
    | e::els -> Printf.fprintf chan  "%a" pp_expr e; pp_prop chan els

  let rec pp_tprop chan = function
    | [] -> Printf.fprintf chan "_"
    | [e] -> Printf.fprintf chan  "%a" pp_texpr e
    | e::e'::els -> Printf.fprintf chan  "%a /\\ " pp_texpr e; pp_tprop chan (e'::els)

  let pp_lexpr chan = function
    | Name v -> Printf.fprintf chan "%s" v
    | Subscript (v,e) -> Printf.fprintf chan "%s[%a]" v pp_expr e
    | Attrib (v,e) -> Printf.fprintf chan "%s.%a" v pp_expr e

  exception Not_a_number

  let rec expr_2int (e: expr) : int =
    match e with
    | Econst (Cint n) -> n
    | Eop (e0, op, e1) ->
      let i0 = expr_2int e0 in
      let op = op_arith_2fun (get_op_arith op) in
      let i1 = expr_2int e1 in
      op i0 i1
    | _ -> pp_expr stdout e; raise Not_a_number

  let uop_eval (uop : Ops.uop) (se : Smt.zexpr) =
    let open Smt in
    match uop with
    | Onot -> mk_not se

  (** Evaluate binary operation op with Z3 expressions se0 and se1 *)
  let op_eval (op : Ops.op) (ze0 : Smt.zexpr) (ze1: Smt.zexpr) : Smt.zexpr =
    let open Smt in
    match op with
    | Oarith Oadd -> mk_add [ze0; ze1]
    | Oarith Osub -> mk_sub [ze0; ze1]
    | Oarith Omul -> mk_mul [ze0; ze1]
    | Oarith Odiv -> mk_div ze0 ze1
    | Oarith Omod -> mk_mod ze0 ze1
    | Oarith Opow -> mk_pow ze0 ze1
    | Obool Ole -> mk_le ze0 ze1
    | Obool Olt -> mk_lt ze0 ze1
    | Obool Oeq -> mk_eq ze0 ze1
    | Obool One -> mk_not (mk_eq ze0 ze1)
    | Obool Oge -> mk_ge ze0 ze1
    | Obool Ogt -> mk_gt ze0 ze1
    | Ologic Oand -> mk_and [ze0; ze1]
    | Ologic Oor -> mk_or [ze0; ze1]

  let const_to_zexpr (c: const) : Smt.zexpr =
    let open Smt in
    match c with
    | Cint i -> mk_numeral_i i
    | Cbool b -> mk_bool b
    | Cstr s -> mk_string s
    | Cempty -> seq_empty

  let rec seq_to_zexpr (ls: expr list) : Smt.zexpr =
    let open Smt in
    seq_empty
    (* FIX: I need to start adding elements to the empty list. *)
    (* Iterate on ls *)

  and expr_to_zexpr (e: expr) : Smt.zexpr =
    let open Smt in
    let valua = get_valuation () in
    match e with
    | Econst c -> const_to_zexpr c
    | Elist s -> seq_to_zexpr s
    | Edict d -> SMap.bindings d |> List.split |> snd |> seq_to_zexpr
    | Evar v -> SMap.find v valua
    | Euop (op, e) ->
      let e' = expr_to_zexpr e in
      uop_eval op e'
    | Eop (e0, op, e1) ->
      let e0' = expr_to_zexpr e0 in
      let e1' = expr_to_zexpr e1 in
      op_eval op e0' e1'
    | Ecall _ -> failwith "expr_to_zexpr: Ecall is incompatible."
    | Esubscript _ | Eattrib _ | Ejoined _ -> failwith "expr_to_zexpr: TODO implement?"
  
  let is_list (te: texpr) : bool =
    match te with
    | Tlist _ -> true
    | _ -> false
  let get_list (te: texpr) : texpr list =
    match te with
    | Tlist ls -> ls
    | _ -> failwith "get_list: not a list."
  let get_tmap (te: texpr) : texpr SMap.t =
    match te with
    | Tdict td
    | Tobj (_, td) -> td
    | _ -> failwith "get_tmap: not a dict or object."
  exception Vague_obj of string
  let get_clssname = function
    | Tobj (c, _) -> begin try Option.get c with _ -> raise (Vague_obj "get_clssname: object doesn't have name") end
    | _ -> failwith "get_clssname: not an object."
  let get_clssname_option = function
    | Tobj (c, _) -> c
    | e -> if !Utils.debug then Printf.printf "get_clssname_option: t-expression %a is not an object.\n" pp_texpr e; None
    (* failwith "get_clssname_option: not an object." *)

  let rec is_rule (te: texpr) : T.rule list =
    match te with
    | Tconst (t,_) | Tvar (t,_) | Tuop (t,_,_) | Tbop (t,_,_,_) ->
      begin match T.is_t_rule t with
      | None -> []
      | Some r -> [r]
      end
    | Tlist ls ->
      List.fold_left (fun acc te -> (is_rule te)@acc) [] ls
    | Tdict td | Tobj (_, td) ->
      SMap.fold (fun v te acc -> (is_rule te)@acc) td []

  (* let mk_uop ((uop,(e,t)): uop * texpr) : texpr =
    Euop (uop,e), t
  let mk_op (((e0,t0),op,(e1,t1)): texpr * op * texpr) : texpr =
    Eop (e0,op,e1), Taint.comb t0 t1 *)

  let is_int (c: const) : bool =
    match c with
    | Cint _ -> true
    | _ -> false
  let is_string (c: const) : bool =
    match c with
    | Cstr _ -> true
    | _ -> false
  let get_int (c: const) : int =
    match c with
    | Cint i -> i
    | _ -> failwith "get_int: not an int."
  let get_str (c: const) : string =
    match c with
    | Cstr s -> s
    | _ -> failwith "get_str: not a string."

  let hd = function
  | Elist ls -> List.hd ls
  | _ -> failwith "hd: not a list."
end

module E = Expr

type vname = E.vname
type expr = E.expr
type lexpr = E.lexpr
type typ = Int | String | List of typ | Bool
let rec typ_to_str = function
  | Int -> "int"
  | String -> "str"
  | List typ -> "list[" ^ typ_to_str typ ^ "]"
  | Bool -> "bool"
let rec typ_to_smt = function
  | Int -> Smt.Int
  | String -> Smt.String
  | Bool -> Smt.Bool
  | List typ -> Smt.List (typ_to_smt typ)

(** Statements *)
type stmt =
  | Assign of lexpr * typ option * expr       (* assignment *)
  | If     of expr * block * block   (* conditional statement *)
  | While  of expr * block        (* while *)
  | For    of vname * expr * block
  | Return of expr
  | Expr   of expr
  | SymbolicNew of vname
and ln_stmt = L.location * stmt
and block = ln_stmt list

type stmt_ext =
  | Assign of lexpr * typ option * expr
  | If     of expr * block_ext * block_ext
  | Constr of expr
  | While  of expr * block_ext
  | For    of vname * expr * block_ext
  | Loop   of expr * block_ext
  | Fun    of vname * vname list * vname
  | Return of expr
  | Expr   of expr
  | Clear  of vname
  | SymbolicNew of vname
and ln_stmt_ext = L.location * stmt_ext
and block_ext = ln_stmt_ext list

let strip_lc b = List.map (fun (_,s) -> s) b

let rec reverse_ext_aux b b' = match b with
  | (lc,s)::b -> reverse_ext_aux b ((lc, reverse_ext_stat s)::b')
  | [] -> b'
and reverse_ext_stat s = match s with 
| If (e,b0,b1) -> If (e, reverse_ext_aux b0 [], reverse_ext_aux b1 [])
| While (e,b) -> While (e, reverse_ext_aux b [])
| _ -> s
let reverse_ext b = reverse_ext_aux b []

let rec reverse_aux b b' = match b with
  | (lc,s)::b -> reverse_aux b ((lc, reverse_stat s)::b')
  | [] -> b'
and reverse_stat s : stmt = match (s: stmt) with
| If (e,b0,b1) -> If (e, reverse_aux b0 [], reverse_aux b1 [])
| While (e,b) -> While (e, reverse_aux b [])
| _ -> s
let reverse b = reverse_aux b []

let vars (b: block) : SSet.t =
  let rec aux_s (s: stmt) (res: SSet.t) =
    match s with
    | Assign (le,_,e) ->
      let v = E.lexpr_get_name le in
      let res = SSet.union (E.vars_in_expr e) res in
      SSet.add v res
    | If (e,b0,b1) ->
      let e_res = E.vars_in_expr e in
      let res = SSet.union e_res res in
      let res = aux_b b0 res in
      let res = aux_b b1 res in
      res
    | For (v,e,b) ->
      let e_res = E.vars_in_expr e in
      let res = SSet.union e_res res in
      let res = SSet.add v res in
      let res = aux_b b res in
      res
    | While (e,b) ->
      let e_res = E.vars_in_expr e in
      let res = SSet.union e_res res in
      let res = aux_b b res in
      res
    | Return e ->
      SSet.union (E.vars_in_expr e) res
    | Expr e ->
      SSet.union (E.vars_in_expr e) res
    | SymbolicNew _ ->
      res

  and aux_b (b: block) (res: SSet.t) =
    match b with
    | [] -> res
    | (_,s)::b ->
      let res = aux_s s res in
      aux_b b res
  in
  let res = aux_b b SSet.empty in
  res

(* let rec expr_normalize ?(sign:Signature.t list=[]) (e: expr) : expr =
match e with
  | Ecsti _ | Evar _ | Ecstb _ -> e 
  | Euop (uop, e) ->
    let e = expr_normalize ~sign e in
    Euop (uop, e)
  | Eop (e0, op, e1) ->
    let e0 = expr_normalize ~sign e0 in
    let e1 = expr_normalize ~sign e1 in
    Eop (e0, op, e1)
  | Ecall (f, inps) ->
    let out = f ^ "_out" in
    let expr: expr =
      (* try
        let sign = List.find (fun (s: Signature.t) -> s.name = f) sign in
        let taint_o = sign.Signature.output in
        let source = Option.get taint_o |> Taint.get_source in
        Esource ((f ^ "_out"), source)
      with _ -> *)
        Evar (f ^ "_out") in
    expr *)

let rec stmt_to_ext ?(sign:Signature.t list=[]) (s: stmt) : stmt_ext =
  match s with
  | Assign (le,typ,e) -> Assign (le,typ,e)
  | If (e,b1,b2) ->
    let b1 = block_to_ext ~sign b1 in
    let b2 = block_to_ext ~sign b2 in
    If (e,b1,b2)
  | For (v,e,b) ->
    let b = block_to_ext ~sign b in
    For (v,e,b)
  | While (e,b) ->
    let b = block_to_ext ~sign b in
    While (e,b)
  | Return e -> Return e
  | Expr e -> Expr e
  | SymbolicNew v -> SymbolicNew v

and stmt_to_lnext ?(sign:Signature.t list=[]) (loc: L.location) (s: stmt) : ln_stmt_ext =
  loc, stmt_to_ext ~sign s

and block_to_ext ?(sign:Signature.t list=[]) (b : block) =
  let ba = List.fold_left
    (fun ba (loc,s) -> (stmt_to_lnext ~sign loc s)::ba)
    [] b in
  List.rev ba

let rec ext_to_stmt (s: stmt_ext) : stmt =
  match s with
  | Assign (v,typ,e)-> Assign (v,typ,e)
  | If (e,b1a,b2a) ->
    let b1 = ext_to_block b1a in
    let b2 = ext_to_block b2a in
    If (e,b1,b2)
  | While (e,ba) | Loop (e,ba) ->
    let b = ext_to_block ba in
    While (e,b)
  | Return e ->
    Return e
  | _ -> failwith "saux_to_s: statement not expected."
and ext_to_block (be: block_ext) =
  let b = List.fold_left
    (fun b (lc,s) -> (lc, ext_to_stmt s) :: b)
    [] be in
  List.rev b