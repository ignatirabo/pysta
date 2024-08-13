module SSet = String_helper.SSet
module SMap = String_helper.SMap
module SP = Symbolic_path
module L = Language
module E = L.Expr
module SMT = Smt
type sp = SP.t
type svm = Svm.svm
type vname = string
type expr = E.expr
type texpr = E.texpr
type elt = texpr
type state = { svm : svm; sp : sp; }
val empty : state
val pp : string -> out_channel -> state -> unit
val get_sp : state -> sp
val get_svm : state -> svm
val set_sp : sp -> state -> state
val set_svm : svm -> state -> state
val new_var : ?typ:L.typ option -> state -> vname -> state
val new_obj : state -> vname -> string option -> state
val new_symbol : ?typ:L.typ option -> vname -> expr
val new_tsymbol : ?typ:L.typ option -> ?taint:Taint.t option -> vname -> texpr
val update_elt_opt : vname -> (elt option -> elt option) -> state -> state
val update_elt : vname -> elt -> state -> state
val find : ?typ:L.typ option -> vname -> state -> elt * state
val find_obj : ?clssname:string option -> vname -> state -> elt * state
val find_limited : vname -> state -> elt * state
val add_constraint :
  ?line_number:int -> ?important:bool -> E.texpr -> state -> state
val new_state : ?svm:svm option -> Prog.prog option -> state
val expr_to_zexpr : expr list -> SMT.zexpr list
val check : state -> SMT.zstatus * SMT.zsolver
val check_argument : state -> texpr -> SMT.zstatus * SMT.zsolver
