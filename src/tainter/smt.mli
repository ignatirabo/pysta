type zexpr
type zstatus = SAT | UNSAT | UNKNOWN
type zsymbol
type zsolver
val mk_true: zexpr
val mk_false: zexpr
val mk_numeral_i: int -> zexpr
val mk_bool: bool -> zexpr
val mk_bool_const: zsymbol -> zexpr
val mk_string: string -> zexpr
val mk_symbol: string -> zsymbol
val mk_const: zsymbol -> zexpr
val mk_add: zexpr list -> zexpr
val mk_sub: zexpr list -> zexpr
val mk_mul: zexpr list -> zexpr
val mk_div: zexpr -> zexpr -> zexpr
val mk_mod: zexpr -> zexpr -> zexpr
val mk_pow: zexpr -> zexpr -> zexpr
val mk_le: zexpr -> zexpr -> zexpr
val mk_lt: zexpr -> zexpr -> zexpr
val mk_eq: zexpr -> zexpr -> zexpr
val mk_not: zexpr -> zexpr
val mk_ge: zexpr -> zexpr -> zexpr
val mk_gt: zexpr -> zexpr -> zexpr
val mk_and: zexpr list -> zexpr
val mk_or: zexpr list -> zexpr
val to_string: zexpr -> string
val seq_empty: zexpr

(* Check *)
val check: zexpr list -> zstatus * zsolver
val get_string_model: zsolver -> string option
val get_args: zexpr -> Z3.Expr.expr list
val compare: zexpr -> zexpr -> int
val print_expr: out_channel -> zexpr -> unit
val print_prop: out_channel -> zexpr list -> unit
val pp_zstatus: out_channel -> zstatus -> unit

module SSet := String_helper.SSet
module SMap := String_helper.SMap
type valuation = zexpr SMap.t
val valua: valuation ref
val used_vnames: SSet.t ref
val clear_vnames: unit -> unit
val new_name: string -> string
type typ = Int | String | List of typ | Bool
val new_var: ?typ:typ option -> string -> string
val get_valuation: unit -> valuation
val get: string -> zexpr
