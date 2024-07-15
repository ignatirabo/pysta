module SSet := String_helper.SSet
module SMap := String_helper.SMap

module L = Location

module Ops : sig
  type uop = Onot
  type op = Oadd | Osub | Omul | Odiv | Omod | Opow | Ole | Olt | Oeq
          | One | Oge | Ogt | Oand | Oor
  val uop_2str : uop -> string
  val op_2str : op -> string
end

module Expr : sig
  type vname = string
  type cname = string
  type const =
    | Cempty         (** empty list       *)
    | Cint of int    (** integer constant *)
    | Cbool of bool  (** boolean constant *)
    | Cstr of string (** string constant  *) [@@deriving compare]
  type expr =
    | Econst     of const                (** constant *)
    | Elist      of expr list            (** list *)
    | Edict      of expr SMap.t          (** dictionary *)
    | Evar       of vname                (** variable         *)
    | Euop       of Ops.uop * expr       (** unary negation   *)
    | Eop        of expr * Ops.op * expr (** binary op        *)
    | Ecall      of expr * expr list    (** function call *)
    | Esubscript of expr * const         (** list access *)
    | Eattrib    of vname * expr         (** var name, attribute expr: possible name or fun call *)
    | Ejoined    of expr list            (** joined expressions (e.g., JoinedStr                 *)
    [@@deriving compare]
  type texpr =
    | Tconst     of Taint.t * const                (** constant *)
    | Tlist      of texpr list                     (** list *)
    | Tdict      of texpr SMap.t                   (** dictionary *)
    | Tvar       of Taint.t * vname                (** variable         *)
    | Tuop       of Taint.t * Ops.uop * expr       (** unary negation   *)
    | Tbop        of Taint.t * expr * Ops.op * expr (** binary op        *)
    | Tobj       of cname option * texpr SMap.t    (** object *)
  
  val compare_texpr : texpr -> texpr -> int
  type lexpr =
    | Name of vname (** Variable *)
    | Subscript of vname * expr (** Lists and dictionaries *)
    | Attrib of vname * expr (** Object attribute *)
  val lexpr_get_name : lexpr -> string
  type t = expr
  exception Not_a_number
  val flatten_attr : expr -> string
  val get_last_attr : expr -> string
  val get_attr_name : expr -> string
  val expr_2int : expr -> int
  val expr_2str : expr -> string
  val expr_neg : expr -> expr
  val texpr_neg : texpr -> texpr
  val expr_2texpr : expr -> Taint.t -> texpr
  val texpr_2expr : texpr -> expr
  val set_taint : texpr -> Taint.t -> texpr
  (* val set_expr : texpr -> expr -> texpr *)
  val get_taint : texpr -> Taint.t
  val get_expr : texpr -> expr
  val clear_taint : texpr -> texpr
  val add_taint : texpr -> Taint.t -> texpr
  val is_rule : texpr -> Taint.rule list
  (* val mk_uop : Ops.uop * texpr -> texpr
  val mk_op : texpr * Ops.op * texpr -> texpr *)
  val vars_in_expr : expr -> SSet.t
  val pp_expr : out_channel -> expr -> unit
  val pp_texpr : out_channel -> texpr -> unit
  val pp_prop : out_channel -> expr list -> unit
  val pp_tprop : out_channel -> texpr list -> unit
  val pp_lexpr : out_channel -> lexpr -> unit
  val expr_to_zexpr : expr -> Smt.zexpr
  val get_list : texpr -> texpr list
  val get_tmap : texpr -> texpr SMap.t
  exception Vague_obj of string
  val get_clssname : texpr -> string
  val get_clssname_option : texpr -> string option
  val is_list : texpr -> bool
  val is_int : const -> bool
  val is_string : const -> bool
  val get_int : const -> int
  val get_str : const -> string
  val hd : expr -> expr
end

module E := Expr
type vname := string
type expr := E.expr
type lexpr := E.lexpr
type typ = Int | String | List of typ | Bool
val typ_to_str : typ -> string

type stmt =
  | Assign of lexpr * typ option * expr
  | If     of expr * block * block
  | While  of expr * block
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
val strip_lc : ('a * 'b) list -> 'b list
val reverse_ext : block_ext -> block_ext
val reverse : block -> block
val vars : block -> SSet.t
(* val expr_normalize : ?sign:Signature.t list -> expr -> block_ext * expr *)
(* val stmt_to_ext : ?sign:Signature.t list -> stmt -> block_ext *)
val block_to_ext : ?sign:Signature.t list -> block -> block_ext
val ext_to_stmt : stmt_ext -> stmt
val ext_to_block : block_ext -> block
