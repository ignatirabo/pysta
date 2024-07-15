module E := Language.Expr
type id = { line_number : int; important : bool; order : int; }
type elt = id * E.texpr
type t
val counter : int ref
val empty : t
val add : elt -> t -> t
val make_id : int -> bool -> id
val get_of_id : (id -> bool) -> t -> t
val to_list : t -> elt list
val union : t -> t -> t
val replace_id : id -> id -> t -> t
val length : t -> int
val extract : t -> id list
val pp : out_channel -> t -> unit
