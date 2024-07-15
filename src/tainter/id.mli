type id = { key : int; temp : bool; message : string list; }
type t = id
val make : ?message:string list -> bool -> id
val get_id : id -> int
val get_temporal : id -> bool
val get_message : id -> string list
val set_message : string list -> id -> id
val add_message : string list -> id -> id
val pp : string -> out_channel -> id -> unit
