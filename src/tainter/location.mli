type location = { start: int; stop: int (* column_start: int; column_end: int *) }
type t = location
val location_temp : location
val pp_location_start : out_channel -> location -> unit
val loc_2str : location -> string
val locs_2str : location list -> string
val startloc_2str : location -> string
val startlocs_2str : location list -> string
val start : location -> int
val stop : location -> int