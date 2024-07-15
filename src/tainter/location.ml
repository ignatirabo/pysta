type location = { start: int; stop: int }
type t = location
let location_temp : location = { start=(-1) ; stop=(-1) }
let pp_location_start chan loc = Printf.fprintf chan "%d" loc.start

let loc_2str loc =
  "(" ^ (Int.to_string loc.start) ^ "," ^ (Int.to_string loc.stop) ^ ")"

let locs_2str locs =
  let rec aux acc locs =
    match locs with
    | [] -> ""
    | [loc] -> acc ^ loc_2str loc
    | loc1::loc2::locs -> aux (loc_2str loc1 ^ ";" ^ acc) (loc2::locs) in
  aux "" locs

let startloc_2str loc =
  Int.to_string loc.start

let startlocs_2str locs =
  let rec aux acc locs =
    match locs with
    | [] -> ""
    | [loc] -> acc ^ startloc_2str loc
    | loc1::loc2::locs -> aux (startloc_2str loc1 ^ ";" ^ acc) (loc2::locs) in
  aux "" locs

let start t = t.start
let stop t = t.stop