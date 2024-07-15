module E = Language.Expr
type id = { line_number: int ; important: bool ; order: int }
type elt = id * E.texpr
let compare_id id0 id1 =
  let ln0, ln1 = id0.line_number, id1.line_number in
  if ln0 < ln1 then
    -1
  else if ln0 > ln1 then
    1
  else
    match id0.important, id1.important with
    | false, true -> 1
    | true, false -> -1
    | _ -> 0

let compare elt0 elt1 =
  match compare_id (fst elt0) (fst elt1) with
  | -1 -> -1
  | 0 -> E.compare_texpr (snd elt0) (snd elt1)
  | 1 -> 1
  | _ -> failwith "Unexpected value"

module SpOrd = struct type t = elt let compare = compare end
module SpSet = Set.Make(SpOrd)
type t = SpSet.t

let counter = ref 0
let empty = SpSet.empty
let add (elt:elt) (sp:t) =
  SpSet.add elt sp
let make_id line_number important =
  counter := !counter + 1;
  let order = !counter in
  { line_number ; important ; order}
let get_of_id (f: id -> bool) = SpSet.filter (fun elt -> f (fst elt))
let to_list = SpSet.elements
let union sp1 sp2 = SpSet.union sp1 sp2
let replace_id id1 id2 sp = SpSet.map (fun (i,e) -> if i = id1 then (id2,e) else (id1,e)) sp 
let length = SpSet.cardinal

(** Extract returns a list of lines of codes. It reflects the order of execution of the program *)
let extract sp = SpSet.fold (fun (id,_) acc -> if id.important then id::acc else acc) sp []

let pp chan sp =
  let sp = to_list sp in
  let _,sp = List.split sp in
  E.pp_tprop chan sp