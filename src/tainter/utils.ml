(* Debug flags *)
let debug = ref false
let debug_funenter = ref false
let debug_smt_final = ref false

let list_extract (f: 'a -> bool) (l: 'a list) : 'a * 'a list =
  let rec aux (l: 'a list) (acc: 'a list) : 'a * 'a list =
    match l with
    | [] -> failwith "list_extract: no element found (maybe placeholder name?)"
    | e::l ->
      if f e then
        e, (List.rev acc) @ l
      else
        aux l (e::acc) in
  aux l []

let starts_with_capital str =
  match String.length str with
  | 0 -> false  (* Empty string *)
  | _ ->
    let first_char = String.get str 0 in
    'A' <= first_char && first_char <= 'Z'

let is_class_constructor str =
  let ls = String.split_on_char '.' str in
  let ls = List.rev ls in
  let hd = List.hd ls in
  let first_char = String.get hd 0 in
  'A' <= first_char && first_char <= 'Z'