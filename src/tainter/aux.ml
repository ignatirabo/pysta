let list_nth_replace (i: int) (v: 'a) (ls: 'a list) : 'a list =
  let rec aux i v ls acc =
    if (i <= 0) then
      (List.rev acc) @ [v] @ (List.tl ls)
    else
      match ls with
      | [] -> failwith "list_nth_replace: index is out of range."
      | v'::ls -> aux (i-1) v ls (v'::acc) in
  aux i v ls []
