let vname_output s = "out" ^ s
let vname_input s = "in" ^ s

let filter_split (b: 'a -> bool) (ls: 'a list) =
  List.fold_left
    (fun (ls_t, ls_f) a ->
        if b a then
          ((a :: ls_t), ls_f)
        else
          (ls_t, (a :: ls_f)))
    ([],[]) ls

let get_lc (lc,_) = lc
