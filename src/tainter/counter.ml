let counter_debug = ref false

module type COUNTER = sig
  module L := Language
  type t
  val max: int
  val empty: t
  val step: L.block_ext -> t -> bool * t
  val delete: t -> t
  val pp: out_channel -> t -> unit
end

module CounterConstant4 : COUNTER = struct
  module L = Language
  type t = int list
  let max = 4
  let empty: t = [] 
  let step (bext: L.block_ext) counter =
    match bext, counter with
    | (_, Loop (_,_))::_, h::counter  ->
      if !counter_debug then
        Printf.printf "counter head = %d\n\n" h;
      if h < max then
        true, h+1::counter
      else
        false, counter
    | (_, While (_,_))::_, _ ->
      if !counter_debug then
        Printf.printf "new counter\n\n";
      true, 0::counter
    | _, _ -> true, counter

  let delete c =
    if !counter_debug then
      Printf.printf "delete counter\n\n";
    match c with
    | [] -> []
    | _::cs -> cs

  let pp chan (t: t) =
    let rec aux acc t = match t with 
      | [] -> ""
      | [c] -> acc ^ Int.to_string c
      | c::c'::cs -> aux (acc ^ (Int.to_string c ^ ",")) (c'::cs) in
    let s = aux "" t in
    Printf.fprintf chan "[%s]" s
end
