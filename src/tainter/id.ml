(** Id for states *)
type id = { key: int ; temp: bool ; message: string list }
type t = id
let global_id = ref 0
let global_temp_id = ref 0
let make ?message:(message=[]) temp : id =
  if temp then
    begin
      global_temp_id := !global_temp_id - 1;
      { key=(!global_temp_id); temp; message}
    end
  else
    begin
      global_id := !global_id + 1;
      { key=(!global_id); temp; message }
    end
let get_id t = t.key
let get_temporal t = t.temp
let get_message t = t.message
let set_message message t = { t with message } 
let add_message message t = { t with message=message@t.message }
let pp indent chan t =
  match t.temp, t.message with
  | true, [] ->
    Printf.fprintf chan "%s(%d, Temporary)" indent t.key
  | true, _ ->
    Printf.fprintf chan "%s(%d, Temporary, %s)" indent t.key (String.concat "; " t.message)
  | false, [] ->
    Printf.fprintf chan "%s(%d)" indent t.key
  | false, s ->
    Printf.fprintf chan "%s(%d, %s)" indent t.key (String.concat "; " s)
