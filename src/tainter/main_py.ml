module SSet = String_helper.SSet
module SMap = String_helper.SMap
module L = Language
module D = Domain
module S = Semantics

open Pyreir

let get_column row i  =
  Array.get row i |> Option.get

let main ( ) =
  let pysa_dir = "/Users/tiraboschi/pysa_project/" in
  let db_name = pysa_dir ^ "sapp.db" in
  let filename = "long_control.py" in
  let db = Sqlite3.db_open db_name in
  let query = "SELECT messages.contents, trace_frames.kind, trace_frames.caller_port, trace_frames.callee_location, trace_frames.callee_port FROM messages JOIN trace_frames
  ON messages.id = trace_frames.filename_id AND messages.kind='filename'
  AND trace_frames.kind='postcondition' AND messages.contents LIKE '%" ^ filename ^ "%'" in
  let rows = ref [] in
  let _ = Sqlite3.exec_no_headers db ~cb:(fun r -> rows := r :: !rows) query in
  (* The first row is the important one to know where to start *)
  Printf.printf "Finished: length rows = %d\n" (List.length !rows);
  let rec loop i rows =
    if (List.length rows > 0) then 
      let row = List.hd rows in
      let rows = List.tl rows in
      Printf.printf "[%s ; %s ; %s ; %s ; %s]\n" (get_column row 0) (get_column row 1) (get_column row 2) (get_column row 3) (get_column row 4);
      loop (i+1) rows
    else () in
  loop 0 !rows;
  let row = List.hd !rows in
  let location = get_column row 3 in
  let line = String.split_on_char '|' location |> List.hd |> int_of_string in
  Printf.printf "line = %d\n" line

(*
let old_main_1 ( ) =
  let _ = D.new_state in
  Printf.printf "Starting.\n";
  let open PyreAst.Parser in
  let content = "x = g(y,z)" in
  (* let content = "x = g(y)" in *)
  let ast =
    with_context (fun context ->
    match Concrete.parse_module ~context content with
    | Result.Error { Error.message; line; column; _ } ->
      let message =
        Format.sprintf "Parsing error at line %d, column %d: %s"
        line column message in
      failwith message
    | Result.Ok ast -> ast) in
  let prog = Ir_cast.modl_to_prog ast in
  Printf.printf "Generated IR program:\n";
  Ir_sig.pp_prog Format.std_formatter prog;
  let taint_prog = Ir_transfer.from_prog prog [] in
  Printf.printf "\n\nGenerated program:\n";
  Prog.pp_prog stdout taint_prog;
  let taint_block_aux = L.block_to_ext taint_prog.pmain in
  Printf.printf "\n\nGenerated aux program:\n";
  Prog.pp_block_ext "" stdout taint_block_aux;
  Printf.printf "Finished.\n"

let old_main ( ) =
  Printf.printf "Starting.\n";
  let filename = "general.pysa" in
  Printf.printf "filename: %s\n" filename;
  let file = Unix.openfile filename [ Unix.O_RDONLY ] 0o644 in
  let channel = Unix.in_channel_of_descr file in
  let lexbuf = Lexing.from_channel channel in
  let signatures : Signature.t list =
    try Pysasig_parser.signatures Pysasig_lexer.token lexbuf
    with Parsing.Parse_error ->
      failwith (Printf.sprintf "Parsing failed at line %d."
                  !Pysasig_lexer.num_line) in
  Printf.printf "Parsing sigs done! length: %d\n" (List.length signatures);
  Printf.printf "Finished.\n"
*)

let _ = ignore (main ( ))
