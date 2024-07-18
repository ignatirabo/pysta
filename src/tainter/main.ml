module SSet = String_helper.SSet
module SMap = String_helper.SMap
module S = Signature

(* let analyze p (mode, rel_dom, non_rel_dom, apron) : bool * bool = *)
let analyze ?(json=false) filename prog sign locs (config: Mode.t) : Yojson.Basic.t =
  match config.mode with
  | M_interpret ->
    failwith "Interpreter is not implemented"
  | M_analysis -> begin
    match config.analysis with
    | S_pytaint ->
      Printf.printf "Python taint analyzer...\n";
      let ctx : Semantics.context = { prog ; sign ; locs } in
      let cnfs = Semantics.analyze_prog ~json filename ctx in
      let json_data = Json.from_traces cnfs in
      begin
        if json then
          let json_string = Yojson.Basic.to_string json_data in
          let json_string = Test.replace_double_quote json_string in
          print_endline json_string
          (* () *)
      end;
      json_data
    end
  (* | _ -> failwith ("Mode undefined: " ^ (Mode.mode_to_str config.mode) ^ ".") *)

let get_column row i  =
  Array.get row i |> Option.get

let check_sapp_db filename db : Location.t option =
  let filename = String.split_on_char '/' filename |> List.rev |> List.hd in
  (* let pysa_dir = "../pysa_playground/" in *)
  let db_name = db in
  let db = Sqlite3.db_open db_name in
  let query = "SELECT messages.contents, trace_frames.kind, trace_frames.caller_port, trace_frames.callee_location, trace_frames.callee_port FROM messages JOIN trace_frames
  ON messages.id = trace_frames.filename_id AND messages.kind='filename'
  AND trace_frames.kind='postcondition' AND messages.contents LIKE '%" ^ filename ^ "%'" in
  let rows = ref [] in
  let _ = Sqlite3.exec_no_headers db ~cb:(fun r -> rows := r :: !rows) query in
  try
    let row = List.hd !rows in
    let location = get_column row 3 in
    let start = String.split_on_char '|' location |> List.hd |> int_of_string in
    Some { start ; stop=(-1) }
  with _ ->
    None

let main ( ) =
  Printf.printf "Starting.\n";
    let config = Mode.parse() in
    let json_taints = Yojson.Safe.from_file config.file_taint in
    let taints : Taint.taints = Taint.json json_taints in
    Taint.taint_config := taints;
    let signatures =
      if String.length config.file_sig > 0 then
        Mode.signatures config.file_sig
      else [] in
    let libs = Mode.library config.dir_lib signatures in
    (* Add special symbolic functions *)
    let libs = List.fold_left (fun libs func -> SMap.add (S.get_name func.Function.sign) func libs) libs Prog.symbolic_lib in
    begin
      match config.smode with
      | SM_file ->
        Printf.printf "File mode: %s\n" config.file_prog;
        let locs: Location.t option =
          if String.length config.locs > 0 then
            (* User locations override SAPP's db *)
            Some (Mode.locations config.locs)
          else
            begin
              Printf.printf "Checking SAPP db for entry point.";
              (* Check SAPP database to get locations. *)
              let loc_opt = check_sapp_db config.file_prog config.db in
              if Option.is_some loc_opt then
                Printf.printf " Entry point found at line %d.\n" (Option.get loc_opt).start
              else
                Printf.printf " No entry point found.\n";
              loc_opt
            end in
        Printf.printf "\n\n";
        let prog = Mode.prog config.file_prog signatures in
        if Option.is_some prog then
          let prog = Option.get prog in
          let prog : Prog.prog = { prog with lib=libs } in
          Printf.printf "Parsing complete.\n\n%a\n" Prog.pp_prog prog;
          ignore (analyze ~json:true config.file_prog prog signatures locs config)
      | SM_test ->
        Printf.printf "Testing mode.\n";
        List.iter (fun (filename, loc, json) ->
          let locs: Location.t option =
            Some { start=loc ; stop=0 } in
          Printf.printf "File %s\n\n" filename;
          let prog = Mode.prog filename signatures in
          if Option.is_some prog then
            begin
              let prog = Option.get prog in
              let prog : Prog.prog = { prog with lib=libs } in
              let json_analysis = analyze filename prog signatures locs config in
              let json = Test.replace_single_quote json in
              let json_expected = Yojson.Basic.from_string json in
              if json_analysis = json_expected then
                Printf.printf "File %s works as expected.\n" filename
              else
                Printf.printf "File %s is NOT working as expected.\nEXPECTED:\n%s\nACTUAL:\n%s\n"
                  filename json (Yojson.Basic.to_string json_analysis)
            end
          else
            Printf.printf "File %s does not exist!\n" filename) Test.expected_values;
        flush stdout
    end;
    Printf.printf "\nFinished.\n"

let _ = ignore (main ( ))
