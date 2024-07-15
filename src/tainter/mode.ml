module SSet = String_helper.SSet
module SMap = String_helper.SMap
module S = Signature
(* type apron =
  | A_box
  | A_polka *)

type analysis =
  | S_pytaint

type mode =
  | M_interpret
  | M_analysis

type smode =
  | SM_test
  | SM_file

let smode_not_test = function
  | SM_test -> false
  | _ -> true

let str_to_int_list (s: string) : int list =
  let l : string list = String.split_on_char ',' s in
  List.map int_of_string l

(* let str_to_apron (s: string) : apron =
  match s with
  | "intvs" -> A_box
  | "polyhedra" -> A_polka
  | _ -> failwith "Unkown Apron domain" *)
(* let apron_to_str = function
  | A_box -> "Intvs"
  | A_polka -> "Polyhedra" *)

let str_to_analysis (s: string) : analysis =
  match s with
  | "pytaint" -> S_pytaint
  | _ -> failwith "Unkown analysis"
let non_rel_dom_to_str = function
  | S_pytaint -> "Python tainting" 

let mode_to_str = function
  | M_interpret -> "Interpret"
  | M_analysis -> "Analysis"

(* let str_to_rel (s: string) : rel_dom =
  match s with
  | "rse" -> D_rse
  | "dep" -> D_dep
  | _ -> failwith "Unkown relational domain" *)

type t =  { smode:smode ; mode:mode ; analysis:analysis; db:string; file_taint:string;
            file_prog:string ; file_sig:string ; dir_lib:string ; locs:string }

let parse () : t =
  Printf.printf "Parsing input.\n";
  let file_prog = ref ""
  and file_sig = ref "../pysa_playground/stubs/taint/general.pysa"
  and file_taint = ref "../pysa_playground/stubs/taint/taint.config"
  and dir_lib = ref "library"
  and db = ref "sapp.db"
  and locs = ref ""
  and mode: mode ref = ref M_analysis
  and smode: smode ref = ref SM_file
  and analysis: analysis ref  = ref S_pytaint in
  let aux_analysis an =
    (match !analysis, an with
    | S_pytaint, an -> analysis := an);
    match !mode with
    | M_interpret -> mode := M_analysis
    | _ -> () in
  let set_mode m = Arg.Unit (fun () -> mode := m) in
  let set_smode m = Arg.Unit (fun () -> smode := m) in
  let set_signature = Arg.String (fun s -> file_sig := s) in
  let set_library = Arg.String (fun s -> dir_lib := s) in
  let set_db = Arg.String (fun s -> db := s) in
  let set_locations = Arg.String (fun s -> locs := s) in
  let set_analysis an =
    Arg.Unit (fun () -> aux_analysis an) in
  let set_debug = Arg.Set Utils.debug in
  let set_debug_counter = Arg.Set Counter.counter_debug in
  Arg.parse
    [
      "--pytaint", set_analysis S_pytaint, "set python taint analysis";
      "--debug", set_debug, "set debug prints";
      "--debug-counter", set_debug_counter, "set debug prints for counter";
      "--interpret", set_mode M_interpret, "interpret";
      "--sig", set_signature, "set pysa signature";
      "--lib", set_library, "set library";
      "--test", set_smode SM_test, "test";
      "--locs", set_locations, "set start and end locations";
      "--db", set_db, "set SAPP database"
    ] (fun s -> file_prog := s) "file";
  if String.length !file_prog = 0 && smode_not_test !smode then
    failwith "Filename is empty."
  else
    let smode, mode, analysis = !smode, !mode, !analysis in
    let file_prog, file_sig, file_taint , dir_lib, locs, db = !file_prog, !file_sig, !file_taint, !dir_lib, !locs, !db in
    { smode ;  mode ; analysis ; file_prog ; file_sig ; file_taint; dir_lib ; locs ; db }

let prog filename signatures : Prog.prog option =
  let open Pyreir in
  let open PyreAst.Parser in
  try
    let content = Core.In_channel.read_all filename in
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
    let prog = Ir_transfer.from_prog prog signatures in
    let prog = Prog.binding prog in
    Some prog
  with Sys_error _ ->
    Printf.printf "No file %s found.\n" filename;
    None

let builtin = ["input";"print";"eval"]

let library_file filename signatures : Function.t =
  let open Pyreir in
  let open PyreAst.Parser in
  if !Utils.debug then Printf.printf "library: %s\n" filename;
  let file_module = List.nth (String.split_on_char '/' filename) 1 |> String.split_on_char '.' in
  let file_module =
    if List.exists ((=) (List.hd file_module)) builtin then (* List.hd file_module = "input" || List.hd file_module = "print" then *)
      [  ]
    else
      [ List.filter (fun s -> s <> "py") file_module |> String.concat "." ] in
  (* Printf.printf "%s\n" file_module; *)
  let content = Core.In_channel.read_all filename in
  let ast =
    with_context (fun context ->
    match Concrete.parse_module ~context content with
    | Result.Error { Error.message; line; column; _ } ->
      let message =
        Format.sprintf "Parsing error at line %d, column %d: %s"
        line column message in
      failwith message
    | Result.Ok ast -> ast) in
  let libs : Ir_sig.proc list = Ir_cast.modl_to_library ast in
  (* List.iter (fun p -> Printf.printf "id = %s\n" p.Ir_sig.id) libs; *)
  let libs = List.map (fun proc ->
    let id = String.concat "." (file_module @ [ proc.Ir_sig.id ]) in
    { proc with id}) libs in
  let libs : Function.t = List.fold_left
    (fun libs lib ->
      let func : Function.func = (Ir_transfer.from_proc ~sign:signatures lib) in
      (* let pname = String.concat "." (file_module@[proc.pname]) in
      let proc = { proc with pname } in *)
      SMap.add (S.get_name func.sign) func libs) SMap.empty libs in
  libs

let library dir_lib signatures : Function.t =
  Printf.printf "Reading library: \"%s\"\n" dir_lib;
  (* Get all files in dir_lib, for now manual *)
  let filenames = Sys.readdir dir_lib
    |> Array.to_list
    |> List.filter (fun x -> Filename.extension x = ".py")
    |> List.map (fun filename -> dir_lib ^ "/" ^ filename) in
  (* let filenames = [ dir_lib ^ "/spawn.py" ; dir_lib ^ "/input.py" ] in *)
  let lib = List.fold_left (fun libs filename -> SMap.union (fun name func0 _ -> Some func0) (library_file filename signatures) libs) SMap.empty filenames in
  Printf.printf "Library finished, %d functions found.\n" (SMap.cardinal lib);
  lib

let signatures filename : Signature.t list =
  Printf.printf "Signatures filename: %s\n" filename;
  let file = Unix.openfile filename [ Unix.O_RDONLY ] 0o644 in
  let channel = Unix.in_channel_of_descr file in
  let lexbuf = Lexing.from_channel channel in
  let signatures =
    try Pysasig_parser.signatures Pysasig_lexer.token lexbuf
    with Parsing.Parse_error ->
      failwith (Printf.sprintf "Parsing failed at line %d."
                  !Pysasig_lexer.num_line) in
  Printf.printf "Parsing signatures done: %d signatures.\n" (List.length signatures);
  if !Utils.debug then
    List.iter (fun sign -> Signature.pp_t stdout sign) signatures;
  signatures

let locations (locs: string) : Location.t =
  let locs = String.split_on_char ',' locs in
  match locs with
  | [l] -> { start=(int_of_string l) ; stop=(int_of_string l) }
  | [l1;l2] -> { start=(int_of_string l1) ; stop=(int_of_string l2) }
  | _ -> failwith "Wrong amount of arguments to create loc."
