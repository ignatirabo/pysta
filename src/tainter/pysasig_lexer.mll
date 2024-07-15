{
open Pysasig_parser
(* let h = Hashtbl.create 3 *)
(* let _ =
  List.iter (fun (a, b) -> Hashtbl.add h a b)
    [ "def "       , K_DEF    ;
      "TaintSource", K_TSOURCE;
      "TaintSink"  , K_TSINK  ] *)
(* let fstring s = try Hashtbl.find h s with Not_found -> NAME s *)
let num_line = ref 1
}

let letter = ['a'-'z']|['A'-'Z']|'_'|'.'|['1'-'9']
let word = letter*

let newline = '\n'
let whitespace = ' '
let tab = '\t'

rule token = parse
| "def"         { K_DEF }
| "TaintSource" { K_TSOURCE }
| "TaintSink"   { K_TSINK }
| "@Sanitize"       { K_SANITIZE }
| "Parameters"      { K_PARAMETERS }
| "TaintInTaintOut" {K_TITO }
| "<-"  { LARROW }
| "->"  { RARROW }
| "="   { EQ }
| '('   { LPAR }
| ')'   { RPAR }
| '['   { LBRACK }
| ']'   { RBRACK }
| ':'   { COLON }
| '*'   { STAR }
| ','   { COMMA }
| '.'   { DOT }
| "..." { THREEDOTS }
| '#'   { single_line_comment lexbuf }
| word  { NAME (Lexing.lexeme lexbuf) }
| whitespace | tab { token lexbuf }
| newline          { incr num_line; token lexbuf }

| _  as c  { Printf.printf "character: %c at line %d\n" c !num_line; failwith "unbound char" }
| eof      { EOF }

and single_line_comment = parse
  | '\n' { incr num_line; token lexbuf }
  | eof { EOF }
  | _ { single_line_comment lexbuf }
