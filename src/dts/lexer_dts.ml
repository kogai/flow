open Sedlexing
open Parser_dts
open Token_dts

let rec token (env: Lex_env.t) lexbuf =
  env, T_NULL
and type_token (env: Lex_env.t) lexbuf =
  env, T_NULL

(* 
let letter = [%sedlex.regexp? 'a'..'z' | 'A'..'Z' | '_' | '$']
let digit = [%sedlex.regexp? '0'..'9']
let int_t = [%sedlex.regexp? Opt '-', digit, Star digit]
let alphanumeric = [%sedlex.regexp? digit | letter]
let word = [%sedlex.regexp? letter, Star alphanumeric]

let floatnumber = [%sedlex.regexp? Plus digit, '.', Plus digit]
let wholenumber = [%sedlex.regexp? Opt '-', digit | floatnumber]
let scientificnumber = [%sedlex.regexp? wholenumber,'e'|'E', Opt '-'|'+', Plus digit]

let new_line = [%sedlex.regexp? '\r'|'\n'|"\r\n"]

let rec lex synthetic_lexbuf =
  let lexbuf = synthetic_lexbuf.lexbuf_inner in
  match%sedlex lexbuf with
  | white_space ->
    ignore @@ update synthetic_lexbuf; 
    lex synthetic_lexbuf
  | new_line ->
    ignore @@ next_line synthetic_lexbuf; 
    lex synthetic_lexbuf
  | int_t -> INT (lexbuf |> Utf8.lexeme |> int_of_string)
  | scientificnumber -> FLOAT (lexbuf |> Utf8.lexeme |> float_of_string)
  | "true" -> TRUE 
  | "false" -> FALSE 
  | "null" -> NULL 
  | '"' -> read_string (Buffer.create 127) synthetic_lexbuf '"'
  | '\'' -> read_string (Buffer.create 127) synthetic_lexbuf '\''
  | "{" -> LBRACE 
  | "}" -> RBRACE 
  | "[" -> LBRACKET 
  | "]" -> RBRACKET 
  | ":" -> COLON 
  | "," -> COMMA 
  | eof -> EOF
  | _ -> raise (SyntaxError ("Unexpected character: " ^ Utf8.lexeme lexbuf))

and read_string buf synthetic_lexbuf q =
  let lexbuf = synthetic_lexbuf.lexbuf_inner in
  let quote = [%sedlex.regexp? '"' | "'"] in 
  let string_continue = [%sedlex.regexp? Compl quote] in 

  match%sedlex lexbuf with
  | quote ->
    STRING (Buffer.contents buf) 
  | string_continue ->
    Buffer.add_string buf (Utf8.lexeme lexbuf);
    read_string buf synthetic_lexbuf q 
  | eof -> raise (SyntaxError ("String is not terminated"))
  | _ -> raise (SyntaxError ("Illegal string charcter: [" ^ Utf8.lexeme lexbuf ^ "]"))

let init_synthetic_lexbuf ?(file = "") lexbuf_inner =
  let position = {Lexing.
                   pos_fname = file;
                   pos_lnum = 1;
                   pos_bol = 0;
                   pos_cnum = 0;
                 } in
  { position; lexbuf_inner }

let read synthetic_lexbuf = 
  let lex' () =
    let before = synthetic_lexbuf.position in
    let token = lex synthetic_lexbuf in
    let after = synthetic_lexbuf.position in
    (token, before, after) in

  let parser' = MenhirLib.Convert.Simplified.traditional2revised Parser.prog in 
  try
    parser' lex'
  with
  | Parser.Error
    -> raise @@ ParseError "Paser error"
  | Sedlexing.MalFormed
  | Sedlexing.InvalidCodepoint _
    -> raise @@ ParseError "Some reason" *)
