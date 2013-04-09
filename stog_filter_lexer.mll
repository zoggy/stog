{
(** Filter lexer. *)

open Stog_filter_parser;;
module L = Lexing;;

let error lexbuf msg =
  let pos = lexbuf.Lexing.lex_curr_p in
  let msg = Printf.sprintf "Line %d, character %d: %s"
    pos.Lexing.pos_lnum (pos.Lexing.pos_cnum - pos.Lexing.pos_bol) msg
  in
  failwith msg
;;

let string_buffer = Buffer.create 256 ;;
}


let lowercase = ['a'-'z']
let uppercase = ['A'-'Z']
let digit = ['0'-'9']
let attchar = lowercase | uppercase | digit | '_' | '-'

let attribute = (lowercase|uppercase) attchar*

rule main = parse
| '"' { Buffer.reset string_buffer; string lexbuf }
| '\n'
    {
      let module L = Lexing in
      let pos = lexbuf.L.lex_curr_p in
      lexbuf.L.lex_curr_p <-
        { pos with
          L.pos_lnum = pos.L.pos_lnum + 1 ;
          pos_bol = pos.L.pos_cnum ;
        };
      main lexbuf
    }
| ' ' { main lexbuf }
| attribute {
    let lexeme = Lexing.lexeme lexbuf in
    Attribute lexeme
  }
| '(' { LPAR }
| ')' { RPAR }
| '=' { EQUAL }
| '|' { OR }
| '&' { AND }
| '!' { NOT }
| eof { EOF }
| _ { error lexbuf (Printf.sprintf "Invalid character %s" (Lexing.lexeme lexbuf)) }

and string = parse
 "\\\""  { Buffer.add_char string_buffer '"'; string lexbuf }
| "\\\\" { Buffer.add_char string_buffer '\\'; string lexbuf }
| '"'  { String (Buffer.contents string_buffer) }
| '\n' {
      let module L = Lexing in
      let pos = lexbuf.L.lex_curr_p in
      lexbuf.L.lex_curr_p <-
        { pos with
          L.pos_lnum = pos.L.pos_lnum + 1 ;
          pos_bol = pos.L.pos_cnum ;
        };
      Buffer.add_string string_buffer (Lexing.lexeme lexbuf);
      string lexbuf
    }
| _ { Buffer.add_string string_buffer (Lexing.lexeme lexbuf); string lexbuf }
| eof { error lexbuf "String not terminated." }


