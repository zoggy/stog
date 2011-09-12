(** *)

type command = string array


let string_of_char c = String.make 1 c
let concat char string =
  string_of_char char ^ string

let rec parse_squote = parser
    | [< ''\'' >] -> ""
    | [< ''\\'; 'c; word = parse_squote >] ->
        concat c word
    | [< 'c; word = parse_squote >] ->
        concat c word
    | [< >] -> failwith "squote"

let rec parse_dquote = parser
    | [< ''"' >] -> ""
    | [< ''\\'; 'c; word = parse_dquote >] ->
        concat c word
    | [< 'c; word = parse_dquote >] ->
        concat c word
    | [< >] -> failwith "dquote"

let rec parse_noquote = parser
  | [< '' ' >] -> ""
  | [< ''\\'; 'c; word = parse_noquote >] ->
      concat c word
  | [< ''"'; subword = parse_dquote; word = parse_noquote >] ->
      subword ^ word
  | [< ''\''; subword = parse_squote; word = parse_noquote >] ->
      subword ^ word
  | [< 'c; word = parse_noquote >] ->
      concat c word
  | [< >] -> ""


let rec parse_words = parser
    [< '' '; words = parse_words >] ->
      words
  | [< ''"'; word = parse_dquote; words = parse_words >] ->
      word :: words
  | [< ''\''; word = parse_squote; words = parse_words >] ->
      word :: words
  | [< ''\\'; 'c; word = parse_noquote; words = parse_words >] ->
      concat c word :: words
  | [< 'c; word = parse_noquote; words = parse_words >] ->
      concat c word :: words
    | [< >] -> []


let list_of_string s =
  parse_words (Stream.of_string s)

let command_of_string s = Array.of_list (list_of_string s)
let string_of_command a =
  String.concat " " (Array.to_list (Array.map Filename.quote a))
