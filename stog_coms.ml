(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012 Maxence Guesdon. All rights reserved.                   *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation; either version 2 of the         *)
(*    License.                                                                   *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
(*    GNU Library General Public License for more details.                       *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public                  *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    As a special exception, you have permission to link this program           *)
(*    with the OCaml compiler and distribute executables, as long as you         *)
(*    follow the requirements of the GNU GPL in regard to all of the             *)
(*    software in the executable aside from the OCaml compiler.                  *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

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
  | [< ''\n' >] -> ""
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
  | [< ''\n'; words = parse_words >] ->
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
