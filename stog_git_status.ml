(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 INRIA All rights reserved.                         *)
(*    Author: Maxence Guesdon, INRIA Saclay                                      *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU General Public License for more details.                               *)
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

(** Parsing [git status --procelain -z] output.

  See {{:http://git-scm.com/docs/git-status}Git status documentation} . *)

open Stog_git_types

let char_of_status : Stog_git_types.status -> char = function
| `B -> ' '
| `M -> 'M'
| `A -> 'A'
| `D -> 'D'
| `R -> 'R'
| `C -> 'C'
| `U -> 'U'
| `Q -> '?'
| `I -> '!'

let status_of_char : char -> Stog_git_types.status = function
  ' ' -> `B
| 'M' -> `M
| 'A' -> `A
| 'D' -> `D
| 'R' -> `R
| 'C' -> `C
| 'U' -> `U
| '?' -> `Q
| '!' -> `I
| c -> failwith (Printf.sprintf "Invalid status character %c" c)

let parse_line str =
  try
    assert (String.get str 2 = ' ');
    let p1 = String.index_from str 2 '\000' in
    let file1 = Ojs_path.of_string (String.sub str 3 (p1 - 3)) in
    let file2 =
      let p = p1 + 1 in
      match String.index_from str p '\000' with
      | exception Not_found -> None
      | p2 ->
          let f = String.sub str p (p2 - p) in
          Some (Ojs_path.of_string f)
    in
    (status_of_char (String.get str 0),
     status_of_char (String.get str 1),
     file1,
     file2)
  with
    _ -> failwith ("Invalid line:\n"^str)

let parse str =
  let lines = Stog_misc.split_string str ['\n' ; '\r'] in
  List.map parse_line lines

let is_unmerged (s1, s2, _, _) =
  match s1, s2 with
  | `U, _
  | _, `U
  | `A, `A
  | `D, `D -> true
  | _ -> false

