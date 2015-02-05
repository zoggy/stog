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

let is_status_char c = try ignore(status_of_char c); true with _ -> false

let rec parse_line str acc p =
  let next =
    try
      let len = String.length str in
      if len > p + 2 then
        begin
          assert (String.get str (p+2) = ' ');
          let p1 = String.index_from str (p+2) '\000' in
          (*prerr_endline (Printf.sprintf "p=%d (%c), p1=%d (%c)" p str.[p] p1 str.[p1]);*)
          let file1 = Ojs_path.of_string (String.sub str (p+3) (p1 - (p + 3))) in
          let (file2, p_next) =
            let p = p1 + 1 in
            let p_space =
              try
                (* look from space from p+2 to avoid space status char which can
                   be after the \000 char at p-1 *)
                let q =
                  if len > p+2
                  then String.index_from str (p+2) ' '
                  else raise Not_found
                in
                (*prerr_endline (Printf.sprintf "q=%d" q);*)
                if is_status_char (String.get str (q-2)) &&
                  is_status_char (String.get str (q-1))
                then
                  q - 2
                else
                  len
              with
                Not_found -> len
            in
            let p_zero =
              try
                if len > p
                then String.index_from str p '\000'
                else raise Not_found
              with Not_found -> len
            in
            (*prerr_endline (Printf.sprintf "p_space = %d, p_zero = %d" p_space p_zero);*)
            if p_space < p_zero then (* no second filename *)
              (None, p)
            else
              begin
                let f = String.sub str p (p_zero - p) in
                let f = if f = "" then None else Some (Ojs_path.of_string f) in
                (f, p_zero + 1)
              end
          in
          let st =
            (status_of_char (String.get str p),
             status_of_char (String.get str (p+1)),
             file1,
             file2)
          in
          Some (st :: acc, p_next)
        end
      else
        None
    with
    | _ -> failwith ("Invalid line:\n"^str)
  in
  match next with
    None -> List.rev acc
  | Some (acc, p) -> parse_line str acc p

let parse str = parse_line str [] 0

let is_unmerged (s1, s2, _, _) =
  match s1, s2 with
  | `U, _
  | _, `U
  | `A, `A
  | `D, `D -> true
  | _ -> false

