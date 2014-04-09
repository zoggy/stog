(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Maxence Guesdon. All rights reserved.              *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
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

type input =
  { in_phrase : string ;
  }

type output =
  { stdout : string ;
    stderr : string ;
    topout : string ;
  }

type result =
  | Exc of string
  | Ok of output
  | Handled_error of output

let pid = Unix.getpid();;

let read_input ic =
  let (v : input) = input_value ic in
  v
;;

let write_input oc (i : input) =
(*  prerr_endline (Printf.sprintf "write_input len(phrase)=%d" (String.length i.in_phrase));*)
  output_value oc i;
  flush oc
;;

let read_result ic =
  let (r : result) = input_value ic in
  r
;;


let write_result oc (r : result) =
(*  let s =
    match r with
      Exc s -> Printf.sprintf "Exc(%S)" s
    | Ok (s1, s2) -> Printf.sprintf "Ok(%S,%S)" s1 s2
  in
  prerr_endline (Printf.sprintf "write_result %s" s);
*)
  output_value oc r ;
  flush oc
;;

