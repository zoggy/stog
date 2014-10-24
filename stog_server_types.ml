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

module J = Yojson.Safe

type page_update =
  | Patch of Xmldiff.patch
  | Update_all of Xtmpl.tree

type server_message =
  | Update of string * page_update (* path * operation *)
  | Errors of string list * string list (* errors * warnings *)

type client_msg = [
    `Stog_msg of
      [
      | `Get of string (* path *)
      | `Refresh
      ]
  ] [@@deriving Yojson]

let wsdata_of_client_msg msg = J.to_string (client_msg_to_yojson msg)
let client_msg_of_wsdata s =
  try
    let json = J.from_string s in
    match client_msg_of_yojson json with
      `Error s -> raise (Yojson.Json_error s)
    | `Ok msg -> Some msg
  with
    Yojson.Json_error s ->
      prerr_endline s;
      None
  | e ->
      prerr_endline (Printexc.to_string e);
      None

let to_hex s =
  let len = String.length s in
  let result = Bytes.create (2 * len) in
  for i = 0 to len - 1 do
    Bytes.blit_string
      (Printf.sprintf "%02x" (int_of_char (String.get s i)))
      0 result (2*i) 2;
  done;
  Bytes.to_string result
;;

let from_hex =
  let digit c =
    match c with
    | '0'..'9' -> Char.code c - Char.code '0'
    | 'A'..'F' -> Char.code c - Char.code 'A' + 10
    | 'a'..'f' -> Char.code c - Char.code 'a' + 10
    | _ -> raise (Invalid_argument "Digest.from_hex")
  in
  fun s ->
    let byte i = digit (String.get s i) lsl 4 + digit (String.get s (i+1)) in
    let len = String.length s in
    let result = Bytes.create (len/2) in
    for i = 0 to (len / 2) - 1 do
      Bytes.set result i (Char.chr (byte (2 * i)));
    done;
    Bytes.to_string result
;;