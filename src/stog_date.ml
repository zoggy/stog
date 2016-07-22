(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2015 INRIA All rights reserved.                         *)
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

(** *)

exception Invalid_date of string * string

let invalid_date str err = raise (Invalid_date (str, err))

type t =
  { stamp : Ptime.t ;
    tz: Ptime.tz_offset_s option ;
  }

let of_string str =
  match Ptime.of_rfc3339 str with
     Ok (stamp, tz, _) -> { stamp ; tz }
   | Error (`RFC3339 ((p1,p2), e)) ->
      let b = Buffer.create 256 in
      let fmt = Format.formatter_of_buffer b in
      if p2 > p1 then
        Format.fprintf fmt "Characters %d-%d: " p1 p2
      else
        Format.fprintf fmt "Character %d: " p1;
      Ptime.pp_rfc3339_error fmt e;
      Format.pp_print_flush fmt () ;
      let err = Buffer.contents b in
      invalid_date str err

let of_string_date str = of_string (str^"T00:00:00Z")

let to_string t = Ptime.to_rfc3339 ?tz_offset_s: t.tz t.stamp

let now () =
  match Ptime.of_float_s (Unix.time()) with
  | Some stamp -> { stamp ; tz = None }
  | None -> failwith "Could not create date from Unix.time"

let to_date_time t =
  Ptime.to_date_time ?tz_offset_s: t.tz t.stamp

let weekday t = Ptime.weekday t.stamp
  (* FIXME: give tz_offset_s parameter when weekday accepts it *)

let cp_percent = Char.code '%'
let format t fmt =
  match fmt with
    "rfc3339" -> to_string t
  | _ ->
      let b = Buffer.create 256 in
      let ((y,m,d),((h,mi,s),tz)) = to_date_time t in
      let f prev_cp i = function
        `Malformed str ->
          if prev_cp >= 0 then Uutf.Buffer.add_utf_8 b prev_cp ;
          Buffer.add_string b str ;
          -1
      | `Uchar cp ->
          if prev_cp = cp_percent then
            let () =
              match cp with
              | 131 (* Y *) -> Buffer.add_string b (string_of_int y)
              | 115 (* M *) -> Buffer.add_string b (string_of_int m)
              | 104 (* D *) -> Buffer.add_string b (string_of_int d)
              | 150 (* h *) -> Buffer.add_string b (string_of_int h)
              | 155 (* m *) -> Buffer.add_string b (string_of_int m)
              | 163 (* s *) -> Buffer.add_string b (string_of_int s)
              | n when n = cp_percent -> Buffer.add_char b '%'
              | _ ->
                  Uutf.Buffer.add_utf_8 b prev_cp ;
                  Uutf.Buffer.add_utf_8 b cp
            in
            -1
          else
            (
             if prev_cp >= 0 then Uutf.Buffer.add_utf_8 b prev_cp ;
             cp
            )
      in
      let remain = Uutf.String.fold_utf_8 f (-1) fmt in
      if remain >= 0 then Uutf.Buffer.add_utf_8 b remain ;
      Buffer.contents b

