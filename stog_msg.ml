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

(** *)

let print_verbose = ref print_endline;;
let verbose_level = ref 0;;

let verbose ?info ?(level=1) msg =
  if level <= !verbose_level then
    begin
      let msg = Printf.sprintf "%s%s"
        (match info with None -> "" | Some s -> Printf.sprintf "[%s]" s)
        msg
      in
      !print_verbose msg
    end
;;
let set_print_verbose = (:=) print_verbose;;
let set_verbose_level = (:=) verbose_level;;
let incr_verbose_level () = incr verbose_level;;
let verbose_level () = !verbose_level ;;

let warnings = ref 0;;
let print_warning = ref prerr_endline;;

module Sset = Set.Make (struct type t = string let compare = String.compare end);;
let seen_warnings = ref Sset.empty;;
let warning ?info msg =
  let msg = Printf.sprintf "Warning: %s%s"
    (match info with None -> "" | Some s -> Printf.sprintf "[%s]" s)
    msg
  in
  if not (Sset.mem msg !seen_warnings) then
    begin
      incr warnings;
      seen_warnings := Sset.add msg !seen_warnings;
      !print_warning msg
    end
;;

let set_print_warning = (:=) print_warning;;
let warnings () = !warnings;;

let errors = ref 0;;
let print_error = ref prerr_endline;;

let error ?info ?fatal msg =
  let msg = Printf.sprintf "Error: %s%s"
    (match info with None -> "" | Some s -> Printf.sprintf "[%s]" s)
    msg
  in
  incr errors;
  !print_error msg;
  match fatal with  None -> () | Some n -> exit n
;;

let set_print_error = (:=) print_error;;
let errors () = !errors;;

