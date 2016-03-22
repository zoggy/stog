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

type error =
| Loc of Xtmpl_xml.loc * exn
| Template_file_not_found of string

exception Error of error

let error e = raise (Error e)
let error_loc ?loc e =
  match loc with
    None -> raise e
  | Some loc -> error (Loc (loc, e))

let template_file_not_found ?loc file =
  error_loc ?loc (Error (Template_file_not_found file))

let rec string_of_error ?(to_string=Printexc.to_string) = function
| Template_file_not_found file ->
    Printf.sprintf "Template file not found: %s" file
| Loc (loc, e) ->
    let str =
      match e with
        Error err -> string_of_error ~to_string err
      | Xtmpl_rewrite.Error e -> Xtmpl_rewrite.string_of_error e
      | Xtmpl_xml.Error e -> Xtmpl_xml.string_of_error e
      | _ -> to_string e
    in
    Printf.sprintf "From %s\n%s" (Xtmpl_xml.string_of_loc loc) str

