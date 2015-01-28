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

module S = Cohttp_lwt_unix.Server
open Xtmpl
let noatts = Xtmpl.atts_empty

let path_sessions = ["sessions"]

let page ~title contents =
  E (("","html"), noatts, [
     E(("","header"), noatts, [
        E(("","title"), noatts, [D title])
      ]) ;
     E(("","body"), noatts, [
       E(("","div"), atts_of_list [("","id"), [D "page"]],
         contents
        )
     ])
   ]
  )

module Form_login = [%ojs.form "templates/form_login.tmpl"]

let param_of_body body =
  let params = Uri.query_of_encoded body in
  fun s ->
    match List.assoc s params with
    | exception Not_found -> None
    | [] | "" :: _ -> None
    | s :: _ -> Some s



