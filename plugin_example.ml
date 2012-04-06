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

(** Stog plugin example. *)

let fun_list env args subs =
  (* get the optional sep attribute ... *)
  let sep = Xtmpl.opt_arg args "sep" in
  (* and parse it as xml *)
  let xml = Xtmpl.xml_of_string sep in
  (* We can access the stog structure with !Stog_html.current_stog .
     We don't use it here.
  *)

  (* then insert the separator between all children of the node *)
  let rec iter acc = function
    [] -> List.rev acc
  | h :: q ->
      let acc =
        match acc with
          [] -> [h]
        | _ -> h :: xml :: acc
      in
      iter acc q
  in
  (* and finally return the list of xml trees *)
  iter [] subs
;;

(* register the new function, associated to tag "list" *)
Stog_html.plugin_funs := ("list", fun_list) :: !Stog_html.plugin_funs;;
