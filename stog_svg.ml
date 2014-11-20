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

let prefix_ids =
  let rec iter p = function
    (Xtmpl.D _) as t -> t
  | Xtmpl.E (tag, atts, subs) ->
      let atts =
       match Xtmpl.get_arg_cdata atts ("","id") with
         None -> atts
       | Some s ->
            Xtmpl.atts_replace ("","id") [ Xtmpl.D (p^s) ] atts
      in
      let atts =
        match Xtmpl.get_arg_cdata atts ("http://www.w3.org/1999/xlink","href") with
         None -> atts
       | Some s ->
            let len = String.length s in
            let s = String.sub s 1 (len -1) (* remove beginning '#' *) in
            Xtmpl.atts_replace ("http://www.w3.org/1999/xlink","href") [ Xtmpl.D ("#"^p^s) ] atts
      in
      Xtmpl.E (tag, atts, List.map (iter p) subs)

  in
  iter
;;

let rec prefix_svg_ids prefix = function
  (Xtmpl.D _) as t -> t
| Xtmpl.E ((_,"svg"), _, _) as t -> prefix_ids prefix t
| Xtmpl.E (t,atts,subs) ->
    Xtmpl.E (t, atts, List.map (prefix_svg_ids prefix) subs)
;;

let fun_prefix_svg_ids stog env atts subs =
  match Xtmpl.get_arg_cdata atts ("","prefix") with
    None -> (stog, subs)
  | Some prefix -> (stog, List.map (prefix_svg_ids prefix) subs)
;;