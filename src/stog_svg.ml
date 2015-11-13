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

module XR = Xtmpl_rewrite

let prefix_ids =
  let rec iter p t =
    match t with
  | XR.D _ | XR.C _ | XR.PI _ -> t
  | XR.E node ->
      let atts = node.XR.atts in
      let atts =
       match XR.get_att_cdata atts ("","id") with
         None -> atts
       | Some s ->
            XR.atts_replace ("","id") [ XR.cdata (p^s) ] atts
      in
      let atts =
        match XR.get_att_cdata atts ("http://www.w3.org/1999/xlink","href") with
         None -> atts
       | Some s ->
            let len = String.length s in
            let s = String.sub s 1 (len -1) (* remove beginning '#' *) in
            XR.atts_replace ("http://www.w3.org/1999/xlink","href") [ XR.cdata ("#"^p^s) ] atts
      in
      XR.E { node with XR.atts ; subs = List.map (iter p) node.XR.subs }
  in
  iter
;;

let rec prefix_svg_ids prefix t =
  match t with
  | XR.D _ | XR.C _ | XR.PI _ -> t
  | XR.E { XR.name = (_,"svg")} as t -> prefix_ids prefix t
  | XR.E node ->
      XR.E { node with XR.subs = List.map (prefix_svg_ids prefix) node.XR.subs }
;;

let fun_prefix_svg_ids stog env ?loc atts subs =
  match XR.get_att_cdata atts ("","prefix") with
    None -> (stog, subs)
  | Some prefix -> (stog, List.map (prefix_svg_ids prefix) subs)
;;