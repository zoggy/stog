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

open Stog_types;;
open Stog_filter_types;;

module XR = Xtmpl_rewrite

let filter_of_string str =
  let lexbuf = Lexing.from_string str in
  Stog_filter_parser.filter Stog_filter_lexer.main lexbuf
;;

module Set =
  Set.Make
    (struct
       type t = (doc_id * doc)
       let compare (id1,_) (id2, _) = Stog_tmap.compare_key id1 id2
     end)

let filter_pred env att s data (doc_id, doc) =
  let (data, v) =
    let xmls = XR.from_string s in
    let (data, xmls) = XR.apply_to_xmls data env xmls in
    (data, XR.to_string xmls)
  in
  let (data, v_doc) =
    match Stog_types.get_def doc.doc_defs att  with
      None -> (data, "")
    | Some (_, body) ->
        let (data, xmls) = XR.apply_to_xmls data env body in
        (data, XR.to_string xmls)
  in
  (data, v = v_doc)
;;

let set_filter =
  let f pred doc (data, set) =
    let (data,b) = pred data doc in
    let set = if b then Set.add doc set else set in
    (data, set)
  in
  fun data pred set ->
    Set.fold (f pred) set (data, Set.empty)
;;

let rec filter data env set = function
  Pred (att, v) -> set_filter data (filter_pred env att v) set
| Or (f1, f2) ->
    let (data, s1) = filter data env set f1 in
    let (data, s2) = filter data env set f2 in
    (data, Set.union s1 s2)
| And (f1, f2) ->
    let (data, s2) = filter data env set f2 in
    filter data env s2 f1
| Not f ->
    let (data, s) = filter data env set f in
    (data, Set.diff set s)


let filter_docs data env t docs =
  let set = List.fold_right Set.add docs Set.empty in
  let (data, set) = filter data env set t in
  (data, Set.elements set)
;;
  