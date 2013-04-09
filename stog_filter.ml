(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Maxence Guesdon. All rights reserved.              *)
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

(** Filters. *)

open Stog_types;;
open Stog_filter_types;;

let filter_of_string str =
  let lexbuf = Lexing.from_string str in
  Stog_filter_parser.filter Stog_filter_lexer.main lexbuf
;;

module Set =
  Set.Make
    (struct
       type t = (elt_id * elt)
       let compare (id1,_) (id2, _) = Stog_tmap.compare_key id1 id2
     end)

let filter_pred env att s (elt_id, elt) =
  let v =
    let xml = Xtmpl.xml_of_string s in
    let xmls = Xtmpl.apply_to_xmls env [xml] in
    Xtmpl.string_of_xmls xmls
  in
  let v_elt =
    match Stog_types.get_def elt.elt_defs att  with
      None -> ""
    | Some (_, body) ->
        Xtmpl.string_of_xmls (Xtmpl.apply_to_xmls env body)
  in
  v = v_elt
;;

let rec filter env set = function
  Pred (att, v) -> Set.filter (filter_pred env att v) set
| Or (f1, f2) -> Set.union (filter env set f1) (filter env set f2)
| And (f1, f2) -> filter env (filter env set f2) f1
| Not f -> Set.diff set (filter env set f)


let filter_elts env t elts =
  let set = List.fold_right Set.add elts Set.empty in
  Set.elements (filter env set t)
;;
  