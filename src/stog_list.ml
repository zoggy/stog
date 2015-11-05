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
module XR = Xtmpl_rewrite

let docs ?set ?setname ?filter ?typ ?max ?(reverse=true) ?(sort=[]) stog env =
  let docs =
    match set with
      Some set ->
        let l = Stog_types.Doc_set.elements set in
        List.map (fun id -> (id, Stog_types.doc stog id)) l
    | None ->
        Stog_types.doc_list ?set: setname stog
  in
  let (stog, docs) =
    match filter with
      None -> (stog, docs)
    | Some filter -> Stog_filter.filter_docs stog env filter docs
  in
  let docs =
    match typ with
    | None | Some [] -> docs
    | Some types ->
        List.filter (fun (_,doc) -> List.mem doc.doc_type types) docs
  in
  let (stog, docs) =
    match sort with
      [] -> (stog, Stog_types.sort_ids_docs_by_date docs)
    | fields ->
        let docs = List.map
          (fun (id, e) -> (id, e, Stog_engine.env_of_defs ~env e.doc_defs))
            docs
        in
        Stog_types.sort_ids_docs_by_rules stog fields docs
  in
  let docs = if reverse then List.rev docs else docs in
  let docs =
    match max with
      None -> docs
    | Some n -> Stog_misc.list_chop n docs
  in
  (*prerr_endline (Printf.sprintf "Stog_list.docs: %d docs returned" (List.length docs));*)
  (stog, docs)
;;

let docs_of_args ?set stog env args =
  let setname = XR.get_att_cdata args ("", "set") in
  let filter =
    Stog_misc.map_opt
      Stog_filter.filter_of_string
      (XR.get_att_cdata args ("", "filter"))
  in
  let typ =
    match XR.get_att_cdata args ("", "type") with
      None | Some "" -> None
    | Some s ->
        Some (Stog_misc.split_string s [',' ; ';'])
  in
  let max = Stog_misc.map_opt int_of_string
    (XR.get_att_cdata args ("", "max"))
  in
  let reverse =
    match XR.get_att_cdata args ("", "reverse") with
      None -> true
    | Some s -> Stog_io.bool_of_string s
  in
  let sort =
    match XR.get_att_cdata args ("", "sort") with
      None -> None
    | Some s -> Some (Stog_misc.split_string s [','])
  in
  docs ?set ?setname ?filter ?typ ?max ~reverse ?sort stog env
;;

        