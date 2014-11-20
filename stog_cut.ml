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

open Stog_types

module Sset = Stog_types.Str_set;;
module Nmap = Xtmpl.Name_map;;

module PMap = Stog_path.Map

type cutpoint =
  {
    cut_tag : string * string ;
    cut_doc_type : string ;
    cut_path_sep : string ;
    cut_insert_link : bool ;
    cut_use_parent_path : bool ;
  }
;;

(* since we iter in documents with List.fold_right to keep documents in
  order, we encounter next document of a cutpoint A before cutpoint A. *)
type links = {
  by_doc : (Stog_path.path option * Stog_path.path option) Stog_path.Map.t ;
  next_by_cp : Stog_path.path Nmap.t ;
}

let cutpoint_of_atts doc atts =
  let typ = Xtmpl.opt_arg_cdata atts ~def: doc.doc_type ("","type") in
  let tag =
    match Xtmpl.get_arg_cdata atts ("","tag") with
      None -> failwith "Missing 'tag' attribute for <cut-doc> node"
    | Some s ->
        match Stog_misc.split_string s [':'] with
          [] | [_] -> ("", s)
        | h :: q -> (h, String.concat ":" q)
  in
  let sep = Xtmpl.opt_arg_cdata atts ~def: "-" ("", Stog_tags.path_sep) in
  let insert_link = not (Xtmpl.opt_arg_cdata atts ~def: "true" ("","insert-link") = "false") in
  let use_parent_path = not (Xtmpl.opt_arg_cdata atts ~def: "true" ("","use-parent-path") = "false") in
  { cut_tag = tag ; cut_doc_type = typ ;
    cut_path_sep = sep ; cut_insert_link = insert_link ;
    cut_use_parent_path = use_parent_path ;
  }
;;

let new_doc_in_cutpoint cut_tag links doc =
  let next_path =
    try Some (Nmap.find cut_tag links.next_by_cp)
    with Not_found -> None
  in
  let links =
    match next_path with
      None -> links
    | Some next_path ->
        let next =
          try snd (PMap.find next_path links.by_doc)
          with Not_found -> None
        in
        let by_doc = PMap.add next_path
          (Some doc.doc_path, next) links.by_doc
        in
        let prev =
          try fst (PMap.find doc.doc_path links.by_doc)
          with Not_found -> None
        in
        let by_doc = PMap.add doc.doc_path
          (prev, Some next_path) by_doc
        in
        { links with by_doc }
  in
  let next_by_cp = Nmap.add cut_tag doc.doc_path links.next_by_cp in
  { links with next_by_cp }
;;

let add_doc links stog doc =
  let (prev, next) =
    try PMap.find doc.doc_path links.by_doc
    with Not_found -> None, None
  in
  let doc =
    match prev with
      None -> doc
    | Some prev_path ->
        let def = (("", Stog_tags.previous_path), Xtmpl.atts_empty,
           [Xtmpl.D (Stog_path.to_string prev_path)])
        in
        { doc with doc_defs = def :: doc.doc_defs }
    in
  let doc =
    match next with
      None -> doc
    | Some next_path ->
        let def = (("", Stog_tags.next_path), Xtmpl.atts_empty,
           [Xtmpl.D (Stog_path.to_string next_path)])
        in
        { doc with doc_defs = def :: doc.doc_defs }
  in
  Stog_types.add_doc stog doc
;;

let mk_path use_parent_path path sep id =
  let path_s = Stog_path.to_string path in
  match Stog_misc.filename_extension path_s with
    "" ->
      let msg =  "To be cut, " ^path_s ^ " should have an extension (e.g. \".html\")" in
      failwith msg
  | ext ->
      let p =
        if use_parent_path then
          (Filename.chop_extension path_s) ^ sep ^ id
        else
          "/" ^ id
      in
      let p = p ^ "." ^ ext in
      Stog_path.of_string p
;;

let cut_docs =
  let id_set =
    let rec iter set = function
      Xtmpl.D _ -> set
    | Xtmpl.E (tag, atts, subs) ->
        let set =
          match Xtmpl.get_arg_cdata atts ("", "id") with
            None
          | Some "" -> set
          | Some id -> Sset.add id set
        in
        List.fold_left iter set subs
    in
    fun doc ->
      let xmls =
        match doc.doc_out with
          None -> doc.doc_body
        | Some xmls -> xmls
      in
      List.fold_left iter Sset.empty xmls
  in
  let string_of_tag = function
    ("",t) -> "<"^t^">"
  | (n, t) -> "<"^n^":"^t^">"
  in
  let set_id_map stog path atts new_path with_id =
    if path <> new_path then
      match Xtmpl.get_arg_cdata atts ("","id") with
        None -> stog
      | Some id ->
          let new_id = if with_id then Some id else None in
          Stog_types.id_map_add stog path id new_path new_id
    else
      stog
  in
  let rec iter doc new_path cutpoints links stog new_docs xml =
    match xml with
      Xtmpl.D _ -> (stog, [xml], new_docs, links)
    | Xtmpl.E (("","cut-doc"), atts, xmls) ->
        let cutpoints = (cutpoint_of_atts doc atts) :: cutpoints in
        let (stog, xmls, new_docs, links2) = List.fold_right
          (fold doc new_path cutpoints) xmls (stog, [], new_docs, links)
        in
        let links = { links2 with next_by_cp = links.next_by_cp } in
        (stog, xmls, new_docs, links)

    | Xtmpl.E (tag, atts, xmls) ->
        let cp_opt =
          try Some (List.find (fun cp -> cp.cut_tag = tag) cutpoints)
          with Not_found -> None
        in
        match cp_opt with
          None ->
            (* not a cut point *)
            let (stog, xmls, new_docs, links) = List.fold_right
              (fold doc new_path cutpoints) xmls (stog, [], new_docs, links)
            in
            (stog, [Xtmpl.E (tag, atts, xmls)], new_docs, links)
        | Some cp ->
            try
              let title =
                match Xtmpl.get_arg_cdata atts ("","title") with
                  None ->
                    Stog_msg.warning ("Missing title on cutpoint; not cutting node "^(string_of_tag tag));
                    raise Not_found
                | Some s -> s
              in
              let id =
                match Xtmpl.get_arg_cdata atts ("","id") with
                  None ->
                    Stog_msg.warning ("Missing id on cutpoint; not cutting node "^(string_of_tag tag));
                    raise Not_found
                | Some s -> s
              in
              let new_path = mk_path cp.cut_use_parent_path new_path cp.cut_path_sep id in
              let stog = set_id_map stog doc.doc_path atts new_path false in
              let (stog, xmls, new_docs, links2) =
                List.fold_right (fold doc new_path cutpoints)
                  xmls (stog, [], new_docs, links)
              in
              let links = { links2 with next_by_cp = links.next_by_cp } in
              let doc =
                { doc with
                  doc_path = new_path ;
                  doc_parent = Some doc.doc_path ;
                  doc_type = cp.cut_doc_type ;
                  doc_title = title ;
                  doc_body = xmls ;
                  doc_out = None ;
                }
              in
              let links = new_doc_in_cutpoint cp.cut_tag links doc in
              let xml =
                if cp.cut_insert_link then
                  [ Xtmpl.E (("","div"),
                     Xtmpl.atts_one ("","class") [Xtmpl.D ("cutlink "^(snd tag))],
                     [Xtmpl.E (("","doc"),
                        Xtmpl.atts_one ("","href")
                          [Xtmpl.D (Stog_path.to_string new_path)],
                        [])]
                    )
                  ]
                else
                  []
              in
              (stog, xml, doc :: new_docs, links)
            with
              Not_found ->
                (* not enough information to cut *)
                let (stog, xmls, new_docs, links) =
                  List.fold_right (fold doc new_path cutpoints)
                    xmls (stog, [], new_docs, links)
                in
                (stog, xmls, new_docs, links)

  and fold doc new_path cutpoints xml (stog, xmls, new_docs, links) =
    let (stog, xmls2, new_docs, links) =
      iter doc new_path cutpoints links stog new_docs xml
    in
    (stog, xmls2 @ xmls, new_docs, links)
  in
  let cut_doc stog doc =
    let links = { by_doc = PMap.empty ; next_by_cp = Nmap.empty } in
    match doc.doc_out with
      None -> (stog, doc, [], links)
    | Some body ->
        let (stog, body, new_docs, links) =
          List.fold_right (fold doc doc.doc_path [])
            body (stog, [], [], links)
        in
        let children =
          match new_docs with
            [] -> doc.doc_children
          | _ -> doc.doc_children @ (List.map (fun doc -> doc.doc_path) new_docs)
        in
        (stog,
         { doc with doc_out = Some body ; doc_children = children },
         new_docs, links)
  in
  let add_id_mappings stog src_path dst_path set =
    Sset.fold
      (fun id stog -> Stog_types.id_map_add stog src_path id dst_path (Some id))
      set stog
  in
  let set_doc_id_mappings orig_path all_ids stog doc =
    let ids = id_set doc in
    let stog = add_id_mappings stog orig_path doc.doc_path ids in
    add_id_mappings stog doc.doc_path orig_path (Sset.diff all_ids ids)
  in
  let f_doc env doc_id stog =
    let doc = Stog_types.doc stog doc_id in
    let (stog, doc2, new_docs, links) = cut_doc stog doc in
    match new_docs with
      [] ->
        (* no new documents means the original document was not modified either *)
        stog
    | _ ->
        let all_ids = id_set doc in
        let stog =
          List.fold_left (set_doc_id_mappings doc.doc_path all_ids)
          stog new_docs
        in
        let stog = Stog_types.set_doc stog doc_id doc2 in
        let stog = List.fold_left (add_doc links) stog new_docs in
        stog
  in
  fun env stog docs ->
    Stog_types.Doc_set.fold (f_doc env) docs stog
;;