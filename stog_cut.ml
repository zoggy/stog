(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Maxence Guesdon. All rights reserved.              *)
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

(** *)

open Stog_types

module Sset = Stog_types.Str_set;;
module Nmap = Xtmpl.Name_map;;

type cutpoint =
  {
    cut_tag : string * string ;
    cut_elt_type : string ;
    cut_hid_sep : string ;
    cut_insert_link : bool ;
  }
;;

(* since we iter in elements with List.fold_right to keep elements in
  order, we encounter next element of a cutpoint A before cutpoint A. *)
type links = {
  by_elt : (Stog_types.human_id option * Stog_types.human_id option) Hid_map.t ;
  next_by_cp : Stog_types.human_id Nmap.t ;
}

let cutpoint_of_atts elt atts =
  let typ = Xtmpl.opt_arg_cdata atts ~def: elt.elt_type ("","type") in
  let tag =
    match Xtmpl.get_arg_cdata atts ("","tag") with
      None -> failwith "Missing 'tag' attribute for <cut-elt> node"
    | Some s ->
        match Stog_misc.split_string s [':'] with
          [] | [_] -> ("", s)
        | h :: q -> (h, String.concat ":" q)
  in
  let sep = Xtmpl.opt_arg_cdata atts ~def: "-" ("", Stog_tags.hid_sep) in
  let insert_link = not (Xtmpl.opt_arg_cdata atts ~def: "true" ("","insert-link") = "false") in
  { cut_tag = tag ; cut_elt_type = typ ;
    cut_hid_sep = sep ; cut_insert_link = insert_link ;
  }
;;

let new_elt_in_cutpoint cut_tag links elt =
  let next_hid =
    try Some (Nmap.find cut_tag links.next_by_cp)
    with Not_found -> None
  in
  let links =
    match next_hid with
      None -> links
    | Some next_hid ->
        let next =
          try snd (Hid_map.find next_hid links.by_elt)
          with Not_found -> None
        in
        let by_elt = Hid_map.add next_hid
          (Some elt.elt_human_id, next) links.by_elt
        in
        let prev =
          try fst (Hid_map.find elt.elt_human_id links.by_elt)
          with Not_found -> None
        in
        let by_elt = Hid_map.add elt.elt_human_id
          (prev, Some next_hid) by_elt
        in
        { links with by_elt }
  in
  let next_by_cp = Nmap.add cut_tag elt.elt_human_id links.next_by_cp in
  { links with next_by_cp }
;;

let add_elt links stog elt =
  let (prev, next) =
    try Hid_map.find elt.elt_human_id links.by_elt
    with Not_found -> None, None
  in
  let elt =
    match prev with
      None -> elt
    | Some prev_hid ->
        let def = (("", Stog_tags.previous_hid), Xtmpl.atts_empty,
           [Xtmpl.D (Stog_types.string_of_human_id prev_hid)])
        in
        { elt with elt_defs = def :: elt.elt_defs }
    in
  let elt =
    match next with
      None -> elt
    | Some next_hid ->
        let def = (("", Stog_tags.next_hid), Xtmpl.atts_empty,
           [Xtmpl.D (Stog_types.string_of_human_id next_hid)])
        in
        { elt with elt_defs = def :: elt.elt_defs }
  in
  Stog_types.add_elt stog elt
;;

let cut_elts =
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
    fun elt ->
      let xmls =
        match elt.elt_out with
          None -> elt.elt_body
        | Some xmls -> xmls
      in
      List.fold_left iter Sset.empty xmls
  in
  let string_of_tag = function
    ("",t) -> "<"^t^">"
  | (n, t) -> "<"^n^":"^t^">"
  in
  let set_id_map stog hid atts new_hid with_id =
    if hid <> new_hid then
      match Xtmpl.get_arg_cdata atts ("","id") with
        None -> stog
      | Some id ->
          let new_id = if with_id then Some id else None in
          Stog_types.id_map_add stog hid id new_hid new_id
    else
      stog
  in
  let rec iter elt new_hid cutpoints links stog new_elts xml =
    match xml with
      Xtmpl.D _ -> (stog, [xml], new_elts, links)
    | Xtmpl.E (("","cut-elt"), atts, xmls) ->
        let cutpoints = (cutpoint_of_atts elt atts) :: cutpoints in
        let (stog, xmls, new_elts, links2) = List.fold_right
          (fold elt new_hid cutpoints) xmls (stog, [], new_elts, links)
        in
        let links = { links2 with next_by_cp = links.next_by_cp } in
        (stog, xmls, new_elts, links)

    | Xtmpl.E (tag, atts, xmls) ->
        let cp_opt =
          try Some (List.find (fun cp -> cp.cut_tag = tag) cutpoints)
          with Not_found -> None
        in
        match cp_opt with
          None ->
            (* not a cut point *)
            let (stog, xmls, new_elts, links) = List.fold_right
              (fold elt new_hid cutpoints) xmls (stog, [], new_elts, links)
            in
            (stog, [Xtmpl.E (tag, atts, xmls)], new_elts, links)
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
              let new_hid_s = (Stog_types.string_of_human_id new_hid) ^ cp.cut_hid_sep ^ id in
              let new_hid = Stog_types.human_id_of_string new_hid_s in
              let stog = set_id_map stog elt.elt_human_id atts new_hid false in
              let (stog, xmls, new_elts, links2) =
                List.fold_right (fold elt new_hid cutpoints)
                  xmls (stog, [], new_elts, links)
              in
              let links = { links2 with next_by_cp = links.next_by_cp } in
              let elt =
                { elt with
                  elt_human_id = new_hid ;
                  elt_parent = Some elt.elt_human_id ;
                  elt_type = cp.cut_elt_type ;
                  elt_title = title ;
                  elt_body = xmls ;
                  elt_out = None ;
                }
              in
              let links = new_elt_in_cutpoint cp.cut_tag links elt in
              let xml =
                if cp.cut_insert_link then
                  [ Xtmpl.E (("","div"),
                     Xtmpl.atts_one ("","class") [Xtmpl.D ("cutlink "^(snd tag))],
                     [Xtmpl.E (("","elt"), Xtmpl.atts_one ("","href") [Xtmpl.D new_hid_s],[])]
                    )
                  ]
                else
                  []
              in
              (stog, xml, elt :: new_elts, links)
            with
              Not_found ->
                (* not enough information to cut *)
                let (stog, xmls, new_elts, links) =
                  List.fold_right (fold elt new_hid cutpoints)
                    xmls (stog, [], new_elts, links)
                in
                (stog, xmls, new_elts, links)

  and fold elt new_hid cutpoints xml (stog, xmls, new_elts, links) =
    let (stog, xmls2, new_elts, links) =
      iter elt new_hid cutpoints links stog new_elts xml
    in
    (stog, xmls2 @ xmls, new_elts, links)
  in
  let cut_elt stog elt =
    let links = { by_elt = Hid_map.empty ; next_by_cp = Nmap.empty } in
    match elt.elt_out with
      None -> (stog, elt, [], links)
    | Some body ->
        let (stog, body, new_elts, links) =
          List.fold_right (fold elt elt.elt_human_id [])
            body (stog, [], [], links)
        in
        let children =
          match new_elts with
            [] -> elt.elt_children
          | _ -> elt.elt_children @ (List.map (fun elt -> elt.elt_human_id) new_elts)
        in
        (stog,
         { elt with elt_out = Some body ; elt_children = children },
         new_elts, links)
  in
  let add_id_mappings stog src_hid dst_hid set =
    Sset.fold
      (fun id stog -> Stog_types.id_map_add stog src_hid id dst_hid (Some id))
      set stog
  in
  let set_elt_id_mappings orig_hid all_ids stog elt =
    let ids = id_set elt in
    let stog = add_id_mappings stog orig_hid elt.elt_human_id ids in
    add_id_mappings stog elt.elt_human_id orig_hid (Sset.diff all_ids ids)
  in
  let f_elt env stog elt_id =
    let elt = Stog_types.elt stog elt_id in
    let (stog, elt2, new_elts, links) = cut_elt stog elt in
    match new_elts with
      [] ->
        (* no new elements means the original element was not modified either *)
        stog
    | _ ->
        let all_ids = id_set elt in
        let stog =
          List.fold_left (set_elt_id_mappings elt.elt_human_id all_ids)
          stog new_elts
        in
        let stog = Stog_types.set_elt stog elt_id elt2 in
        let stog = List.fold_left (add_elt links) stog new_elts in
        stog
  in
  fun env stog elts ->
    List.fold_left (f_elt env) stog elts
;;