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

(** *)

type block_data =
  { blocks : (Xtmpl.tree * Xtmpl.tree) Smap.t Smap.t ;
    counters : int Smap.t Smap.t ;
  }

let empty_data = {
    blocks = Smap.empty ;
    counters = Smap.empty ;
  }


let bump_counter data s_hid name =
  let map =
    try Smap.find s_hid data.counters
    with Not_found -> Smap.empty
  in
  let cpt =
    try Smap.find name map + 1
    with Not_found -> 1
  in
  let map = Smap.add name cpt map in
  let data = { data with counters = Smap.add s_hid map data.counters } in
  (data, cpt)
;;

let get_counter data s_hid name =
  try Smap.find name (Smap.find s_hid data.counters)
  with Not_found -> 0
;;

let set_counter data s_hid name v =
  let map =
   try Smap.find s_hid data.counters
   with Not_found -> Smap.empty
  in
  let map = Smap.add name v map in
  { data with counters = Smap.add s_hid map data.counters }
;;

let add_block ?(on_dup=`Warn) ~hid ~id ~short ~long data =
  let map =
    try Smap.find hid data.blocks
    with Not_found -> Smap.empty
    in
  let map =
    try
      ignore (Smap.find id map);
      begin
        let msg = Printf.sprintf "Multiple blocks with id %S for hid=%S" id hid in
        match on_dup with
          `Warn -> Stog_msg.warning msg
        | `Fail -> failwith msg
        | `Ignore -> ()
      end;
      map
    with Not_found ->
        Smap.add id (short, long) map
  in
  { data with blocks = Smap.add hid map data.blocks }
;;

let fun_counter (stog, data) env atts subs =
(*  prerr_endline (Printf.sprintf "fun_counter args=%s\nenv=%s"
    (String.concat "\n" (List.map (fun (s, v) -> Printf.sprintf "%S, %S" s v) atts))
    (Xtmpl.string_of_env env));
*)
  match Xtmpl.get_arg atts ("", "counter-name") with
    None -> ((stog, data), subs)
  | Some name ->
      let ((stog, data), hid) = get_hid (stog, data) env in
      let cpt = get_counter data hid name in
      ((stog, data), [Xtmpl.D (string_of_int cpt)])
;;


let fun_elt_href ?typ src_elt href (stog, data) env args subs =
  let report_error msg = Stog_msg.error ~info: "Stog_html.fun_elt_href" msg in
  let quotes =
    match Xtmpl.get_arg args ("", "quotes") with
      None -> false
    | Some s -> Stog_io.bool_of_string s
  in
  let (stog, data, elt, text) =
    let ((stog,data), info) = elt_by_href ?typ stog (stog,data) env href in
    let (stog, text) =
      match info with
        None -> (stog, [Xtmpl.D "??"])
      | Some (elt, hid, id) ->
          let stog = Stog_deps.add_dep stog src_elt (Stog_types.Elt elt) in
          match subs, id with
          | [], None ->
              let quote = if quotes then "\"" else "" in
              let s = Printf.sprintf "%s%s%s" quote elt.elt_title quote in
              (stog, [Xtmpl.xml_of_string s])
          | text, None -> (stog, text)
          | _, Some id ->
              let hid = Stog_types.string_of_human_id elt.elt_human_id in
              let title =
                try
                  let id_map = Smap.find hid data.blocks in
                  try
                    let (short, long) = Smap.find id id_map in
                    match Xtmpl.get_arg args ("", "long") with
                      Some "true" -> long
                    | _ -> short
                  with Not_found ->
                      let msg = Printf.sprintf "Unknown block hid=%S, id=%S" hid id in
                      report_error msg;
                      Xtmpl.D "??"
                with Not_found ->
                    let msg = Printf.sprintf "Unknown element %S in block map" hid in
                    report_error msg;
                    Xtmpl.D "??"
              in
              match subs with
                [] ->
                  if quotes then
                    (stog, [ Xtmpl.D "\"" ; title ; Xtmpl.D "\""])
                  else
                    (stog, [title])
              | text -> (stog, text)
    in
    (stog, data, info, text)
  in
  match elt with
    None -> ((stog, data), [Xtmpl.E (("", "span"), [("", "class"), "unknown-ref"], text)])
  | Some (elt, _, id) ->
      let href =
        let url = elt_url stog elt in
        match id with
          None -> url
        | Some id -> Neturl.modify_url ~fragment: id url
      in
      let xml = Xtmpl.E (("", "a"), [("", "href"), Stog_types.string_of_url href], text) in
      ((stog, data), [ xml ])
;;

let fun_elt ?typ src_elt (stog, data) env args subs =
  let href =
    match Xtmpl.get_arg args ("", "href") with
      None ->
        let msg = Printf.sprintf "Missing href for <%s>"
          (match typ with None -> "elt" | Some s -> s)
        in
        failwith msg
    | Some s -> s
  in
  fun_elt_href ?typ src_elt href (stog, data) env args subs
;;

let fun_post = fun_elt ~typ: "post";;
let fun_page = fun_elt ~typ: "page";;


let make_fun_section sect_up cls sect_down (stog, data) env args subs =
  let ((stog, data), hid) = get_hid (stog, data) env in
  let data = List.fold_left
    (fun data cls_down ->
       set_counter data hid (concat_name ~sep: ":" cls_down) 0
     )
    data sect_down
  in
  let att_id = (("", "id"), "<id/>") in
  let class_name = concat_name ~sep: "-" cls in
  let body =
    [ Xtmpl.E (("", "div"), [("", "class"), class_name],
       (
        (Xtmpl.E (("", "div"),
          (("", "class"), class_name^"-title") :: att_id :: [],
          [Xtmpl.E (("", "title"),[],[])])) ::
        subs
       )
      )
    ]
  in
  let f ((stog, data), acc) (prefix, cls) =
    let ((stog,data), s) =
      get_in_env (stog,data) env (prefix, (concat_name (prefix, cls))^"-counter")
    in
    let s =
      if Stog_io.bool_of_string s then
        Printf.sprintf "<counter counter-name=%S/>"
          (concat_name (prefix, cls))
      else
        ""
    in
    ((stog, data), s :: acc)
  in
  let ((stog,data), counters) =
    let ((stog,data), cpts) = List.fold_left f ((stog,data), []) (cls::sect_up) in
    ((stog,data), String.concat "."  (List.filter ((<>) "") cpts))
  in
  let ((stog,data), counter_name) =
    let (pref, name) = cls in
    let ((stog,data), s) = get_in_env (stog,data) env (pref, (concat_name cls)^"-counter") in
    if (Stog_io.bool_of_string s) then
      ((stog,data), concat_name cls)
    else
      ((stog,data), "")
  in
  let label = String.capitalize (snd cls) in
  let xmls =
    [ Xtmpl.E (("", Stog_tags.block),
       (("", "label"), label) :: (("", "class"), class_name) :: (("", "counter-name"), counter_name) ::
         (("", "with-contents"), "true") :: args,
       [
         Xtmpl.E (("", "long-title-format"), [],
          [Xtmpl.xml_of_string (Printf.sprintf "%s%s<title/>" counters (if counters = "" then "" else ". "))]);
         Xtmpl.E (("", "short-title-format"), [],
          (match counter_name with
             "" -> [Xtmpl.xml_of_string "<title/>"]
           | _ -> [Xtmpl.xml_of_string counters]
          ));
         Xtmpl.E (("", "contents"), [], body) ;
       ]
      )
    ]
  in
  ((stog, data), xmls)
;;


