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

open Stog_types;;
module Smap = Stog_types.Str_map ;;
module Sset = Stog_types.Str_set ;;

type block_data =
  { blocks : (Xtmpl.tree * Xtmpl.tree) Smap.t Smap.t ;
    counters : int Smap.t Smap.t ;
  }

let empty_data = {
    blocks = Smap.empty ;
    counters = Smap.empty ;
  }

let random_id () =
  Printf.sprintf "%0x-%0x-%0x-%0x"
    (Random.int 0xffff) (Random.int 0xffff)
    (Random.int 0xffff) (Random.int 0xffff)
;;


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
      let ((stog, data), hid) = Stog_html.get_hid (stog, data) env in
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
    let ((stog,data), info) = Stog_html.elt_by_href ?typ stog (stog,data) env href in
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
        let url = Stog_engine.elt_url stog elt in
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
  let ((stog, data), hid) = Stog_html.get_hid (stog, data) env in
  let data = List.fold_left
    (fun data cls_down ->
       set_counter data hid (Stog_html.concat_name ~sep: ":" cls_down) 0
     )
    data sect_down
  in
  let att_id = (("", "id"), "<id/>") in
  let class_name = Stog_html.concat_name ~sep: "-" cls in
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
      Stog_html.get_in_env (stog,data) env (prefix, (Stog_html.concat_name (prefix, cls))^"-counter")
    in
    let s =
      if Stog_io.bool_of_string s then
        Printf.sprintf "<counter counter-name=%S/>"
          (Stog_html.concat_name (prefix, cls))
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
    let ((stog,data), s) = Stog_html.get_in_env (stog,data) env (pref, (Stog_html.concat_name cls)^"-counter") in
    if (Stog_io.bool_of_string s) then
      ((stog,data), Stog_html.concat_name cls)
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

type block =
  { blk_id : string ;
    blk_label : Xtmpl.tree list option ;
    blk_class : string option ;
    blk_title : Xtmpl.tree list ;
    blk_cpt_name : string option ;
    blk_long_f : Xtmpl.tree list ;
    blk_short_f : Xtmpl.tree list ;
    blk_body : Xtmpl.tree list ;
  }

let mk_block ~id ?label ?clas ~title ?counter ~short_fmt ~long_fmt body =
  { blk_id = id ;
    blk_label = label ;
    blk_class = clas ;
    blk_title = title ;
    blk_cpt_name = counter ;
    blk_long_f = long_fmt ;
    blk_short_f = short_fmt ;
    blk_body = body ;
  }
;;

let node_of_block b =
  let atts =
    [
      ("","id"), b.blk_id ;
      ("","with-contents"), "true" ;
    ]
  in
  let title = Xtmpl.E (("","title"), [], b.blk_title) in
  let label =
    match b.blk_label with
      None -> []
    | Some l -> [ Xtmpl.E (("","label"), [], l) ]
  in
  let clas =
    match b.blk_class with
      None -> []
    | Some s -> [ Xtmpl.E (("","class"), [], [ Xtmpl.D s ]) ]
  in
  let cpt_name =
    match b.blk_cpt_name with
      None -> []
    | Some s -> [ Xtmpl.E (("","counter-name"), [], [ Xtmpl.D s]) ]
  in
  let long_f = [ Xtmpl.E (("","long-title-format"), [], b.blk_long_f) ]
  in
  let short_f = [ Xtmpl.E (("","short-title-format"), [], b.blk_short_f) ]
  in
  let contents = [ Xtmpl.E (("","contents"), [], b.blk_body) ] in
  let subs = title :: label @ clas @ cpt_name @ long_f @ short_f @ contents in
  Xtmpl.E (("",Stog_tags.block), atts, subs)
;;

let block_body_of_subs stog blk = function
  [] ->
    let tmpl_file =
      match blk.blk_class with
        None -> "block.tmpl"
      | Some c -> Printf.sprintf "block-%s.tmpl" c
    in
    let tmpl = Filename.concat stog.Stog_types.stog_tmpl_dir tmpl_file in
    [ Xtmpl.xml_of_file tmpl ]
| l -> l
;;

let read_block_from_subs stog =
  let s_xmls l = String.concat "" (List.map Xtmpl.string_of_xml l) in
  let rec f blk = function
    Xtmpl.D _ -> blk
  | Xtmpl.E (("", tag), _, xmls) ->
      begin
        match tag, xmls with
          "id", [Xtmpl.D id] -> { blk with blk_id = id }
        | "id", _ ->
            Stog_msg.warning
            (Printf.sprintf "Ignoring id of block: %S" (s_xmls xmls));
            blk
        | "label", _ -> { blk with blk_label = Some xmls }
        | "class", [Xtmpl.D cls] -> { blk with blk_class = Some cls }
        | "class", _ ->
            Stog_msg.warning
            (Printf.sprintf  "Ignoring class of block: %S" (s_xmls xmls));
            blk
        | "counter-name", [Xtmpl.D s] -> { blk with blk_cpt_name = Some s }
        | "counter-name", _ ->
            Stog_msg.warning
            (Printf.sprintf "Ignoring counter-name of block: %S" (s_xmls xmls));
            blk
        | "title", _ -> { blk with blk_title = xmls }
        | "long-title-format", _ -> { blk with blk_long_f = xmls }
        | "short-title-format", _ -> { blk with blk_short_f = xmls }
        | "contents", _ -> { blk with blk_body = block_body_of_subs stog blk xmls }
        | _, _ ->
            Stog_msg.warning (Printf.sprintf "Ignoring block node %S" tag);
            blk
      end
  | Xtmpl.E _ -> blk
  in
  List.fold_left f
;;

let read_block stog args subs =
  let with_contents =
    match Xtmpl.get_arg args ("", "with-contents") with
      Some "true" -> true
    | None | Some _ -> false
  in
  let blk_id =
    match Xtmpl.get_arg args ("", "id") with
      Some id -> id
    | None -> random_id ()
  in
  let blk_label =
    match Xtmpl.get_arg args ("", "label") with
      None -> None
    | Some s -> Some [ Xtmpl.xml_of_string s ]
  in
  let blk_class = Xtmpl.get_arg args ("", "class") in
  let blk_title =
    match Xtmpl.get_arg args ("", "title") with
      None -> [ Xtmpl.xml_of_string " " ]
    | Some s -> [ Xtmpl.xml_of_string s ]
  in
  let blk_cpt_name = Xtmpl.get_arg args ("", "counter-name") in
  let xml_title = Xtmpl.E (("", "title"), [], []) in
  let xml_label =  Xtmpl.E (("", "label"), [], []) in
  let xml_cpt s = Xtmpl.E (("", "counter"), [("", "counter-name"), s], []) in
  let blk_long_f =
    match Xtmpl.get_arg args ("", "long-title-format") with
      None ->
        (xml_label ::
         (match blk_cpt_name with
            None -> []
          | Some c -> [Xtmpl.D " " ; xml_cpt c]
         ) @ [ Xtmpl.D ": " ; xml_title ]
        )
    | Some s -> [ Xtmpl.xml_of_string s ]
  in
  let blk_short_f =
    match Xtmpl.get_arg args ("", "short-title-format") with
      None ->
        begin
          match blk_label, blk_cpt_name with
            None, None ->  [ xml_title ]
          | Some _, None -> [ xml_label ]
          | None, Some s -> [ xml_cpt s ]
          | Some _, Some s -> [ xml_label ; Xtmpl.D " " ; xml_cpt s ]
        end
    | Some s -> [ Xtmpl.xml_of_string s ]
  in
  let blk = {
    blk_id ; blk_label ; blk_class ; blk_title ;
    blk_cpt_name ; blk_long_f ; blk_short_f ;
    blk_body = [] ;
    }
  in
  match with_contents with
    false -> { blk with blk_body = block_body_of_subs stog blk subs }
  | true -> read_block_from_subs stog blk subs
;;

let fun_block1 (stog, data) env args subs =
  match Xtmpl.get_arg args ("", "href") with
    Some s when s <> "" ->
      begin
        match Xtmpl.get_arg args ("", Stog_tags.elt_hid) with
          Some _ -> raise Xtmpl.No_change
        | None ->
            let ((stog, data), hid) = Stog_html.get_hid (stog, data) env in
            let xmls =
              [ Xtmpl.E (("", Stog_tags.block),
                 [("", Stog_tags.elt_hid), hid ; ("", "href"), s], subs)
              ]
            in
            ((stog, data), xmls)
      end
  | _ ->
      let ((stog, data), hid) = Stog_html.get_hid (stog, data) env in
      let block = read_block stog args subs in
      let data =
        match block.blk_cpt_name with
          None -> data
        | Some name -> fst (bump_counter data hid name)
      in
      let env = Xtmpl.env_add_att "id" block.blk_id env in
      let env = Xtmpl.env_add "title" (fun acc _ _ _ -> (acc, block.blk_title)) env in
      let env = Xtmpl.env_add "label"
        (fun acc _ _ _ ->
           match block.blk_label with
             None -> (acc, [])
           | Some xml -> (acc, xml)
        ) env
      in
      let env = Xtmpl.env_add_att "class" (Stog_misc.string_of_opt block.blk_class) env in
      let env = Xtmpl.env_add_att "counter-name" (Stog_misc.string_of_opt block.blk_cpt_name) env in
      let ((stog, data), long) =
        let ((stog, data), xmls) = Xtmpl.apply_to_xmls (stog, data) env block.blk_long_f in
        ((stog, data), Xtmpl.E (("", Xtmpl.tag_main), [], xmls))
      in
      let ((stog, data), short) =
        let ((stog, data), xmls) = Xtmpl.apply_to_xmls (stog, data) env block.blk_short_f in
         ((stog, data), Xtmpl.E (("", Xtmpl.tag_main), [], xmls))
      in
      let data = add_block ~hid ~id: block.blk_id ~short ~long data in
      let env = Xtmpl.env_add "title" (fun acc _ _ _ -> (acc, [long])) env in
      Xtmpl.apply_to_xmls (stog, data) env block.blk_body
;;

let fun_block2 (stog, data) env atts subs =
  match Xtmpl.get_arg atts ("", "href") with
    None -> ((stog, data), subs)
  | Some href ->
      let hid = match Xtmpl.get_arg atts ("", Stog_tags.elt_hid) with
          None -> assert false
        | Some hid -> hid
      in
      let url = Printf.sprintf "%s#%s" hid href in
      let quotes =
        match Xtmpl.get_arg atts ("", "quotes") with
          None -> "false"
        | Some s -> s
      in
      let xmls =
        [ Xtmpl.E (("", Stog_tags.elt),
           [("", "href"), url ; ("", "quotes"), quotes], [])
        ]
      in
      ((stog, data), xmls)
;;


let gather_existing_ids =
  let rec f hid set = function
    Xtmpl.D _ -> set
  | Xtmpl.E (tag, atts, subs) ->
      let set =
        match Xtmpl.get_arg atts ("", "id") with
          None
        | Some "" -> set
        | Some id ->
            if Sset.mem id set then
              failwith (Printf.sprintf
               "id %S defined twice in the same element %S (here for tag %S)" id
               (Stog_types.string_of_human_id hid) (Stog_html.concat_name tag))
            else
               Sset.add id set
      in
      List.fold_left (f hid) set subs
  in
  fun env (stog, data) elt_id ->
    let elt = Stog_types.elt stog elt_id in
    match elt.elt_out with
      None -> (stog, data)
    | Some body ->
        let set =
          let g set xml =
            try f elt.elt_human_id set xml
            with e ->
                prerr_endline (Xtmpl.string_of_xml xml);
                raise e
          in
          List.fold_left g Sset.empty body
        in
        let title = Xtmpl.xml_of_string elt.elt_title in
        let hid = Stog_types.string_of_human_id elt.elt_human_id in
        let data = Sset.fold
          (fun id data ->
             add_block ~on_dup: `Ignore ~hid ~id ~short: title ~long: title data)
            set data
        in
        (stog, data)
;;

let fun_level_base =
  let f _ _ = [
      ("", Stog_tags.block), fun_block1 ;
      ("", Stog_tags.counter), fun_counter ;
    ]
  in
  Stog_engine.fun_apply_stog_data_elt_rules f
;;

let fun_level_gather_ids =
  let f env (stog, data) elts =
    List.fold_left (gather_existing_ids env) (stog, data) elts
  in
  Stog_engine.Fun_stog_data f
;;


let rules_sectionning stog elt_id =
  let elt = Stog_types.elt stog elt_id in
  let tags = Stog_html.get_sectionning_tags stog elt in
  let rec f acc up = function
    [] -> acc
  | tag :: rest ->
    let rule = (tag, make_fun_section up tag rest) in
    f (rule :: acc) (tag :: up) rest
  in
  let rules = f [] [] tags in
  (("", Stog_tags.counter), fun_counter)::
  (("", Stog_tags.block), fun_block1) :: rules
;;

let fun_level_sectionning =
  Stog_engine.fun_apply_stog_data_elt_rules rules_sectionning ;;


let rules_fun_elt stog elt_id  =
  let elt = Stog_types.elt stog elt_id in
  [ ("", Stog_tags.elt), fun_elt elt  ;
    ("", Stog_tags.post), fun_post elt  ;
    ("", Stog_tags.page), fun_page elt  ;
    ("", Stog_tags.block), fun_block2 ;
  ]
;;
let fun_level_fun_elt =
  Stog_engine.fun_apply_stog_data_elt_rules rules_fun_elt ;;


let dump_data env (stog,data) _ =
  let f_block id (short,long) =
    prerr_endline
      ("id="^id^", short="^(Xtmpl.string_of_xml short)^", long="^(Xtmpl.string_of_xml long))
  in
  let f s_hid map =
    prerr_endline ("Blocks for hid="^s_hid^" :");
    Smap.iter f_block map
  in
  Smap.iter f data.blocks ;
  (stog,data)
;;
let level_funs =
  [
    "base", fun_level_base ;
    "sectionning", fun_level_sectionning ;
    "gather-ids", fun_level_gather_ids ;
    "elt", fun_level_fun_elt ;
    "dump", Stog_engine.Fun_stog_data dump_data ;
  ]
;;

let default_levels =
  List.fold_left
    (fun map (name, levels) -> Stog_types.Str_map.add name levels map)
    Stog_types.Str_map.empty
    [ "base", [ 61 ] ;
      "sectionning", [ 100 ] ;
      "gather-ids", [ 120 ] ;
      "elt", [ 150 ] ;
(*      "dump", [ 101 ; 151 ];*)
    ]

let module_name = "blocks";;

let make_module ?levels () =
  let levels = Stog_html.mk_levels module_name level_funs default_levels ?levels () in
  let module M =
  struct
    type data = block_data
    let modul = {
        Stog_engine.mod_name = module_name ;
        mod_levels = levels ;
        mod_data = empty_data ;
       }

    type cache_data = {
        cache_blocks : (Xtmpl.tree * Xtmpl.tree) Str_map.t ;
      }

    let cache_load data elt t =
      let hid = Stog_types.string_of_human_id elt.elt_human_id in
      let blocks = Smap.add hid t.cache_blocks data.blocks in
      { data with blocks }

    let cache_store data elt =
      let hid = Stog_types.string_of_human_id elt.elt_human_id in
      {
        cache_blocks = (try Smap.find hid data.blocks with Not_found -> Smap.empty) ;
      }
  end
  in
  (module M : Stog_engine.Module)
;;

let f stog =
  let levels =
    try Some (Stog_types.Str_map.find module_name stog.Stog_types.stog_levels)
    with Not_found -> None
  in
  make_module ?levels ()
;;

let () = Stog_engine.register_module module_name f;;
