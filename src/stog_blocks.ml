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
module Smap = Stog_types.Str_map ;;
module Sset = Stog_types.Str_set ;;

module XR = Xtmpl_rewrite
module Xml = Xtmpl_xml

type block_data =
  { blocks : (XR.tree list * XR.tree list) Smap.t Smap.t ;
    counters : int Smap.t Smap.t ;
  }

let empty_data = {
    blocks = Smap.empty ;
    counters = Smap.empty ;
  }

let random_id () =
  Printf.sprintf "%04x-%04x-%04x-%04x"
    (Random.int 0xffff) (Random.int 0xffff)
    (Random.int 0xffff) (Random.int 0xffff)
;;


let bump_counter data s_path name =
  let map =
    try Smap.find s_path data.counters
    with Not_found -> Smap.empty
  in
  let cpt =
    try Smap.find name map + 1
    with Not_found -> 1
  in
  let map = Smap.add name cpt map in
  let data = { data with counters = Smap.add s_path map data.counters } in
  (data, cpt)
;;

let get_counter data s_path name =
  try Smap.find name (Smap.find s_path data.counters)
  with Not_found -> 0
;;

let set_counter data s_path name v =
  let map =
   try Smap.find s_path data.counters
   with Not_found -> Smap.empty
  in
  let map = Smap.add name v map in
  { data with counters = Smap.add s_path map data.counters }
;;

let add_block ?(on_dup=`Warn) ~path ~id ~short ~long data =
  let map =
    try Smap.find path data.blocks
    with Not_found -> Smap.empty
    in
  let map =
    try
      ignore (Smap.find id map);
      begin
        let msg = Printf.sprintf "Multiple blocks with id %S for path=%S" id path in
        match on_dup with
          `Warn -> Stog_msg.warning msg
        | `Fail -> failwith msg
        | `Ignore -> ()
      end;
      map
    with Not_found ->
        Smap.add id (short, long) map
  in
  { data with blocks = Smap.add path map data.blocks }
;;

let add_block_for_doc data doc =
  let path = Stog_path.to_string doc.doc_path in
  try ignore(Smap.find path data.blocks); data
  with Not_found ->
      let blocks = Smap.add path Smap.empty data.blocks in
      { data with blocks }
;;

let fun_counter (stog, data) env atts subs =
(*  prerr_endline (Printf.sprintf "fun_counter args=%s\nenv=%s"
    (String.concat "\n" (List.map (fun (s, v) -> Printf.sprintf "%S, %S" s v) atts))
    (XR.string_of_env env));
*)
  match XR.get_att_cdata atts ("", "counter-name") with
    None -> ((stog, data), subs)
  | Some name ->
      let ((stog, data), path) = Stog_html.get_path (stog, data) env in
      let cpt = get_counter data (Stog_path.to_string path) name in
      ((stog, data), [XR.cdata (string_of_int cpt)])
;;


let fun_doc_href ?typ src_doc href (stog, data) env args subs =
  let src_path_s = Stog_path.to_string src_doc.doc_path in
  let report_error msg = Stog_msg.error ~info: "Stog_html.fun_doc_href" msg in
  let quotes =
    match XR.get_att_cdata args ("", "quotes") with
      None -> false
    | Some s -> Stog_io.bool_of_string s
  in
  let (stog, data, doc, text) =
    let ((stog,data), info) =
      Stog_html.doc_by_href ?typ ~src_doc stog (stog,data) env href
    in
    let (stog, text) =
      match info with
        None -> (stog, [XR.cdata "??"])
      | Some (doc, path, id) ->
          let stog = Stog_deps.add_dep stog src_doc (Stog_types.Doc doc) in
          match subs, id with
          | [], None ->
              let quote = if quotes then "\"" else "" in
              let s = Printf.sprintf "%s%s%s" quote doc.doc_title quote in
              (stog, XR.from_string s)
          | text, None -> (stog, text)
          | _, Some id ->
              let path = Stog_path.to_string doc.doc_path in
              let title =
                try
                  let id_map = Smap.find path data.blocks in
                  try
                    let (short, long) = Smap.find id id_map in
                    match XR.get_att_cdata args ("", "long") with
                      Some "true" -> long
                    | _ -> short
                  with Not_found ->
                      let msg = Printf.sprintf "In %s: Unknown block path=%S, id=%S" src_path_s path id in
                      report_error msg;
                      [ XR.cdata "??" ]
                with Not_found ->
                    let msg = Printf.sprintf "In %s: Unknown document %S in block map" src_path_s path in
                    report_error msg;
                    [ XR.cdata "??" ]
              in
              match subs with
                [] ->
                  if quotes then
                    (stog, XR.cdata "\"" :: title @ [ XR.cdata "\""])
                  else
                    (stog, title)
              | text -> (stog, text)
    in
    (stog, data, info, text)
  in
  match doc with
    None ->
      ((stog, data),
       [XR.node ("", "span")
         ~atts: (XR.atts_one ("", "class") [XR.cdata "unknown-ref"])
           text
       ])
  | Some (doc, _, id) ->
      let href =
        let url = Stog_engine.doc_url stog doc in
        match id with
          None -> url
        | Some id -> Stog_url.with_fragment url id
      in
      let xml = XR.node ("", "a")
        ~atts: (XR.atts_one ("", "href") [XR.cdata (Stog_url.to_string href)])
          text
      in
      ((stog, data), [ xml ])
;;

let fun_doc ?typ src_doc (stog, data) env args subs =
  let href =
    match XR.get_att_cdata args ("", "href") with
      None ->
        let msg = Printf.sprintf "Missing href for <%s>"
          (match typ with None -> "doc" | Some s -> s)
        in
        failwith msg
    | Some s -> s
  in
  fun_doc_href ?typ src_doc href (stog, data) env args subs
;;

let fun_post = fun_doc ~typ: "post";;
let fun_page = fun_doc ~typ: "page";;


let make_fun_section sect_up cls sect_down (stog, data) env args subs =
  let ((stog, data), path) = Stog_html.get_path (stog, data) env in
  let data = List.fold_left
    (fun data cls_down ->
       set_counter data
         (Stog_path.to_string path)
         (Stog_html.concat_name ~sep: ":" cls_down) 0
     )
    data sect_down
  in
  let att_id = XR.atts_one ("", "id") [XR.node ("","id") []] in
  let class_name = Stog_html.concat_name ~sep: "-" cls in
  let body =
    [ XR.node ("", "div")
       ~atts: (XR.atts_one ("", "class") [XR.cdata class_name])
        (
         (XR.node ("", "div")
          ~atts: (XR.atts_one ~atts: att_id ("", "class") [XR.cdata (class_name^"-title")])
            [XR.node ("", "title") []]
         ) :: subs
       )
    ]
  in
  let f ((stog, data), acc) (prefix, cls) =
    let ((stog,data), cpt) =
      Stog_html.get_in_env (stog,data) env
        (prefix, (Stog_html.concat_name (prefix, cls))^"-counter")
    in
    let xml =
      match cpt with
      | [XR.D {Xml.text = "false"}]
      | [XR.D {Xml.text ="0"}] -> []
      | _ ->
          [ XR.node ("","counter")
             ~atts: (XR.atts_one ("","counter-name") [XR.cdata (Stog_html.concat_name (prefix, cls))])
               []
          ]
    in
    ((stog, data), xml :: acc)
  in
  let ((stog,data), counter_name) =
    let (pref, name) = cls in
    let ((stog,data), xmls) =
      match XR.get_att args ("","counter") with
      | Some x -> ((stog, data), x)
      | None ->
          Stog_html.get_in_env (stog,data) env
           (pref, (Stog_html.concat_name cls)^"-counter")
    in
    match xmls with
      [ XR.D {Xml.text = "false"} ]
    | [ XR.D {Xml.text = "0"} ] -> ((stog, data), "")
    | _ -> ((stog,data), Stog_html.concat_name cls)
  in
  let ((stog,data), counters) =
    match counter_name with
      "" -> ((stog,data), [])
    | _ ->
        let ((stog,data), cpts) = List.fold_left f ((stog,data), []) (cls::sect_up) in
        let xmls = Stog_misc.list_concat
          ~sep: [XR.cdata "."] (List.filter ((<>) []) cpts)
        in
        let xmls = List.flatten xmls in
        ((stog,data), xmls)
  in
  let label = String.capitalize (snd cls) in
  let xmls =
    [ XR.node ("", Stog_tags.block)
       ~atts:(XR.atts_of_list ~atts: args
       [ ("", "label"), [XR.cdata label] ;
         ("", "class"), [XR.cdata class_name] ;
         ("", "counter-name"), [XR.cdata counter_name] ;
         ("", "with-contents"), [XR.cdata "true"]
       ])
       [
         XR.node ("", "long-title-format")
          (counters @
            (if counters = [] then [] else [XR.cdata ". "]) @
            [XR.node ("","title") []]
          ) ;
          XR.node ("", "short-title-format")
           (match counter_name with
              "" -> [ XR.node ("","title") [] ]
            | _ -> counters
           );
          XR.node ("", "contents") body ;
       ]
    ]
  in
  ((stog, data), xmls)
;;

type block =
  { blk_id : string ;
    blk_label : XR.tree list option ;
    blk_class : string option ;
    blk_title : XR.tree list ;
    blk_cpt_name : string option ;
    blk_long_f : XR.tree list ;
    blk_short_f : XR.tree list ;
    blk_body : XR.tree list ;
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
    XR.atts_of_list
      [
        ("","id"), [XR.cdata b.blk_id] ;
        ("","with-contents"), [XR.cdata "true"] ;
      ]
  in
  let title = XR.node ("","title") b.blk_title in
  let label =
    match b.blk_label with
      None -> []
    | Some l -> [ XR.node ("","label") l ]
  in
  let clas =
    match b.blk_class with
      None -> []
    | Some s -> [ XR.node ("","class") [ XR.cdata s ] ]
  in
  let cpt_name =
    match b.blk_cpt_name with
      None -> []
    | Some s -> [ XR.node ("","counter-name") [ XR.cdata s] ]
  in
  let long_f = [ XR.node ("","long-title-format") b.blk_long_f ] in
  let short_f = [ XR.node ("","short-title-format") b.blk_short_f ] in
  let contents = [ XR.node ("","contents") b.blk_body ] in
  let subs = title :: label @ clas @ cpt_name @ long_f @ short_f @ contents in
  XR.node ("",Stog_tags.block) ~atts subs
;;

let block_body_of_subs stog doc blk = function
  [] ->
    let tmpl_file =
      match blk.blk_class with
        None -> "block.tmpl"
      | Some c -> Printf.sprintf "block-%s.tmpl" c
    in

    let tmpl = Stog_tmpl.get_template_file stog doc tmpl_file in
    XR.from_file tmpl
| l -> l
;;

let read_block_from_subs stog doc =
  let s_xmls = XR.to_string in
  let rec f blk = function
    XR.D _ -> blk
  | XR.E { XR.name = ("", tag) ; subs } ->
      begin
        match tag, subs with
          "id", [XR.D id] -> { blk with blk_id = id.Xml.text }
        | "id", _ ->
            Stog_msg.warning
              (Printf.sprintf "Ignoring id of block: %S" (s_xmls subs));
            blk
        | "label", _ -> { blk with blk_label = Some subs }
        | "class", [XR.D cls] -> { blk with blk_class = Some cls.Xml.text }
        | "class", _ ->
            Stog_msg.warning
              (Printf.sprintf  "Ignoring class of block: %S" (s_xmls subs));
            blk
        | "counter-name", [XR.D s] when Stog_misc.strip_string s.Xml.text = ""->
            blk
        | "counter-name", [XR.D s] ->
            { blk with blk_cpt_name = Some s.Xml.text }
        | "counter-name", _ ->
            Stog_msg.warning
              (Printf.sprintf "Ignoring counter-name of block: %S" (s_xmls subs));
            blk
        | "title", _ -> { blk with blk_title = subs }
        | "long-title-format", _ -> { blk with blk_long_f = subs }
        | "short-title-format", _ -> { blk with blk_short_f = subs }
        | "contents", _ -> { blk with blk_body = block_body_of_subs stog doc blk subs }
        | _, _ ->
            Stog_msg.warning (Printf.sprintf "Ignoring block node %S" tag);
            blk
      end
  | XR.E _ -> blk
  in
  List.fold_left f
;;

let read_block stog doc args subs =
  let with_contents =
    match XR.get_att_cdata args ("", "with-contents") with
      Some "true" -> true
    | None | Some _ -> false
  in
  let blk_id =
    match XR.get_att_cdata args ("", "id") with
      Some id -> id
    | None -> random_id ()
  in
  let blk_label = XR.get_att args ("", "label") in
  let blk_class = XR.get_att_cdata args ("", "class") in
  let blk_title =
    match XR.get_att args ("", "title") with
      None -> [ XR.cdata " " ]
    | Some l -> l
  in
  let blk_cpt_name = XR.get_att_cdata args ("", "counter-name") in
  let xml_title = XR.node ("", "title") [] in
  let xml_label =  XR.node ("", "label") [] in
  let xml_cpt s = XR.node ("", "counter")
    ~atts: (XR.atts_one ("", "counter-name") [XR.cdata s]) []
  in
  let blk_long_f =
    match XR.get_att args ("", "long-title-format") with
      None ->
        (xml_label ::
         (match blk_cpt_name with
            None -> []
          | Some c -> [XR.cdata " " ; xml_cpt c]
         ) @ [ XR.cdata ": " ; xml_title ]
        )
    | Some xmls -> xmls
  in
  let blk_short_f =
    match XR.get_att args ("", "short-title-format") with
      None ->
        begin
          match blk_label, blk_cpt_name with
            None, None ->  [ xml_title ]
          | Some _, None -> [ xml_label ]
          | None, Some s -> [ xml_cpt s ]
          | Some _, Some s -> [ xml_label ; XR.cdata " " ; xml_cpt s ]
        end
    | Some xmls -> xmls
  in
  let blk = {
    blk_id ; blk_label ; blk_class ; blk_title ;
    blk_cpt_name ; blk_long_f ; blk_short_f ;
    blk_body = [] ;
    }
  in
  match with_contents with
    false -> { blk with blk_body = block_body_of_subs stog doc blk subs }
  | true -> read_block_from_subs stog doc blk subs
;;

let fun_block1 (stog, data) env args subs =
  match XR.get_att_cdata args ("", "href") with
    Some s when s <> "" ->
      begin
        match XR.get_att_cdata args ("", Stog_tags.doc_path) with
          Some _ -> raise XR.No_change
        | None ->
            let ((stog, data), path) = Stog_html.get_path (stog, data) env in
            let path = Stog_path.to_string path in
            let xmls =
              [ XR.node ("", Stog_tags.block)
                 ~atts: (XR.atts_of_list
                 [ ("", Stog_tags.doc_path), [XR.cdata path] ;
                   ("", "href"), [XR.cdata s]])
                 subs
              ]
            in
            ((stog, data), xmls)
      end
  | _ ->
      let ((stog, data), path) = Stog_html.get_path (stog, data) env in
      let (_,doc) = Stog_types.doc_by_path stog path in
      let path = Stog_path.to_string path in
      let block = read_block stog doc args subs in
      let data =
        match block.blk_cpt_name with
          None -> data
        | Some name -> fst (bump_counter data path name)
      in
      let env = XR.env_add_xml "id" [XR.cdata block.blk_id] env in
      let env = XR.env_add_cb "title" (fun acc _ _ _ -> (acc, block.blk_title)) env in
      let env = XR.env_add_cb "label"
        (fun acc _ _ _ ->
           match block.blk_label with
             None -> (acc, [])
           | Some xml -> (acc, xml)
        ) env
      in
      let env = XR.env_add_xml "class"
        [XR.cdata (Stog_misc.string_of_opt block.blk_class)] env
      in
      let env = XR.env_add_xml "counter-name"
        [XR.cdata (Stog_misc.string_of_opt block.blk_cpt_name)] env
      in
      let ((stog, data), long) =
        let ((stog, data), xmls) = XR.apply_to_xmls (stog, data) env block.blk_long_f in
        ((stog, data), xmls)
      in
      let ((stog, data), short) =
        let ((stog, data), xmls) = XR.apply_to_xmls (stog, data) env block.blk_short_f in
         ((stog, data), xmls)
      in
      let data = add_block ~path ~id: block.blk_id ~short ~long data in
      let env = XR.env_add_cb "title" (fun acc _ _ _ -> (acc, long)) env in
      XR.apply_to_xmls (stog, data) env block.blk_body
;;

let fun_block2 (stog, data) env atts subs =
  match XR.get_att_cdata atts ("", "href") with
    None -> ((stog, data), subs)
  | Some href ->
      let path = match XR.get_att_cdata atts ("", Stog_tags.doc_path) with
          None -> assert false
        | Some path -> path
      in
      let url = Printf.sprintf "%s#%s" path href in
      let quotes =
        match XR.get_att_cdata atts ("", "quotes") with
          None -> "false"
        | Some s -> s
      in
      let xmls =
        [ XR.node ("", Stog_tags.doc)
           ~atts: (XR.atts_of_list
           [ ("", "href"), [XR.cdata url] ;
             ("", "quotes"), [XR.cdata quotes]
           ])
           []
        ]
      in
      ((stog, data), xmls)
;;


let gather_existing_ids =
  let rec f path set = function
    XR.D _ -> set
  | XR.E { XR.name ; atts; subs } ->
      let set =
        match XR.get_att_cdata atts ("", "id") with
          None
        | Some "" -> set
        | Some id ->
            if Sset.mem id set then
              (
               Stog_msg.warning
                 (Printf.sprintf
                  "id %S defined twice in the same document %S (here for tag %S)" id
                    (Stog_path.to_string path) (Stog_html.concat_name name));
               set
              )
            else
               Sset.add id set
      in
      List.fold_left (f path) set subs
  in
  fun env doc_id (stog, data) ->
    let doc = Stog_types.doc stog doc_id in
    match doc.doc_out with
      None -> (stog, data)
    | Some body ->
        let data = add_block_for_doc data doc in
        let set =
          let g set xml =
            try f doc.doc_path set xml
            with e ->
                prerr_endline (XR.to_string [xml]);
                raise e
          in
          List.fold_left g Sset.empty body
        in
        let title = XR.from_string doc.doc_title in
        let path = Stog_path.to_string doc.doc_path in
        let data = Sset.fold
          (fun id data ->
             add_block ~on_dup: `Ignore ~path ~id ~short: title ~long: title data)
            set data
        in
        (stog, data)
;;

let fun_init _ (stog,data) doc_ids =
  let f doc_id (stog, data) =
    let doc = Stog_types.doc stog doc_id in
    let path = Stog_path.to_string doc.doc_path in
    let counters = Smap.add path Smap.empty data.counters in
    let blocks =  Smap.add path Smap.empty data.blocks in
    let data = { blocks ; counters } in
    (stog, data)
  in
  Stog_types.Doc_set.fold f doc_ids (stog,data)
;;

let fun_level_base =
  let f _ _ = [
      ("", Stog_tags.block), fun_block1 ;
      ("", Stog_tags.counter), fun_counter ;
    ]
  in
  Stog_engine.fun_apply_stog_data_doc_rules f
;;

let fun_level_gather_ids =
  let f env (stog, data) docs =
    Stog_types.Doc_set.fold (gather_existing_ids env) docs (stog, data)
  in
  Stog_engine.Fun_stog_data f
;;


let rules_sectionning stog doc_id =
  let doc = Stog_types.doc stog doc_id in
  let tags = Stog_html.get_sectionning_tags stog doc in
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
  Stog_engine.fun_apply_stog_data_doc_rules rules_sectionning ;;


let rules_fun_doc stog doc_id  =
  let doc = Stog_types.doc stog doc_id in
  [ ("", Stog_tags.doc), fun_doc doc  ;
    ("", Stog_tags.post), fun_post doc  ;
    ("", Stog_tags.page), fun_page doc  ;
    ("", Stog_tags.block), fun_block2 ;
  ]
;;
let fun_level_fun_doc =
  Stog_engine.fun_apply_stog_data_doc_rules rules_fun_doc ;;


let dump_data env (stog,data) _ =
  let f_block id (short,long) =
    prerr_endline
      ("id="^id^", short="^(XR.to_string short)^", long="^(XR.to_string long))
  in
  let f s_path map =
    prerr_endline ("Blocks for path="^s_path^" :");
    Smap.iter f_block map
  in
  Smap.iter f data.blocks ;
  (stog,data)
;;
let level_funs =
  [
    "init", Stog_engine.Fun_stog_data fun_init ;
    "base", fun_level_base ;
    "sectionning", fun_level_sectionning ;
    "gather-ids", fun_level_gather_ids ;
    "doc", fun_level_fun_doc ;
    "dump", Stog_engine.Fun_stog_data dump_data ;
  ]
;;

let default_levels =
  List.fold_left
    (fun map (name, levels) -> Stog_types.Str_map.add name levels map)
    Stog_types.Str_map.empty
    [
      "init", [ 0 ] ;
      "base", [ 61 ] ;
      "sectionning", [ 100 ] ;
      "gather-ids", [ 120 ] ;
      "doc", [ 150 ] ;
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
        cache_blocks : (XR.tree list * XR.tree list) Str_map.t ;
      }

    let cache_load _stog data doc t =
      let path = Stog_path.to_string doc.doc_path in
      let blocks = Smap.add path t.cache_blocks data.blocks in
      { data with blocks }

    let cache_store _stog data doc =
      let path = Stog_path.to_string doc.doc_path in
      {
        cache_blocks = (try Smap.find path data.blocks with Not_found -> Smap.empty) ;
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
