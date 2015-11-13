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
module Smap = Stog_types.Str_map;;

module XR = Xtmpl_rewrite;;
module Xml = Xtmpl_xml;;

let get_in_env = Stog_engine.get_in_env;;

let get_path = Stog_engine.get_path;;

let escape_html s =
  let b = Buffer.create 256 in
  for i = 0 to String.length s - 1 do
    let s =
      match s.[i] with
        '<' -> "&lt;"
      | '>' -> "&gt;"
      | '&' -> "&amp;"
      | c -> String.make 1 c
    in
    Buffer.add_string b s
  done;
  Buffer.contents b
;;

let url_of_path stog path =
  let doc = Stog_types.make_doc ~path () in
  let src = Stog_path.to_string path in
  Stog_engine.doc_url stog { doc with Stog_types.doc_src = src }
;;

let topic_index_path topic =
  Stog_path.of_string ("/topic_" ^ topic ^".html");;
let keyword_index_path kw =
  Stog_path.of_string ("/kw_"^ kw ^ ".html");;
let month_index_path ~year ~month =
  Stog_path.of_string (Printf.sprintf "/%04d_%02d.html" year month);;

let plugin_base_rules = ref [];;

let register_base_rule name f =
   plugin_base_rules := (name, f) :: !plugin_base_rules ;;

let include_href name stog doc ?id ~raw ~subsonly ~depend ?loc href env =
  let new_id = id in
  let (path, id) =
    try
      let p = String.index href '#' in
      let len = String.length href in
      let path = String.sub href 0 p in
      (path, String.sub href (p+1) (len - (p+1)))
    with
      Not_found ->
        failwith ("Missing #id part of href in <"^name^"> rule")
  in
  try
    let (stog, path) =
      match path with
        "" -> get_path stog env
      | s ->  (stog, Stog_path.of_string s)
    in
    let (_, doc) = Stog_types.doc_by_path stog path in
    let stog =
      if depend then Stog_deps.add_dep stog doc (Stog_types.Doc doc) else stog
    in
    let (doc, id) = Stog_types.map_doc_ref stog doc id in
    match Stog_types.find_block_by_id doc id with
    | None ->
        failwith
          (Printf.sprintf "No id %S in document %S"
           id (Stog_path.to_string path))
    | Some xml ->
        match xml with
        | XR.D _ | XR.C _ | XR.PI _ -> assert false
        | XR.E node ->
            let xmls =
              match raw, subsonly with
                true, false ->
                  [ XR.cdata (XR.to_string [xml]) ]
              | true, true ->
                  [ XR.cdata (XR.to_string node.XR.subs) ]
              | false, true ->
                  node.XR.subs
              | false, false ->
                  match new_id with
                    None -> [xml]
                  | Some new_id ->
                      let atts = XR.atts_replace ("","id") new_id node.XR.atts in
                      [ XR.E { node with XR.atts } ]
            in
            (stog, xmls)
  with
    Failure s ->
      Stog_msg.error s;
      (stog, [XR.cdata ("??"^href^"??")])
;;

let include_file stog doc ?id ~raw ~depend file ?loc args subs =
  let atts = XR.atts_one ~atts: args ("", "contents") subs in
  try
    let (stog, xml) = Stog_tmpl.read_template_file stog doc ~depend ~raw file in
    (stog, [XR.node ("", XR.tag_env) ~atts xml])
  with
    e -> Stog_error.error_loc ?loc e
;;

let fun_include_ name doc stog env ?loc args subs =
  let raw = XR.opt_att_cdata ~def: "false" args ("", "raw") = "true" in
  let subsonly = XR.opt_att_cdata ~def: "false" args ("", "subs-only") = "true" in
  let id = XR.get_att args ("", "id") in
  let depend = XR.opt_att_cdata args ~def: "true" ("", "depend") <> "false" in
  match XR.get_att_cdata args ("", "file") with
  | Some file ->
      let (stog, xml) = include_file stog doc ?id ~raw ~depend file ?loc args subs in
      (stog, xml)
  | None ->
      match XR.get_att_cdata args ("", "href") with
        Some href -> include_href name stog doc ?id ~raw ~subsonly ~depend ?loc href env
      | None ->
          failwith ("Missing 'file' or 'href' argument for <"^name^"> rule")
;;

let fun_include = fun_include_ (Stog_tags.include_);;
let fun_late_inc = fun_include_ (Stog_tags.late_inc);;

let fun_inc doc stog env ?loc args subs =
  Stog_msg.warning ("<"^Stog_tags.inc^"> rule is deprecated; use <"^Stog_tags.late_inc^"> rule instead");
  fun_late_inc doc stog env args subs
;;

let fun_image acc _env ?loc args legend =
  let width = XR.opt_att args ("", "width") in
  let src = XR.opt_att args ("", "src") in
  let cls = Printf.sprintf "img%s"
    (match XR.get_att_cdata args ("", "float") with
      Some s ->
         begin
           match s with
             "left" -> "-float-left"
           | "right" -> "-float-right"
           | s -> failwith (Printf.sprintf "unhandled image position: %s" s)
         end
     | None -> ""
    )
  in
  let cls = [ XR.cdata cls ] in
  let cls =
    match XR.get_att args ("", "class") with
      None -> cls
    | Some c -> c @ [XR.cdata " "] @ cls
  in
  let atts = XR.atts_remove ("","width") args in
  let atts = XR.atts_remove ("","src") atts in
  let atts = XR.atts_remove ("","float") atts in
  let xmls =
    [
      XR.node ("", "div") ~atts: (XR.atts_one ("", "class") cls)
       ((XR.node ("", "img")
         ~atts: (XR.atts_of_list ~atts
          [ ("", "class"), [XR.cdata "img"] ; ("", "src"), src; ("", "width"), width ])
          []
        ) ::
        (match legend with
           [] -> []
         | xml ->
             [ XR.node ("", "div")
               ~atts: (XR.atts_one ("", "class") [XR.cdata "legend"])
                xml
             ]
         )
        )
    ]
  in
  (acc, xmls)
;;

let fun_list acc env ?loc args subs =
  let sep = XR.opt_att args ("", "sep") in
  let sep = List.rev sep in
  let rec iter acc = function
    [] -> List.rev acc
  | h :: q ->
      let acc =
        match acc with
          [] -> [h]
        | _ -> h :: sep @ acc
      in
      iter acc q
  in
  (* and finally return the list of xml trees *)
  (acc, iter [] subs)
;;

let doc_by_href ?typ ?src_doc stog acc env ?loc href =
  let (path, id) =
    try
      let p = String.index href '#' in
      let len = String.length href in
      let path = String.sub href 0 p in
      let id = String.sub href (p+1) (len - (p+1)) in
      (path, id)
    with
      Not_found -> (href, "")
  in
  let (acc, info) =
    try
      let (acc, path) =
        match path with
          "" -> get_path acc env
        | s ->  (acc, Stog_path.of_string s)
      in
      let (_, doc) = Stog_types.doc_by_path ?typ stog path in
      let (doc, id) = Stog_types.map_doc_ref stog doc id in
      let path = Stog_path.to_string doc.doc_path in
      (acc, Some (doc, path, id))
    with
      Failure s ->
        let msg =
          match src_doc with
            None -> s
          | Some doc ->
            "In "^(Stog_path.to_string doc.doc_path)^": "^s
        in
        Stog_msg.error ?loc ~info: "Stog_html.doc_by_href" msg;
        (acc, None)
  in
  match info with
    None -> (acc, None)
  | Some (doc, path, "") -> (acc, Some (doc, path, None))
  | Some (doc, path, id) -> (acc, Some (doc, path, Some id))
;;

(* FIXME: add adependency ? *)
let fun_archive_tree stog _env ?loc _atts _subs =
  let mk_months map =
    List.sort (fun (m1, _) (m2, _) -> compare m2 m1)
    (Stog_types.Int_map.fold
     (fun month data acc -> (month, data) :: acc)
     map
     []
    )
  in
  let years =
    Stog_types.Int_map.fold
      (fun year data acc -> (year, mk_months data) :: acc)
      stog.stog_archives
      []
  in
  let years = List.sort (fun (y1,_) (y2,_) -> compare y2 y1) years in

  let f_mon year (month, set) =
    let path = month_index_path ~year ~month in
    let href = url_of_path stog path in
    let month_str = Stog_intl.get_month stog.stog_lang month in
    XR.node ("", "li")
      [
       XR.node ("", "a")
          ~atts: (XR.atts_one ("", "href") [XR.cdata (Stog_url.to_string href)])
          [ XR.cdata month_str ] ;
       XR.cdata (Printf.sprintf "(%d)" (Stog_types.Doc_set.cardinal set))
      ]
  in
  let f_year (year, data) =
    XR.node ("", "li")
      [
       XR.cdata (string_of_int year) ;
       XR.node ("", "ul") (List.map (f_mon year) data)
      ]
  in
  (stog, [ XR.node ("", "ul") (List.map f_year years) ])
;;

let fun_hcode ?(inline=false) ?lang stog _env ?loc args code =
  let lang, opts =
    match lang with
      None ->
        (
         match XR.get_att_cdata args ("", "lang-file") with
           None ->
             let lang = XR.opt_att_cdata args ~def: "txt" ("", "lang") in
             (lang, None)
         | Some f ->
             let lang = XR.opt_att_cdata args ~def: "" ("", "lang") in
             let opts = Printf.sprintf "--config-file=%s" f in
             (lang, Some opts)
        )
    | Some "ocaml" ->
        let lang_file = Filename.concat stog.stog_dir "ocaml.lang" in
        let opts = if Sys.file_exists lang_file then
            Some (Printf.sprintf "--config-file=%s" lang_file)
          else
            None
        in
        ("ocaml", opts)
    | Some lang ->
        (lang, None)
  in
  let code =
    let l = List.map
      (
       function
       | XR.D code -> code.Xml.text
       | x -> XR.to_string [x]
      )
      code
    in
    String.concat "" l
  in
  let code = Stog_misc.strip_blank_lines code in
  let xmls = Stog_highlight.highlight ~lang ?opts code in
  let atts =
    match XR.get_att_cdata args ("","id") with
      None -> XR.atts_empty
    | Some id -> XR.atts_one ("","id") [XR.cdata id]
  in
  let xmls =
    if inline then
      [ XR.node ("", "span")
        ~atts: (XR.atts_one ~atts ("", "class") [XR.cdata "icode"]) xmls
      ]
    else
      [ XR.node ("", "pre")
        ~atts: (XR.atts_one ~atts ("", "class") [XR.cdata (Printf.sprintf "code-%s" lang)])
         xmls
      ]
  in
  (stog, xmls)
;;

let fun_ocaml = fun_hcode ~lang: "ocaml";;
let fun_command_line = fun_hcode ~lang: "sh";;
let fun_icode = fun_hcode ~inline: true ;;

let concat_name ?(sep=":") (prefix, name) =
  match prefix with
    "" -> name
  | p -> p ^ sep ^ name
;;

let fun_as_xml =
  let rec iter xml =
    match xml with
    | XR.D s -> XR.from_string s.Xml.text
    | XR.E node ->
        [ XR.E { node with
            XR.subs = List.flatten (List.map iter node.XR.subs) }
        ]
    | XR.C _ | XR.PI _ -> [xml]
  in
  fun x _env ?loc _ subs ->
    let xmls = XR.merge_cdata_list subs in
    (x, List.flatten (List.map iter xmls))
;;

let fun_as_cdata x _env ?loc _ subs = (x, [XR.cdata (XR.to_string subs)])

(* FIXME: add dependency ? *)
let fun_graph =
  let generated = ref false in
  let report_error msg = Stog_msg.error ~info: "Stog_html.fun_graph" msg in
  fun stog _env ?loc _ _ ->
    let png_name = "site-graph.png" in
    let small_png_name = "small-"^png_name in
    let svg_file = (Filename.chop_extension png_name) ^ ".svg" in
    let src = Stog_url.concat stog.stog_base_url svg_file in
    let small_src = Stog_url.concat stog.stog_base_url small_png_name in
    begin
      match !generated with
        true -> ()
      | false ->
          generated := true;
          let dot_code = Stog_info.dot_of_graph (Stog_engine.doc_url stog) stog in

          let tmp = Filename.temp_file "stog" "dot" in
          Stog_misc.file_of_string ~file: tmp dot_code;

          let com = Printf.sprintf "dot -Gcharset=utf-8 -Tpng -o %s %s"
            (Filename.quote (Filename.concat stog.stog_outdir png_name))
            (Filename.quote tmp)
          in
          let svg_code = Stog_misc.dot_to_svg dot_code in
          Stog_misc.file_of_string ~file: (Filename.concat stog.stog_outdir svg_file) svg_code;
          match Sys.command com with
            0 ->
              begin
                (try Sys.remove tmp with _ -> ());
                let com = Printf.sprintf "convert -scale 120x120 %s %s"
                  (Filename.quote (Filename.concat stog.stog_outdir png_name))
                  (Filename.quote (Filename.concat stog.stog_outdir small_png_name))
                in
                match Sys.command com with
                  0 -> ()
                | _ ->
                    report_error (Printf.sprintf "Command failed: %s" com)
              end
          | _ ->
              report_error (Printf.sprintf "Command failed: %s" com)
    end;
    let xmls = [
        XR.node ("", "a")
         ~atts: (XR.atts_one ("", "href") [ XR.cdata (Stog_url.to_string src)])
         [
           XR.node ("", "img")
            ~atts: (XR.atts_of_list
               [ ("", "src"), [ XR.cdata (Stog_url.to_string small_src)] ;
                 ("", "alt"), [XR.cdata "Graph"]])
            []
         ]
      ]
    in
    (stog, xmls)
;;

let fun_if stog env ?loc args subs =
  let pred (prefix, name) v (stog, cond) =
    let nodes = [ XR.node (prefix, name) [] ] in
    let v_nodes = XR.to_string nodes in
    let (stog, nodes2) = XR.apply_to_xmls stog env nodes in
    let v2 = if nodes = nodes2 then [] else nodes2 in
(*
    let v = match v with [XR.D { Xml.text = ""}] -> [] | _ -> v in
    let v2 = match v2 with [XR.D { Xml.text = ""}] -> [] | _ -> v2 in
*)
    let v = XR.to_string v in
    let v2 = XR.to_string v2 in
    let v2 = if v2 = v_nodes then "" else v2 in
(*
    prerr_endline (Printf.sprintf "fun_if: pred: att=(%s,%s), nodes=%S nodes2=%S, v=%S, v2=%S"
     prefix name (XR.string_of_xmls nodes)
       (XR.string_of_xmls nodes2) (XR.string_of_xmls v) (XR.string_of_xmls v2));
*)
(*
       prerr_endline (Printf.sprintf "v_xml=%S, v2_xml=%S, subs=%S, v=v2=%b"
       (XR.string_of_xmls v) (XR.string_of_xmls v2) (XR.string_of_xmls subs) (v=v2));
*)
    (stog, cond && (v = v2))
  in
  let (stog, cond) = Xml.Name_map.fold pred args (stog, true) in
  let subs = List.filter
    (function XR.D _ -> false | _ -> true)
    subs
  in
  let xmls =
    match cond, subs with
    | true, [] -> failwith (Xml.loc_sprintf loc "<if>: missing children")
    | true, h :: _
    | false, _ :: h :: _ -> [h]
    | false, []
    | false, [_] -> []
  in
  (stog, xmls)
;;

let fun_dummy_ data _ ?loc _ subs = (data, subs);;

let fun_twocolumns stog env ?loc args subs =
  (*prerr_endline (Printf.sprintf "two-columns, length(subs)=%d" (List.length subs));*)
  let empty = [] in
  let subs = List.fold_right
    (fun xml acc ->
       match xml with
         XR.D _ | XR.C _ | XR.PI _ -> acc
       | XR.E { XR.subs } -> subs :: acc
    ) subs []
  in
  let left, right =
    match subs with
      [] -> empty, empty
    | [left] -> left, empty
    | left :: right :: _ -> left, right
  in
  let xmls =
    [ XR.node ("", "table") ~atts: (XR.atts_one ("", "class") [XR.cdata "two-columns"])
      [ XR.node ("", "tr")
        [ XR.node ("", "td")
          ~atts: (XR.atts_one ("", "class") [XR.cdata "two-columns-left"]) left;
          XR.node ("", "td")
            ~atts: (XR.atts_one ("", "class") [XR.cdata "two-columns-right"]) right;
        ]
      ]
    ]
  in
  (stog, xmls)
;;

let fun_ncolumns stog env ?loc args subs =
  let subs = List.fold_right
    (fun xml acc ->
       match xml with
         XR.D _ | XR.C _ | XR.PI _ -> acc
       | XR.E { XR.subs } -> subs :: acc
    ) subs []
  in
  let tds =
    let f (n,acc) xmls =
       let acc =
        (XR.node ("", "td")
          ~atts: (XR.atts_one ("", "class") [XR.cdata (Printf.sprintf "n-columns column-%d" n)])
           xmls
        ) :: acc
      in
      (n+1, acc)
    in
    List.rev (snd (List.fold_left f (0,[]) subs))
  in
  let xmls =
    [ XR.node ("", "table")
      ~atts: (XR.atts_one ("", "class") [XR.cdata "n-columns"])
        [ XR.node ("", "tr") tds ]
    ]
  in
  (stog, xmls)
;;

let fun_exta stog env ?loc atts subs =
  (stog,
   [ XR.node ("", "span")
     ~atts: (XR.atts_one ("", "class") [XR.cdata "ext-a"])
      [ XR.node ("", "a") ~atts subs ]
   ]
  )
;;

type toc = Toc of string option * XR.tree list * Xml.name * toc list (* name, title, class, subs *)

let fun_prepare_toc tags stog env ?loc args subs =
  let depth =
    match XR.get_att_cdata args ("", "depth") with
      None -> max_int
    | Some s -> int_of_string s
  in
  let show_noids =
    XR.opt_att_cdata args ~def: "false" ("", "show-without-ids") <> "false"
  in
  let rec iter d acc = function
  | XR.D _ | XR.C _ | XR.PI _ -> acc
  | XR.E { XR.name = tag; atts ; subs } when List.mem tag tags ->
      begin
        match
          XR.get_att_cdata atts ("", "id"),
          XR.get_att atts ("", "title")
        with
          None, None
        | Some _, None ->
            acc
        | id, Some title ->
            let name, ok =
              match id with
                None -> (None, show_noids)
              | Some id -> (Some id, true)
            in
            if (not ok) || d > depth
            then acc
            else
              (
               let subs = List.rev (List.fold_left (iter (d+1)) [] subs) in
               (Toc (name, title, tag, subs)) :: acc
              )
      end
  | XR.E { XR.subs } -> List.fold_left (iter d) acc subs
  in
  let toc = List.rev (List.fold_left (iter 1) [] subs) in
  (*(
   match toc with
     [] -> prerr_endline "empty toc!"
   | _ -> prerr_endline (Printf.sprintf "toc is %d long" (List.length toc));
  );*)
  let rec xml_of_toc = function
    Toc (name, title, cl, subs) ->
      let cl =
        match cl with
          ("", s) -> s
        | (p, s) -> p ^"-"^ s
      in
      XR.node ("", "li")
        ~atts: (XR.atts_one ("", "class") [XR.cdata ("toc-"^cl)])
        (
         (match name with
            None -> title
          | Some name ->
              let atts = XR.atts_of_list
                [ ("", "href"), [XR.cdata ("#"^name)] ;
                  ("","long"), [XR.cdata "true"]
                ]
              in
              [ XR.node ("", "doc") ~atts [] ]
         )
           @
           ( match subs with
              [] -> []
            | _ ->
                [ XR.node ("", "ul")
                  ~atts: (XR.atts_one ("", "class") [XR.cdata "toc"])
                    (List.map xml_of_toc subs)
                ]
           )
        )
  in
  let xml =
    XR.node ("", "ul")
     ~atts: (XR.atts_one ("", "class") [XR.cdata "toc"])
     (List.map xml_of_toc toc)
  in
  let atts = XR.atts_one ("", "toc-contents") [ xml ] in
  (stog, [ XR.node ("", XR.tag_env) ~atts subs ])
;;

let fun_toc stog env ?loc args subs =
  (stog, subs @ [XR.node ("", "toc-contents") [] ])
;;

let concat_xmls ?(sep=[]) l =
  let rec f xml = function
    [] -> xml
  | l -> xml @ sep @ l
  in
  List.fold_right f l []
;;

let fun_error_ stog env ?loc args subs =
  let (stog, xmls) = XR.apply_to_xmls stog env subs in
  let s = XR.to_string xmls in
  Stog_msg.error s;
  (stog, [])
;;

let fun_doc_navpath doc stog env ?loc args subs =
  let root =
    match XR.get_att_cdata args ("", "with-root") with
      None -> None
    | Some root_path ->
        let root_path = Stog_path.of_string root_path in
        ignore(Stog_types.doc_by_path stog root_path);
        Some root_path
  in
  let rec f acc path =
    (*prerr_endline (Printf.sprintf "path = [%s]" (String.concat "/" path));*)
    match List.rev path with
      [] ->
        begin
          match root with
            None -> acc
          | Some path -> path :: acc
        end
    | _ :: q ->
        let path = { Stog_path.path = path ; path_absolute = true } in
        f (path :: acc) (List.rev q)
  in
  let path = doc.Stog_types.doc_path in
  let paths =
    (* remove last component of path to keep only "parent path" *)
    match List.rev path.Stog_path.path with
      [] | [_] -> (match root with None -> [] | Some path -> [path])
    | _ :: q -> f [] (List.rev q)
  in
  let map path =
    try
      let path =
        (* try to link to /the/path/index *)
        try
          let path = Stog_path.append path ["index"] in
          ignore(Stog_types.doc_by_path stog path);
          path
        with
          Failure _ ->
            (* if no such document exist, try /the/path *)
            ignore(Stog_types.doc_by_path stog path);
            path
      in
      let xml = XR.node ("", Stog_tags.doc)
        ~atts: (XR.atts_one ("","href") [XR.cdata (Stog_path.to_string path)])
          []
      in
      [ xml ]
    with Failure _ ->
        match List.rev path.Stog_path.path with
          [] -> [XR.cdata "?"]
        | h :: _ -> [ XR.cdata h ]
  in
  let xmls = concat_xmls ~sep: subs (List.map map paths) in
  (stog, xmls)
;;



let intro_of_doc stog doc =
  let rec iter acc = function
    [] -> raise Not_found
  | (XR.E { XR.name = ("",tag) }) :: _ when tag = Stog_tags.sep -> List.rev acc
  | h :: q -> iter (h::acc) q
  in
  try
    let xml = iter [] doc.doc_body in
    xml
  with
    Not_found -> doc.doc_body
;;

let html_of_topics doc stog env ?loc args _ =
  let sep = XR.opt_att args ~def: [XR.cdata ", "] ("", "sep") in
  let (stog, tmpl) = Stog_tmpl.get_template stog ~doc Stog_tmpl.topic "topic.tmpl" in
  let f stog w =
    let env = XR.env_of_list ~env
      [ ("", Stog_tags.topic), (fun acc _ ?loc _ _ -> (acc, [XR.cdata w])) ]
    in
    XR.apply_to_xmls stog env tmpl
  in
  let (stog, l) =
    List.fold_left
      (fun (stog, acc) w ->
         let (stog, xmls) = f stog w in
         let href = url_of_path stog (topic_index_path w) in
         let xml = XR.node ("", "a")
            ~atts: (XR.atts_one ("", "href") [ XR.cdata (Stog_url.to_string href) ])
            xmls
         in
         (stog, [xml] :: acc)
      )
      (stog, []) doc.doc_topics
  in
  let xmls = Stog_misc.list_concat ~sep (List.rev l) in
  (stog, List.flatten xmls)
;;

let html_of_keywords doc stog env ?loc args _ =
  let sep = XR.opt_att args ~def: [XR.cdata ", "] ("", "sep") in
  let (stog, tmpl) = Stog_tmpl.get_template stog ~doc Stog_tmpl.keyword "keyword.tmpl" in
  let f stog w =
    let env = XR.env_of_list ~env
      [ ("", Stog_tags.keyword), (fun acc _ ?loc _ _ -> (acc, [XR.cdata w])) ]
    in
    XR.apply_to_xmls stog env tmpl
  in
  let (stog, l) =
    List.fold_left
      (fun (stog, acc) w ->
         let (stog, xmls) = f stog w in
         let href = url_of_path stog (keyword_index_path w) in
         let xml = XR.node ("", "a")
            ~atts: (XR.atts_one ("", "href") [XR.cdata (Stog_url.to_string href)])
            xmls
         in
         (stog, [xml] :: acc)
      )
      (stog, []) doc.doc_keywords
  in
  let xmls = Stog_misc.list_concat ~sep (List.rev l) in
  (stog, List.flatten xmls)
;;

(*
(* FIXME: handle RSS files as any other document ? *)
let rec doc_to_rss_item stog doc_id doc =
  let link = Stog_engine.doc_url stog doc in
  let pubdate =
    match doc.doc_date with
      None -> assert false
    | Some d -> d
  in
  let f_word w =
    { Rss.cat_name = w ; Rss.cat_domain = None }
  in
  let cats =
    (List.map f_word doc.doc_topics) @
    (List.map f_word doc.doc_keywords)
  in
  let (stog, env) = Stog_engine.doc_env stog (XR.env_empty()) stog doc in
  let (stog, author) =
    let (stog, author) = get_in_env stog env ("", "author") in
    let author = XR.string_of_xmls author in
    (stog, Stog_misc.opt_of_string author)
  in
  let (stog, desc_xml) = XR.apply_to_xmls stog env (intro_of_doc stog doc) in
  let desc = XR.string_of_xmls desc_xml in
  let item =
    Rss.item ~title: doc.doc_title
      ~desc
      ~link
      ~pubdate
      ~cats
      ~guid: (Rss.Guid_permalink link)
      ?author
      ()
  in
  (stog, item)

and generate_rss_feed_file stog ?title link docs file =
  let docs = (* no reverse since the list is reversed
    below in a List.fold_left when converting to items *)
    Stog_types.sort_ids_docs_by_date docs
  in
  let docs = List.filter
    (fun (_,doc) -> match doc.doc_date with None -> false | _ -> true)  docs
  in
  let (stog, items) = List.fold_left
    (fun (stog, acc) (id, doc) ->
       let (stog, item) = doc_to_rss_item stog id doc in
       (stog, item :: acc)
    )
      (stog, [])
      docs
  in
  let title = Printf.sprintf "%s%s"
    stog.stog_title
    (match title with None -> "" | Some t -> Printf.sprintf ": %s" t)
  in
  let pubdate =
    match docs with
      [] -> None
    | (_,h) :: _ -> h.doc_date
  in
  let image =
    try
      let file =
        match Stog_types.get_def stog.stog_defs ("", "rss-image") with
          Some (_,xmls) -> XR.string_of_xmls xmls
        | None -> ""
      in
      let url = Stog_types.url_concat stog.stog_base_url file in
      let image = {
          Rss.image_url = url ;
          image_title = stog.stog_title ;
          image_link = stog.stog_base_url ;
          image_height = None ;
          image_width = None ;
          image_desc = None ;
        }
      in
      Some image
    with
      Not_found -> None
  in
  let desc = String.concat "" (List.map XR.string_of_xml stog.stog_desc) in
  let channel =
    Rss.channel ~title ~link
    ~desc ?image
    ~managing_editor: stog.stog_email
    ?pubdate ?last_build_date: pubdate
    ~generator: "Stog"
    items
  in
  let channel = Rss.keep_n_items stog.stog_rss_length channel in
  Rss.print_file ~encoding: "UTF-8" file channel;
  stog
*)


let format_date d f stog args =
  let s =
    match XR.get_att_cdata args ("","format") with
      None -> f stog.stog_lang d
    | Some fmt -> Netdate.format ~fmt d
  in
  (stog, [ XR.cdata s ])
;;

let fun_date_gen f stog env ?loc args _ =
  let (stog, path) = Stog_engine.get_path_in_args_or_env stog env args in
  let (_, doc) = Stog_types.doc_by_path stog path in
  match doc.doc_date with
    None -> (stog, [])
  | Some d -> format_date d f stog args
;;

let fun_date = fun_date_gen Stog_intl.string_of_date ;;
let fun_datetime = fun_date_gen Stog_intl.string_of_datetime ;;

let fun_date_today stog env ?loc args _ =
  let d = Netdate.create (Unix.time()) in
  format_date d Stog_intl.string_of_date stog args;;

let fun_date_now stog env ?loc args _ =
  let d = Netdate.create (Unix.time()) in
  format_date d Stog_intl.string_of_datetime stog args;;

let fun_print_date_gen f stog ?loc args subs =
  match XR.merge_cdata_list subs with
    [XR.D cdata] ->
      begin
        try
          let d = Stog_io.date_of_string cdata.Xml.text in
          format_date d f stog args
        with Failure s ->
          Stog_msg.error ?loc: cdata.Xml.loc s;
          (stog, [])
      end
  | _ -> raise XR.No_change

let fun_print_date stog env ?loc args subs =
  fun_print_date_gen Stog_intl.string_of_date stog args subs;;
let fun_print_datetime stog env ?loc args subs =
  fun_print_date_gen Stog_intl.string_of_datetime stog args subs;;

let rec build_base_rules stog doc_id =
  let doc = Stog_types.doc stog doc_id in
  let f_title doc acc _ ?loc _ _ =
    (acc, XR.from_string doc.doc_title)
  in
  let f_url doc stog _ ?loc _ _ =
    (stog,[ XR.cdata (Stog_url.to_string (Stog_engine.doc_url stog doc)) ])
  in
  let f_body doc acc _ ?loc _ _ = (acc, doc.doc_body) in
  let f_type doc acc _ ?loc _ _ = (acc, [XR.cdata doc.doc_type]) in
  let f_src doc acc _ ?loc _ _ = (acc, [XR.cdata doc.doc_src]) in
  let f_intro doc stog _ ?loc _ _ = (stog, intro_of_doc stog doc) in
  let mk f stog env ?loc atts subs =
    let doc =
      match XR.get_att_cdata atts ("", Stog_tags.doc_path) with
        None -> Stog_types.doc stog doc_id
      | Some path ->
          let (_, doc) = Stog_types.doc_by_path
            stog (Stog_path.of_string path)
          in
          doc
    in
    f doc stog env ?loc atts subs
  in
  let (previous, next) =
    let html_link stog doc =
      let href = Stog_engine.doc_url stog doc in
      [ XR.node ("", "a")
         ~atts: (XR.atts_one ("","href") [XR.cdata (Stog_url.to_string href)])
         (XR.from_string doc.doc_title)
      ]
    in
    let try_link key search stog _ ?loc _ _ =
      let fallback () =
        match search stog doc_id with
        | None -> []
        | Some id -> html_link stog (Stog_types.doc stog id)
      in
      let xmls =
        match Stog_types.get_def doc.doc_defs key with
          None -> fallback ()
        | Some (_,body) ->
            let path = Stog_path.of_string (XR.to_string body) in
            try
              let (_, doc) = Stog_types.doc_by_path stog path in
              html_link stog doc
            with Failure s ->
                Stog_msg.warning s;
                fallback ()
      in
      (stog, xmls)
    in
    (try_link ("","previous") Stog_info.pred_by_date,
     try_link ("","next") Stog_info.succ_by_date)
  in
  let l =
    !plugin_base_rules @
    [
      ("", Stog_tags.archive_tree), fun_archive_tree ;
      ("", Stog_tags.as_cdata), fun_as_cdata ;
      ("", Stog_tags.as_xml), fun_as_xml ;
      ("", Stog_tags.command_line), fun_command_line ~inline: false ;
      ("", Stog_tags.date_now), fun_date_now ;
      ("", Stog_tags.date_today), fun_date_today ;
      ("", Stog_tags.dummy_), fun_dummy_ ;
      ("", Stog_tags.documents), (fun acc -> doc_list doc acc) ;
      ("", Stog_tags.doc_body), mk f_body ;
      ("", Stog_tags.doc_date), fun_date ;
      ("", Stog_tags.doc_datetime), fun_datetime ;
      ("", Stog_tags.doc_intro), mk f_intro ;
      ("", Stog_tags.doc_keywords), mk html_of_keywords ;
      ("", Stog_tags.doc_navpath), mk fun_doc_navpath ;
      ("", Stog_tags.print_date), fun_print_date ;
      ("", Stog_tags.print_datetime), fun_print_datetime ;
      ("", Stog_tags.doc_src), mk f_src ;
      ("", Stog_tags.doc_title), mk f_title ;
      ("", Stog_tags.doc_topics), mk html_of_topics ;
      ("", Stog_tags.doc_type), mk f_type ;
      ("", Stog_tags.doc_url), mk f_url ;
      ("", Stog_tags.exec), Stog_exec.fun_exec  ;
      ("", Stog_tags.ext_a), fun_exta ;
      ("", Stog_tags.error_), fun_error_ ;
      ("", Stog_tags.graph), fun_graph ;
      ("", Stog_tags.hcode), fun_hcode ~inline: false ?lang: None;
      ("", Stog_tags.icode), fun_icode ?lang: None ;
      ("", Stog_tags.if_), fun_if ;
      ("", Stog_tags.image), fun_image ;
      ("", Stog_tags.include_), mk fun_include ;
      ("", Stog_tags.latex), Stog_latex.fun_latex ;
      ("", Stog_tags.latex_body), Stog_latex.fun_latex_body ;
      ("", Stog_tags.list), fun_list ;
      ("", Stog_tags.n_columns), fun_ncolumns ;
      ("", Stog_tags.next), next;
      ("", Stog_tags.ocaml), fun_ocaml ~inline: false ;
      ("", Stog_tags.ocaml_eval), Stog_ocaml.fun_eval  ;
      ("", Stog_tags.ocaml_printf), Stog_ocaml.fun_printf  ;
      ("", Stog_tags.prefix_svg_ids), Stog_svg.fun_prefix_svg_ids ;
      ("", Stog_tags.previous), previous;
      ("", Stog_tags.two_columns), fun_twocolumns ;
    ]
  in
  l

and doc_list doc ?rss ?set stog env ?loc args _ =
  let (stog, docs) = Stog_list.docs_of_args ?set stog env args in
  (* the document depends on the listed documents *)
  let stog = List.fold_left
    (fun stog (doc2_id, doc2) ->
       Stog_deps.add_dep stog doc (Stog_types.Doc doc2)
    )
    stog docs
  in
  let (stog, tmpl) =
    let file =
      match XR.get_att_cdata args ("", "tmpl") with
        None ->  "doc-in-list.tmpl"
      | Some s -> s
    in
    Stog_tmpl.get_template stog ~doc Stog_tmpl.doc_in_list file
  in
  let f_doc tmpl (stog, acc) (doc_id, doc) =
    let (stog, env) = Stog_engine.doc_env stog env stog doc in
    let rules = build_base_rules stog doc_id in
    let env = XR.env_of_list ~env rules in
    let (stog, xmls) = XR.apply_to_xmls stog env tmpl in
    (stog, (List.rev xmls) @ acc)
  in
  let (stog, xmls) = List.fold_left (f_doc tmpl) (stog, []) docs in
  let xmls = List.rev xmls in
  (*prerr_endline "doc_list:";
  List.iter
    (fun xml -> prerr_endline (Printf.sprintf "ITEM: %s" (XR.string_of_xml xml)))
    xml;
     *)
  let (stog, alt_link) =
    match rss with
      Some link -> (stog, Some link)
    | None ->
        let alt_doc_path =
          match XR.get_att_cdata args ("", "rss") with
            Some path -> Some path
          | None ->
              match XR.get_att_cdata args ("", "alt-doc-path") with
                Some path -> Some path
              | None -> None
        in
        match alt_doc_path with
          None -> (stog, None)
        | Some path ->
            let alt_doc_type = XR.opt_att_cdata ~def: "rss" args ("","alt-doc-type") in
            let alt_doc_in_list_tmpl =
              XR.opt_att_cdata ~def: "rss-item.tmpl" args ("","alt-doc-in-list-tmpl")
            in
            let doc_path =
              if Filename.is_relative path then
                Stog_path.append (Stog_path.parent doc.doc_path)
                  (Stog_path.of_string path).Stog_path.path
              else
                (Stog_path.of_string path)
            in
            let doc_title =
              match XR.get_att_cdata args ("", "alt-doc-title") with
                None -> doc.doc_title
              | Some t -> t
            in
            let (stog, tmpl) =
              Stog_tmpl.get_template stog ~doc Stog_tmpl.rss_item
                alt_doc_in_list_tmpl
            in
            let (stog, xmls) = List.fold_left (f_doc tmpl) (stog, []) docs in
            let doc_body = List.rev xmls in
            let doc = { doc with
                doc_path ; doc_parent = Some doc.doc_path ;
                doc_children = [] ; doc_type = alt_doc_type ;
                doc_body ; doc_title ; doc_sets = [] ;
                doc_out = None ;
              }
            in
            let stog =
              try
                let (doc_id, _) = Stog_types.doc_by_path stog doc.doc_path in
                Stog_types.set_doc stog doc_id doc
              with _ ->
                  Stog_types.add_doc stog doc
            in
            let url = Stog_engine.doc_url stog doc in
            (stog, Some url)
  in
  let xml =
    match alt_link with
      None -> xmls
    | Some link ->
        (XR.node ("", "div")
         ~atts: (XR.atts_one ("", "class") [XR.cdata "rss-button"])
           [
             XR.node ("", "a")
               ~atts: (XR.atts_one ("", "href") [XR.cdata (Stog_url.to_string link)])
               [
                 XR.node ("", "img")
                   ~atts: (XR.atts_of_list
                    [ ("", "src"), [XR.cdata "rss.png"] ;
                      ("", "alt"), [XR.cdata "Rss feed"]])
                   []
               ]
           ]
        ) :: xmls
  in
  (stog, xml)
;;

let make_by_word_indexes stog env f_doc_path doc_type map =
  let f word set stog =
    let path = f_doc_path word in
    try
      ignore(Stog_types.doc_by_path stog path);
      stog
    with Failure _ ->
        let doc =
          { Stog_types.doc_path = path ;
            doc_parent = None ;
            doc_children = [] ;
            doc_type = doc_type ;
            doc_prolog = None ;
            doc_body = [] ;
            doc_date = None ;
            doc_title = word ;
            doc_keywords = [] ;
            doc_topics = [] ;
            doc_defs = [] ;
            doc_src = Stog_path.to_string path ;
            doc_sets = [] ;
            doc_lang_dep = true ;
            doc_out = None ;
            doc_used_mods = Stog_types.Str_set.empty ;
          }
        in
        let rss_path =
          let s = Stog_path.to_string doc.doc_path in
          (try Filename.chop_extension s with _ -> s)^".rss"
        in
        (* we must register the document before evaluating
          the doc_list, because when we add depencies from the
          alternative document to the document containing the list,
          this latter document must exist *)
        let stog = Stog_types.add_doc stog doc in
        let (doc_id, _) = Stog_types.doc_by_path stog doc.doc_path in
        let atts = XR.atts_one ("","rss") [XR.cdata rss_path] in
        let (stog, body) = doc_list doc ~set (*~rss: rss_url*) stog env atts [] in
        let doc = { doc with Stog_types.doc_body = body } in
        Stog_types.set_doc stog doc_id doc
  in
  Stog_types.Str_map.fold f map stog
;;


let make_topic_indexes stog env =
  Stog_msg.verbose ~level: 2 "creating by-topic index documents";
  make_by_word_indexes stog env topic_index_path
  "by-topic" stog.stog_docs_by_topic

;;

let make_keyword_indexes stog env =
  Stog_msg.verbose ~level: 2 "creating by-keyword index documents";
  make_by_word_indexes stog env keyword_index_path
  "by-keyword" stog.stog_docs_by_kw
;;

let make_archive_indexes stog env =
  Stog_msg.verbose ~level: 2 "creating archive documents";
  let f_month year month set stog =
    let path = month_index_path ~year ~month in
    try
      ignore(Stog_types.doc_by_path stog path);
      stog
    with Failure _ ->
        Stog_msg.verbose ~level: 3
          ("Creating document "^(Stog_path.to_string path));
        let title =
          let month_str = Stog_intl.get_month stog.stog_lang month in
          Printf.sprintf "%s %d" month_str year
        in
        let doc =
          { Stog_types.doc_path = path ;
            doc_parent = None ;
            doc_children = [] ;
            doc_type = "by-month";
            doc_prolog = None ;
            doc_body = [] ;
            doc_date = None ;
            doc_title = title ;
            doc_keywords = [] ;
            doc_topics = [] ;
            doc_defs = [] ;
            doc_src = Stog_path.to_string path ;
            doc_sets = [] ;
            doc_lang_dep = true ;
            doc_out = None ;
            doc_used_mods = Stog_types.Str_set.empty ;
          }
        in
        (* we must register the document before evaluating
          the doc_list, because when we add depencies from the
          alternative document to the document containing the list,
          this latter document must exist *)
        let stog = Stog_types.add_doc stog doc in
        let (doc_id, _) = Stog_types.doc_by_path stog doc.doc_path in
        let (stog, body) = doc_list doc ~set stog env XR.atts_empty [] in
        let doc = { doc with doc_body = body } in
        Stog_types.set_doc stog doc_id doc
  in
  let f_year year mmap stog =
    Stog_types.Int_map.fold (f_month year) mmap stog
  in
  Stog_types.Int_map.fold f_year stog.stog_archives stog
;;

let add_docs env stog _ =
  let stog = make_archive_indexes stog env in
  let stog = make_keyword_indexes stog env in
  let stog = make_topic_indexes stog env in
  stog
;;

module Intmap = Stog_types.Int_map

let fun_level_base = Stog_engine.fun_apply_stog_doc_rules build_base_rules;;

let get_sectionning_tags stog doc =
  let spec =
    match Stog_types.get_def doc.doc_defs ("", "sectionning") with
      Some x -> Some x
    | None ->
        Stog_types.get_def stog.stog_defs ("", "sectionning")
  in
  match spec with
    None -> List.map (fun t -> ("",t)) Stog_tags.default_sectionning
  | Some (_,xmls) ->
      let s = XR.to_string xmls in
      let l = Stog_misc.split_string s [',' ; ';'] in
      let strip = Stog_misc.strip_string in
      List.fold_right
        (fun s acc ->
           match Stog_misc.split_string s [':'] with
             [] -> acc
           | [s] -> ("", strip s) :: acc
           | [pref ; s] -> (strip pref, strip s) :: acc
           | pref :: q -> (strip pref, strip (String.concat ":" q)) :: acc
        )
        l []
;;

let rules_toc stog doc_id =
  let doc = Stog_types.doc stog doc_id in
  let tags = get_sectionning_tags stog doc in
  [ ("", Stog_tags.prepare_toc), (fun_prepare_toc tags);
    ("", Stog_tags.toc), fun_toc ;
  ]
;;

let fun_level_toc = Stog_engine.fun_apply_stog_doc_rules rules_toc ;;

let rules_inc_doc stog doc_id =
  let doc = Stog_types.doc stog doc_id in
  [ ("", Stog_tags.inc), fun_inc doc ;
    ("", Stog_tags.late_inc), fun_late_inc doc ;
    ("", Stog_tags.late_cdata), fun_as_cdata ;
  ]
;;
let fun_level_inc_doc =
  Stog_engine.fun_apply_stog_doc_rules rules_inc_doc ;;

let fun_level_clean =
  let f env stog docs =
    Stog_ocaml.close_sessions ();
    let env = XR.env_of_list ~env
      [ ("", Stog_tags.sep), (fun d _ ?loc _ _ -> (d, [])) ]
    in
    Stog_types.Doc_set.fold
      (fun doc_id stog -> Stog_engine.apply_stog_env_doc stog env doc_id)
      docs stog
  in
  Stog_engine.Fun_stog f
;;


let level_funs = [
    "base", fun_level_base ;
    "toc", fun_level_toc ;
    "cut", Stog_engine.Fun_stog Stog_cut.cut_docs ;
    "inc", fun_level_inc_doc ;
    "clean", fun_level_clean ;
    "add-docs", (Stog_engine.Fun_stog add_docs) ;
  ]

let default_levels =
  List.fold_left
    (fun map (name, levels) -> Stog_types.Str_map.add name levels map)
    Stog_types.Str_map.empty
    [ "base", [ 0 ; 61 ] ;
      "add-docs", [ 30 ] ;
      "toc", [ 50 ] ;
      "cut", [ 60 ] ;
      "inc", [ 160 ] ;
      "clean", [ 500 ] ;
    ]
;;

let mk_levels modname level_funs default_levels =
  fun ?(levels=[]) () ->
    let levels =
      List.fold_left
        (fun map (name, levels) ->
           Stog_types.Str_map.add name levels map
        )
        default_levels
        levels
    in
    let f name levels map =
      let level_fun =
        try List.assoc name level_funs
        with Not_found ->
            let msg = Printf.sprintf
              "Level function %S unknown in module %S" name modname
            in
            failwith msg
      in
      List.fold_left
        (fun map level -> Intmap.add level level_fun map)
        map levels
    in
    Stog_types.Str_map.fold f levels Intmap.empty
;;

let module_name = "base";;

let make_module ?levels () =
  let levels = mk_levels module_name level_funs default_levels ?levels () in
  let module M =
  struct
    type data = unit
    let modul = {
        Stog_engine.mod_name = module_name ;
        mod_levels = levels ;
        mod_data = () ;
       }

    type cache_data = unit

    let cache_load _ data doc t = ()
    let cache_store _ data doc = ()
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
