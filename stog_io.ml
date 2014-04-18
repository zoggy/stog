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

open Stog_types;;


let first_that_exists =
  let rec iter = function
    [] -> None
  | (h,data) :: q ->
      if Sys.file_exists h then
        Some (h, data)
      else iter q
  in
  iter
;;

let date_of_string s =
  try Netdate.parse s
  with _ -> failwith ("Invalid date: "^s)
;;
(*
  try Scanf.sscanf s "%d/%d/%d" (fun year month day -> {day; month; year})
  with
  | Scanf.Scan_failure _ -> failwith ("Invalid date: "^s)
  | End_of_file -> failwith (Printf.sprintf "Incomplete date \"%s\"" s)
;;
*)

let topics_of_string s =
  List.map Stog_misc.strip_string
    (Stog_misc.split_string s [','; ';'])
;;
let keywords_of_string = topics_of_string ;;
let sets_of_string = topics_of_string ;;

let bool_of_string s =
  match String.lowercase s with
    "0" | "false" -> false
  | _ -> true
;;

let module_defs_of_xml =
  let f acc xml =
    match xml with
      Xtmpl.D _ -> acc
    | Xtmpl.E (tag, atts, subs) ->
        (tag, atts, subs) :: acc
  in
  List.fold_left f []
;;

let module_requires_of_string str =
  let l = Stog_misc.split_string str [',' ; ';'] in
  let l = List.map Stog_misc.strip_string l in
  let f acc = function
    "" -> acc
  | s -> Stog_types.Str_set.add s acc
  in
  List.fold_left f Stog_types.Str_set.empty l
;;

let read_module stog file =
  let modname = Filename.chop_extension (Filename.basename file) in
  let xml = Xtmpl.xml_of_file file in
  match xml with
    Xtmpl.D _ -> assert false
  | Xtmpl.E (tag, atts, subs) ->
      let mod_requires =
        match Xtmpl.get_arg_cdata atts ("","requires") with
          None -> Stog_types.Str_set.empty
        | Some s -> module_requires_of_string s
      in
      let mod_defs = module_defs_of_xml subs in
      let m = { mod_requires ; mod_defs } in
      let modules = Stog_types.Str_map.add modname m stog.stog_modules in
      { stog with stog_modules = modules }

let read_modules stog =
  let f_dir stog mod_dir =
    Stog_msg.verbose ~level: 2 ("reading module directory "^mod_dir);
    if Sys.file_exists mod_dir then
      (
       let files = Stog_find.find_list
         Stog_find.Stderr
           [mod_dir]
           [ Stog_find.Type Unix.S_REG ; Stog_find.Follow ;
             Stog_find.Regexp (Str.regexp ".*\\.stm$")
           ]
       in
       List.fold_left read_module stog files
      )
    else
      stog
  in
  (* read module directories starting from the
    last one, so that modules defined ahead in the list
    will hide the lastly defined. *)
  List.fold_left f_dir stog (List.rev stog.stog_mod_dirs)
;;

let used_mods_of_string set s =
  let l = Stog_misc.split_string s [';' ; ','] in
  List.fold_left
    (fun set s ->
     match Stog_misc.strip_string s with
       "" -> set
     | modname -> Stog_types.Str_set.add modname set
     )
     set
     l
;;

let extract_stog_info_from_doc stog doc =
  let stog = { stog with stog_title = doc.doc_title } in
  let rec iter (stog, defs) = function
    [] -> (stog, defs)
  | h:: q ->
      let (stog, opt) =
        match h with
        | (("stog", "site-description"), _, xmls) -> { stog with stog_desc = xmls }, None
        | (("stog", "site-url"), _, xmls) ->
            { stog with stog_base_url = Stog_types.url_of_string (Xtmpl.string_of_xmls xmls) }, None
        | (("stog", "site-email)"), _, xmls) -> { stog with stog_email = Xtmpl.string_of_xmls xmls }, None
        | (("stog", "rss-length"), _,xmls) ->
            { stog with
              stog_rss_length = int_of_string (Xtmpl.string_of_xmls xmls) },
            None
        | (("stog", "use"), _, xmls) ->
            let s = Xtmpl.string_of_xmls xmls in
            let stog =  { stog with stog_used_mods = used_mods_of_string stog.stog_used_mods s } in
            (stog, None)
        | (("stog", name), args, body) ->
            let stog = { stog with stog_defs = (("",name), args, body) :: stog.stog_defs } in
            (stog, None)
        | _ ->
            (stog, Some h)
      in
      let defs = match opt with None -> defs | Some x -> h :: defs in
      iter (stog, defs) q
  in
  let (stog, defs) = iter (stog, []) doc.doc_defs in
  (stog, { doc with doc_defs = defs })
;;

let add_doc stog doc =
  let (stog, doc) =
    let is_main =
      try match List.find (fun (s,_,_) -> s = ("","main")) doc.doc_defs with
          (_,_,[Xtmpl.D s]) -> bool_of_string s
        | (_,_,xmls) ->
          prerr_endline (Printf.sprintf "doc %S: not main:\n%S" doc.doc_title (Xtmpl.string_of_xmls xmls));
          false
      with Not_found -> false
    in
    if is_main then
      begin
      match stog.stog_main_doc with
          Some id ->
            let doc2 = Stog_types.doc stog id in
            failwith
            (Printf.sprintf "%S: %S is already defined as main stog document"
             doc.doc_src doc2.doc_src)
        | None ->
            extract_stog_info_from_doc stog doc
      end
    else
      (stog, doc)
  in
  Stog_types.add_doc stog doc
;;



let fill_doc_from_atts =
  let to_s = function [Xtmpl.D s] -> s | _ -> "" in
  let f name v doc =
    match name with
    | ("","with-contents")-> doc
    | ("","title") -> { doc with doc_title = (to_s v) }
    | ("","keywords") -> { doc with doc_keywords = keywords_of_string (to_s v) }
    | ("","topics") -> { doc with doc_topics = topics_of_string (to_s v) }
    | ("","date") -> { doc with doc_date = Some (date_of_string (to_s v)) }
    | ("","sets") -> { doc with doc_sets = sets_of_string (to_s v) }
    | ("","language-dep") -> { doc with doc_lang_dep = bool_of_string (to_s v) }
    | ("","doctype") -> { doc with doc_xml_doctype = Some (to_s v) }
    | ("", "use") ->
        { doc with doc_used_mods = used_mods_of_string doc.doc_used_mods (to_s v) }
    | _ ->
        let defs = (name, Xtmpl.atts_empty, v) :: doc.doc_defs in
        { doc with doc_defs = defs  }
  in
  Xtmpl.Name_map.fold f
;;

let fill_doc_from_nodes =
  let f doc xml =
    match xml with
      Xtmpl.D _ -> doc
    | Xtmpl.E (tag, atts, subs) ->
        let v = Xtmpl.string_of_xmls subs in
        match tag with
        | ("", "contents") -> { doc with doc_body = subs }
        | ("", "title") -> { doc with doc_title = v }
        | ("", "keywords") -> { doc with doc_keywords = keywords_of_string v }
        | ("", "topics") -> { doc with doc_topics = topics_of_string v }
        | ("", "date") -> { doc with doc_date = Some (date_of_string v) }
        | ("", "sets") -> { doc with doc_sets = sets_of_string v }
        | ("", "language-dep") -> { doc with doc_lang_dep = bool_of_string v }
        | ("", "doctype") -> { doc with doc_xml_doctype = Some v }
        | ("", "use") -> { doc with doc_used_mods = used_mods_of_string doc.doc_used_mods v }
        | s -> { doc with doc_defs = (s, atts, subs) :: doc.doc_defs }
  in
  List.fold_left f
;;

let fill_doc_from_atts_and_subs doc atts subs =
  let doc =
    match Xtmpl.get_arg_cdata atts ("","path") with
      None -> doc
    | Some s -> { doc with doc_path = Stog_path.of_string s }
  in
  let doc = fill_doc_from_atts atts doc in
  match Xtmpl.get_arg_cdata atts ("", "with-contents") with
    Some s when bool_of_string s ->
      (* arguments are also passed in sub nodes, and contents is in
         subnode "contents" *)
      fill_doc_from_nodes doc subs
  | _ ->
      (* all arguments are passed in attributes, subnodes are the contents *)
      { doc with doc_body = subs }
;;

let doc_of_file stog file =
  let rel_file = Stog_misc.path_under ~parent: stog.stog_dir file in
  let path =
    let s = "/" ^ rel_file in
    Stog_path.of_string s
  in
  Stog_msg.verbose ~level: 3 (Printf.sprintf "reading document file %S" file);
  let xml = Xtmpl.xml_of_file file in
  let (typ, atts, subs) =
    match xml with
      Xtmpl.D _ -> failwith (Printf.sprintf "File %S does not content an XML tree" file)
    | Xtmpl.E ((_,tag), atts, subs) -> (tag, atts, subs)
  in
  let doc = Stog_types.make_doc ~path ~typ () in
  let doc = { doc with doc_src = rel_file } in
  fill_doc_from_atts_and_subs doc atts subs
;;

let read_files cfg stog dir =
  let stog_cfg_dir = Stog_config.config_dir dir in
  let on_error (e,s1,s2) =
    let msg =  Printf.sprintf "%s: %s %s" (Unix.error_message e) s1 s2 in
    Stog_msg.error ~info: "Stog_io.read_files" msg
  in
  let pred_ign =
    let make_pred re =
      let re = Str.regexp re in
      fun s -> Str.string_match re s 0
    in
    let preds = List.map make_pred cfg.Stog_config.ignored in
    fun entry ->
      entry <> stog_cfg_dir &&
      (let base = Filename.basename entry in base <> "." && base <> "..") &&
      (
       let k = (Unix.stat entry).Unix.st_kind in
       match k with
         Unix.S_REG | Unix.S_DIR -> not (List.exists (fun f -> f entry) preds)
       | _ -> false
      )
  in
  let pred_doc =
    let make_pred s_re =
      let re = Str.regexp s_re in
      fun s -> Str.string_match re s 0
    in
    let preds_ok = List.map make_pred cfg.Stog_config.documents in
    let preds_ko = List.map make_pred cfg.Stog_config.not_documents in
    fun entry ->
      let result =
        (List.exists (fun f -> f entry) preds_ok) &&
        not (List.exists (fun f -> f entry) preds_ko)
      in
      Stog_msg.verbose ~level:5
        (Printf.sprintf "File %S %s be processed."
           entry (if result then "will" else "will not"));
      result
  in
  let is_dir file = (Unix.stat file).Unix.st_kind = Unix.S_DIR in
  let rec iter stog dir =
    let entries =
      Stog_find.find_list
        (Stog_find.Custom on_error)
        [dir]
        ([ Stog_find.Maxdepth 1 ;
           Stog_find.Predicate pred_ign ;
         ] @
         (if cfg.Stog_config.follow_symlinks then [Stog_find.Follow] else [])
        )
    in
    let entries = List.filter ((<>) dir) entries in
    let (dirs, files) = List.partition is_dir entries in
    (*prerr_endline ("dirs=" ^ String.concat ", " dirs);*)
    let (doc_files, files) = List.partition pred_doc files in
    let files = List.map Filename.basename files in
    let files = List.fold_right Str_set.add files Str_set.empty in
    let docs = List.map (doc_of_file stog) doc_files in
    let stog = List.fold_left add_doc stog docs in
    let (stog, dirs) = List.fold_left
      (fun (stog, map) dir ->
        let base = Filename.basename dir in
        let (stog, tree) = iter stog dir in
        (stog, Str_map.add base tree map)
      )
      (stog, Str_map.empty)
      dirs
    in
    let tree = { files ; dirs } in
    (stog, tree)
  in
  let (stog, tree) = iter stog dir in
  { stog with stog_files = tree }
;;

let read_stog dir =
  let len_dir_sep = String.length Filename.dir_sep in
  let rec remove_ending_sep s =
    let len = String.length s in
    if len <= 0 then failwith (Printf.sprintf "Invalid directory %S" dir);
    if len <= len_dir_sep then
      s
    else
      (
       if String.sub s (len - len_dir_sep) len_dir_sep = Filename.dir_sep then
         remove_ending_sep (String.sub s 0 (len - len_dir_sep))
       else
         s
      )
  in
  let dir = remove_ending_sep dir in
  let stog = Stog_types.create_stog dir in
  let cfg = Stog_config.read_config dir in
  let stog = read_files cfg stog dir in
  let stog = read_modules stog in
  let stog =
    let levels = List.fold_left
      (fun map (name, levels) ->
         Stog_types.Str_map.add name levels map)
        stog.stog_levels cfg.Stog_config.levels
    in
    { stog with stog_levels = levels }
  in
  stog
;;

