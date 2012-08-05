(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012 Maxence Guesdon. All rights reserved.                   *)
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
  try Scanf.sscanf s "%d/%d/%d" (fun year month day -> {day; month; year})
  with
  | Scanf.Scan_failure _ -> failwith ("Invalid date: "^s)
  | End_of_file -> failwith (Printf.sprintf "Incomplete date \"%s\"" s)
;;

let topics_of_string s =
  List.map Stog_misc.strip_string
    (Stog_misc.split_string s [','; ';'])
;;
let keywords_of_string = topics_of_string ;;
let streams_of_string s =
  let l = topics_of_string s in
  List.map (fun s -> Stog_misc.split_string s ['/']) l
;;

let bool_of_string s =
  match String.lowercase s with
    "0" | "false" -> false
  | _ -> true
;;

let node_details = function
  Xtmpl.D _  -> None
| Xtmpl.T (tag, atts, subs) -> Some (tag, atts, subs)
| Xtmpl.E (((_,tag),atts), subs) ->
    let atts = List.fold_left
      (fun acc ((s,att), v) ->
         match s with "" -> (att,v) :: acc | _ -> acc) [] atts
    in
    Some (tag, atts, subs)
;;

let fill_elt_from_atts =
  let rec iter elt = function
    [] -> elt
  | h :: q ->
      let elt =
        match h with
        | ("with-contents",_) -> elt
        | ("title", s) -> { elt with elt_title = s }
        | ("keywords", s) -> { elt with elt_keywords = keywords_of_string s }
        | ("topics", s) -> { elt with elt_topics = topics_of_string s }
        | ("date", s) -> { elt with elt_date = Some (date_of_string s) }
        | ("published", s) -> { elt with elt_published = bool_of_string s }
        | ("streams", s) -> { elt with elt_streams = streams_of_string s }
        | (v, s) -> { elt with elt_vars = (v, s) :: elt.elt_vars }
      in
      iter elt q
  in
  iter
;;

let fill_elt_from_nodes =
  let f elt xml =
    match node_details xml with
      None -> elt
    | Some (tag, atts, subs) ->
        match tag with
        | "contents" -> { elt with elt_body = Xml subs }
        | _ -> failwith "not implemented"
  in
  List.fold_left f
;;

let elt_of_file file =
  let xml = Xtmpl.xml_of_string (Stog_misc.string_of_file file) in
  let (typ, atts, subs) =
    match node_details xml with
      None -> failwith (Printf.sprintf "File %S does not content an XML tree" file)
    | Some (tag, atts, subs) -> (tag, atts, subs)
  in
  let elt = Stog_types.make_elt ~typ () in
  let elt = { elt with elt_src = file } in
  let elt = fill_elt_from_atts elt atts in
  match Xtmpl.get_arg atts "with-contents" with
    Some s when bool_of_string s ->
      (* arguments are also passed in sub nodes, and contents is in
         subnode "contents" *)
      elt
  | _ ->
      (* all arguments are passed in attributes, subnodes are the contents *)
      elt
;;

(*
let re_ignore =
  let comment = "^[ \t]*#.*$" in
  let empty = "^[ \t]*$" in
  Str.regexp (comment ^ "\\|" ^ empty)
let re_field = Str.regexp "^\\([^=]+\\)=\\([^\n]*\\)";;
let separator = "<->"
(* matches the separator only when it's alone on its line *)
let re_separator = Str.regexp ("^" ^ Str.quote separator ^ "$")

let read_article_header art header =
  let lines = Stog_misc.split_string header ['\n'] in
  let f art line =
    if Str.string_match re_ignore line 0 then art
    else try
      ignore(Str.string_match re_field line 0);
      let field = Str.matched_group 1 line in
      let value = Str.matched_group 2 line in
      let field = Stog_misc.strip_string field in
      let value = Stog_misc.strip_string value in
      match String.lowercase field with
        s when s = Stog_cst.article_date -> { art with art_date = date_of_string value }
      | s when s = Stog_cst.article_title -> { art with art_title = value }
      | s when s = Stog_cst.topics -> { art with art_topics = topics_of_string value }
      | s when s = Stog_cst.keywords -> { art with art_keywords = keywords_of_string value }
      | s when s = Stog_cst.article_published -> { art with art_published = bool_of_string value }
      | other -> { art with art_vars = (other, value) :: art.art_vars }
    with
      Not_found ->
        Stog_msg.warning
          (Printf.sprintf "Line not handled in %s: %s"
           art.art_human_id line);
          art
    | Invalid_argument s ->
        Stog_msg.warning
        (Printf.sprintf "Invalid_argument(%s): %s" s line);
        art
  in
  List.fold_left f art lines
;;

let read_article_main art ?loc contents =
  let loc = match loc with None -> "..." | Some loc -> loc in
  let p_sep =
    try Str.search_forward re_separator contents 0
    with Not_found ->
      (* to avoid confusions and ambiguities, we ask that the <->
         separator be alone on its line; if that fails, we check if
         there is some separator as part of a line, to give a clearer
         error message. *)
      try
        let fuzzy_sep = Str.regexp_string separator in
        ignore (Str.search_forward fuzzy_sep contents 0);
        failwith ("the <-> separator is not alone on its line in "^loc)
      with Not_found ->
        failwith ("no <-> separator in "^loc)
  in
  let p = p_sep + String.length separator in
  let body =
    Stog_misc.strip_string
    (String.sub contents p (String.length contents - p))
  in
  let art = { art with art_body = body } in
  let header = String.sub contents 0 p_sep in
  read_article_header art header
;;

let read_article_main_of_file art file =
  let contents = Stog_misc.string_of_file file in
  read_article_main art ~loc: file contents
;;

let get_article_files dir =
  let files = Stog_find.find_list
    (Stog_find.Ignore)
    [dir]
    [ Stog_find.Maxdepth 1 ;
      Stog_find.Type Unix.S_REG ;
    ]
  in
  files
;;

let read_article_comments dir =
  let messages = Stog_mailparse.messages_from_dir
    (Filename.concat dir "comments")
  in
  (*prerr_endline (Printf.sprintf "%s: %d messages" dir (List.length messages));*)
  Stog_mailparse.build_message_tree messages
;;

let read_article dir =
  let file_opt =
    first_that_exists [
      Filename.concat dir "index.html", Html ;
      Filename.concat dir "index.txt", Text ;
    ]
  in
  match file_opt with
    None -> None
  | Some (file, kind) ->
      let art_files = get_article_files dir in
      let art_files = List.filter ((<>) file) art_files in
      let comments = read_article_comments  dir in
      (*prerr_endline (Printf.sprintf "%s: %d messages" dir (List.length comments));*)
      let default_date = { day = 01; month = 01; year = 2011; } in
      let a =
        { art_human_id = Filename.basename dir ;
          art_kind = kind ;
          art_body = "" ;
          art_date = default_date ;
          art_keywords = [] ;
          art_topics = [] ;
          art_title = ("title for "^(Filename.basename dir)) ;
          art_published = true ;
          art_location = file ;
          art_files = art_files ;
          art_comments = comments ;
          art_vars = [] ;
        }
      in
      Some (read_article_main_of_file a file)
;;

let ignore_dot_entries =
  List.filter
  (fun s ->
    let f = Filename.basename s in
    String.length f > 0 && f.[0] <> '.')
;;

let read_stog_header stog header =
  let lines = Stog_misc.split_string header ['\n'] in
  let f stog line =
    try
      ignore(Str.string_match re_field line 0);
      let field = Str.matched_group 1 line in
      let value = Str.matched_group 2 line in
      let field = Stog_misc.strip_string field in
      let value = Stog_misc.strip_string value in
      match String.lowercase field with
      | s when s = Stog_cst.site_title -> { stog with stog_title = value }
      | s when s = Stog_cst.site_desc -> { stog with stog_desc = value }
      | s when s = Stog_cst.site_email -> { stog with stog_email = value }
      | s when s = Stog_cst.site_url -> { stog with stog_base_url = value }
      | s when s = Stog_cst.rss_length -> { stog with stog_rss_length = int_of_string value }
      | field -> { stog with stog_vars = (field, value) :: stog.stog_vars }
    with
      Not_found ->
        Stog_msg.warning
        (Printf.sprintf "Line not handled in stog index: %s"
         line);
        stog
    | Invalid_argument s ->
        Stog_msg.warning
        (Printf.sprintf "Invalid_argument(%s): %s" s line);
        stog
  in
  List.fold_left f stog lines
;;

let read_page_header page header =
  let lines = Stog_misc.split_string header ['\n'] in
  let f page line =
    try
      ignore(Str.string_match re_field line 0);
      let field = Str.matched_group 1 line in
      let value = Str.matched_group 2 line in
      let field = Stog_misc.strip_string field in
      let value = Stog_misc.strip_string value in
      match String.lowercase field with
      | s when s = Stog_cst.page_title -> { page with page_title = value }
      | field -> { page with page_vars = (field, value) :: page.page_vars }
    with
      Not_found ->
        Stog_msg.warning
        (Printf.sprintf "Line not handled in page: %s"
         line);
        page
    | Invalid_argument s ->
        Stog_msg.warning
        (Printf.sprintf "Invalid_argument(%s): %s" s line);
        page
  in
  List.fold_left f page lines
;;

let read_page_main page ?loc contents =
 let loc = match loc with None -> "..." | Some loc -> loc in
  let p_sep =
    try Str.search_forward re_separator contents 0
    with Not_found -> failwith ("no <-> separator in "^loc)
  in
  let p = p_sep + String.length separator in
  let body =
    Stog_misc.strip_string
    (String.sub contents p (String.length contents - p))
  in
  let page = { page with page_body = body } in
  let header = String.sub contents 0 p_sep in
  read_page_header page header
;;

let read_page stog file =
  let hid = Filename.chop_extension (Filename.basename file) in
  let page =
    { page_human_id = hid ;
      page_kind = Html ;
      page_body = "" ;
      page_title  = "" ;
      page_vars = [] ;
    }
  in
  let contents = Stog_misc.string_of_file file in
  let page = read_page_main page ~loc: file contents in
  Stog_types.add_page stog page
;;

let read_stog_pages stog dir =
  let pred_name name =
    name <> "index.html" && Filename.check_suffix name ".html"
  in
  let on_error (e,s1,s2) =
    let msg =  Printf.sprintf "%s: %s %s"
      (Unix.error_message e) s1 s2
    in
    Stog_msg.error msg
  in
  let page_files = Stog_find.find_list
    (Stog_find.Custom on_error)
    [dir]
    [ Stog_find.Predicate pred_name ;
      Stog_find.Maxdepth 1 ;
      Stog_find.Type Unix.S_REG ;
    ]
  in
  List.fold_left read_page stog page_files
;;

let read_stog_index stog dir =
  let file_opt =
    first_that_exists [
      Filename.concat dir "index.html", Html ;
      Filename.concat dir "index.txt", Text ;
    ]
  in
  match file_opt with
    None -> failwith ("No index file in "^dir)
  | Some (file, kind) ->
      let contents = Stog_misc.string_of_file file in
      let p_sep =
        try Str.search_forward re_separator contents 0
        with Not_found -> (String.length contents) - 1
      in
      let p = p_sep + String.length separator in
      let body = String.sub contents p (String.length contents - p) in
      let stog = { stog with stog_body = body } in
      let header = String.sub contents 0 p_sep in
      read_stog_header stog header
;;

let read_stog dir =
  let stog = Stog_types.create_stog dir in
  let stog = read_stog_index stog dir in
  let on_error (e,s1,s2) =
    let msg =  Printf.sprintf "%s: %s %s"
      (Unix.error_message e) s1 s2
    in
    Stog_msg.error msg
  in
  let dirs = Stog_find.find_list
    (Stog_find.Custom on_error)
    [dir]
    [ Stog_find.Maxdepth 1 ;
      Stog_find.Type Unix.S_DIR ;
    ]
  in
  let dirs = List.filter ((<>) dir) dirs in
  let dirs = ignore_dot_entries dirs in

  (*List.iter prerr_endline dirs;*)
  let stog =
    List.fold_left
    (fun stog dir ->
       match read_article dir with
         None -> stog
       | Some art -> add_article stog art
    )
    stog
    dirs
  in
  let stog = read_stog_pages stog dir in
  Stog_msg.verbose ~level: 2
    (Printf.sprintf "Pages: %s"
      (String.concat ", "
        (Stog_types.Str_map.fold (fun id _ acc -> id :: acc) stog.stog_page_by_human_id [])));
  stog
;;

let write_stog_article stog _ art =
  let dir = Filename.concat stog.stog_dir art.art_human_id in
  Stog_misc.mkdir dir;
  let file = Filename.concat dir "index.html" in
  let list l = String.concat ", " l in
  let contents =
    Printf.sprintf
    "title: %s\ndate: %s\ntopics: %s\nkeywords: %s\npublished: %s\n<->\n%s"
    art.art_title
    (Stog_intl.short_string_of_date art.art_date)
    (list art.art_topics)
    (list art.art_keywords)
    (if art.art_published then "true" else "false")
    art.art_body
  in
  Stog_misc.file_of_string ~file contents
;;

let write_stog_index stog =
  let file = Filename.concat stog.stog_dir "index.html" in
  let contents = Printf.sprintf
    "title: %s\ndescription: %s\nemail: %s\nurl: %s\nrss-length: %d\n<->\n%s"
    stog.stog_title
    stog.stog_desc
    stog.stog_email
    stog.stog_base_url
    stog.stog_rss_length
    stog.stog_body
  in
  Stog_misc.file_of_string ~file contents
;;

let write_stog stog =
  Stog_misc.mkdir stog.stog_dir;
  write_stog_index stog;
  Stog_tmap.iter (write_stog_article stog) stog.stog_articles
;;

*)