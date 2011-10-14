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

let re_field = Str.regexp "^\\([^:]+\\):\\([^\n]*\\)";;
let separator = "<->";;
let re_separator = Str.regexp_string separator;;

let date_of_string s =
  try Scanf.sscanf s "%d/%d/%d" (fun d m y -> (d, m, y))
  with
  | Scanf.Scan_failure _ -> failwith ("Invalid date: "^s)
  | End_of_file -> failwith (Printf.sprintf "Incomplete date \"%s\"" s)
;;

let topics_of_string s =
  List.map Stog_misc.strip_string
    (Stog_misc.split_string s [','; ';'])
;;
let keywords_of_string = topics_of_string ;;
let bool_of_string s =
  match String.lowercase s with
    "0" | "false" -> false
  | _ -> true
;;

let read_article_header art header =
  let lines = Stog_misc.split_string header ['\n'] in
  let f art line =
    try
      ignore(Str.string_match re_field line 0);
      let field = Str.matched_group 1 line in
      let value = Str.matched_group 2 line in
      let field = Stog_misc.strip_string field in
      let value = Stog_misc.strip_string value in
      match String.lowercase field with
        "date" -> { art with art_date = date_of_string value }
      | "title" -> { art with art_title = value }
      | "topics" -> { art with art_topics = topics_of_string value }
      | "keywords" -> { art with art_keywords = keywords_of_string value }
      | "published" -> { art with art_published = bool_of_string value }
      | _ -> art
    with
      Not_found ->
        prerr_endline
        (Printf.sprintf "Line not handled in %s: %s"
         art.art_human_id line);
        art
    | Invalid_argument s ->
        prerr_endline
        (Printf.sprintf "Invalid_argument(%s): %s" s line);
        art
  in
  List.fold_left f art lines
;;

let read_article_main art ?loc contents =
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
  prerr_endline (Printf.sprintf "%s: %d messages" dir (List.length messages));
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
    None -> failwith ("No index file in "^dir)
  | Some (file, kind) ->
      let art_files = get_article_files dir in
      let art_files = List.filter ((<>) file) art_files in
      let comments = read_article_comments  dir in
      prerr_endline (Printf.sprintf "%s: %d messages" dir (List.length comments));
      let a =
        { art_human_id = Filename.basename dir ;
          art_kind = kind ;
          art_body = "" ;
          art_date = (01, 01, 2011) ;
          art_keywords = [] ;
          art_topics = [] ;
          art_title = ("title for "^(Filename.basename dir)) ;
          art_published = true ;
          art_location = file ;
          art_files = art_files ;
          art_comments = comments ;
        }
      in
      read_article_main_of_file a file
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
      | "title" -> { stog with stog_title = value }
      | "description" -> { stog with stog_desc = value }
      | "email" -> { stog with stog_email = value }
      | "url" -> { stog with stog_base_url = value }
      | "rss-length" -> { stog with stog_rss_length = int_of_string value }
      | _ -> stog
    with
      Not_found ->
        prerr_endline
        (Printf.sprintf "Line not handled in stog index: %s"
         line);
        stog
    | Invalid_argument s ->
        prerr_endline
        (Printf.sprintf "Invalid_argument(%s): %s" s line);
        stog
  in
  List.fold_left f stog lines
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
    prerr_endline msg
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

  List.iter prerr_endline dirs;
  List.fold_left
  (fun stog dir ->
     add_article stog (read_article dir))
  stog
  dirs
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
    (let (y, m, d) = art.art_date in Printf.sprintf "%04d/%02d/%02d" y m d)
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
  