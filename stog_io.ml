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
  with Scanf.Scan_failure _ -> failwith ("Invalid date: "^s)
;;

let topics_of_string s =
  List.map Stog_misc.strip_string
    (Stog_misc.split_string s [','; ';'])
;;
let keywords_of_string = topics_of_string ;;

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

let read_article_main art file =
  let contents = Stog_misc.string_of_file file in
  let p_sep =
    try Str.search_forward re_separator contents 0
    with Not_found -> failwith ("no <-> separator in "^file)
  in
  let p = p_sep + String.length separator in
  let body = String.sub contents p (String.length contents - p) in
  let art = { art with art_body = body } in
  let header = String.sub contents 0 p_sep in
  read_article_header art header
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
      let a =
        { art_human_id = Filename.basename dir ;
          art_kind = kind ;
          art_body = "" ;
          art_date = (01, 01, 2011) ;
          art_keywords = [] ;
          art_topics = [] ;
          art_title = ("title for "^(Filename.basename dir)) ;
          art_location = file ;
          art_files = art_files ;
          art_comments = [] ;
        }
      in
      read_article_main a file
;;


let add_article stog art =
  let (id, articles) = Stog_tmap.add stog.stog_articles art in
  let map = Stog_types.Str_map.add
    art.art_human_id
    id
    stog.stog_art_by_human_id
  in
  { stog with
    stog_articles = articles ;
    stog_art_by_human_id = map ;
  }
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
  let stog = Stog_types.create_stog () in
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

