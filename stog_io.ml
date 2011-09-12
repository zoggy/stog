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
      { art_human_id = Filename.basename dir ;
        art_kind = kind ;
        art_body = "" ;
        art_date = (01, 01, 2011) ;
        art_keywords = [] ;
        art_topics = [] ;
        art_title = ("title for "^(Filename.basename dir)) ;
        art_location = dir ;
        art_files = [] ;
        art_comments = [] ;
      }
;;


let add_article stog art =
  let (id, articles) = Stog_tmap.add stog.stog_articles art in
  let map = Stog_types.Str_map.add
    art.art_human_id
    art
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
let read_stog dir =
  let stog = Stog_types.create_stog () in
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
