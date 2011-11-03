(** *)

type contents_kind = Text | Html
type date = int * int * int (** day 1..31 * month 1..12 * year *)
type time = Stog_date.t ;;

type message = {
    mes_time : time ;
    mes_subject : string ;
    mes_from : string ;
    mes_to : string list ;
    mes_body : string ;
    mes_id : string;
  }

type 'a tree = 'a tree_node
and 'a tree_node = Node of 'a * 'a tree list

type article =
  {
    art_human_id : string;
    art_kind : contents_kind ;
    art_body : string ;
    art_date : date ;
    art_keywords : string list ;
    art_topics : string list ;
    art_published : bool ;
    art_title : string ;
    art_location : string ;
    art_files : string list ; (** list of files in [art_location] *)
    art_comments : message tree list ;
  }
and article_id = article Stog_tmap.key

let today () =
  let t = Unix.gmtime (Unix.time()) in
  (t.Unix.tm_year + 1900, t.Unix.tm_mon+1,  t.Unix.tm_mday)
;;

let dummy_article () =
  { art_human_id = "dummy" ;
    art_kind = Html ;
    art_body = "" ;
    art_date = today () ;
    art_keywords = [] ;
    art_topics = [] ;
    art_published = true ;
    art_title = "Dummy title";
    art_location = "/tmp" ;
    art_files = [] ;
    art_comments = [] ;
  }
;;

module Str_map = Map.Make (struct type t = string let compare = compare end);;
module Art_set = Set.Make (struct type t = article_id let compare = Stog_tmap.compare_key end);;
  module Int_map = Map.Make (struct type t = int let compare = compare end);;

type edge_type =
  Date
| Topic of string
| Keyword of string
| Ref
;;

module Graph = Stog_graph.Make_with_map
  (struct
     type t = article_id
     let compare = Stog_tmap.compare_key
   end
  )
  (struct type t = edge_type let compare = Pervasives.compare end);;

type stog = {
  stog_dir : string ;
  stog_articles : (article, article) Stog_tmap.t ;
  stog_art_by_human_id : article_id Str_map.t ;
  stog_tmpl_dir : string ;
  stog_title : string ;
  stog_body : string ;
  stog_desc : string ;
  stog_graph : Graph.t ;
  stog_arts_by_kw : Art_set.t Str_map.t ;
  stog_arts_by_topic : Art_set.t Str_map.t ;
  stog_archives : Art_set.t Int_map.t Int_map.t ; (* year -> month -> article set *)
  stog_base_url : string ;
  stog_email : string ;
  stog_rss_length : int ;
  }

let create_stog dir = {
  stog_dir = dir ;
  stog_articles = Stog_tmap.create (dummy_article ());
  stog_art_by_human_id = Str_map.empty ;
  stog_tmpl_dir = "tmpl" ;
  stog_title = "Blog title" ;
  stog_body = "" ;
  stog_desc = "" ;
  stog_graph = Graph.create () ;
  stog_arts_by_kw = Str_map.empty ;
  stog_arts_by_topic = Str_map.empty ;
  stog_archives = Int_map.empty ;
  stog_base_url = "http://yourblog.net" ;
  stog_email = "foo@bar.com" ;
  stog_rss_length = 10 ;
  }
;;

let article stog id = Stog_tmap.get stog.stog_articles id;;
let article_by_human_id stog h =
  let id = Str_map.find h stog.stog_art_by_human_id in
  (id, article stog id)
;;

let set_article stog id article =
  {  stog with
    stog_articles = Stog_tmap.modify stog.stog_articles id article }
;;

let add_article stog art =
  let (id, articles) = Stog_tmap.add stog.stog_articles art in
  let map = Str_map.add
    art.art_human_id
    id
    stog.stog_art_by_human_id
  in
  { stog with
    stog_articles = articles ;
    stog_art_by_human_id = map ;
  }
;;

let sort_articles_by_date arts =
  List.sort
  (fun a1 a2 ->
     Pervasives.compare a1.art_date a2.art_date)
  arts
;;

let sort_ids_articles_by_date arts =
  List.sort
  (fun (_,a1) (_,a2) ->
     Pervasives.compare a1.art_date a2.art_date)
  arts
;;

let article_list ?(by_date=false) stog =
  let l =
    Stog_tmap.fold
    (fun id art acc -> (id, art) :: acc)
    stog.stog_articles
    []
  in
  if by_date then sort_ids_articles_by_date l else l
;;


let merge_stogs stogs =
  match stogs with
    [] -> assert false
  | stog :: q ->
      let f acc stog =
        let (articles, by_hid) =
          Stog_tmap.fold
          (fun _ art (arts, by_hid) ->
             let (id, arts) = Stog_tmap.add arts art in
             let by_hid = Str_map.add art.art_human_id id by_hid in
             (arts, by_hid)
          )
          stog.stog_articles
          (acc.stog_articles, acc.stog_art_by_human_id)
        in
        { acc with
          stog_articles = articles ;
          stog_art_by_human_id = by_hid ;
        }
      in
      List.fold_left f stog q
;;


let days = [| "dimanche" ; "lundi" ; "mardi" ; "mercredi" ; "jeudi" ; "vendredi" ; "samedi" |]
let months = [|
   "janvier" ; "février" ; "mars" ; "avril" ; "mai" ; "juin" ;
   "juillet" ; "août" ; "septembre" ; "octobre" ; "novembre" ; "décembre" |];;

let string_of_date (y,m,d) =
  let tm = { Unix.tm_mday = d ; tm_mon = (m-1) ; tm_year = (y - 1900) ;
             tm_sec = 0 ; tm_min = 0 ; tm_hour = 0 ; tm_wday = 0 ;
             tm_yday = 0 ; tm_isdst = false ; }
  in
  let (_, tm) = Unix.mktime tm in
  Printf.sprintf "%s %d %s %d"
    days.(tm.Unix.tm_wday) d months.(m-1) y
;;

let make_human_id stog str =
  let str = Stog_misc.lowercase str in
  let len = String.length str in
  let b = Buffer.create len in
  let rec iter dash i =
    if i >= len then
      Buffer.contents b
    else
      match str.[i] with
        'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '-' ->
          Buffer.add_char b str.[i];
          iter false (i+1)
      | c ->
          if dash then
            iter dash (i+1)
          else
          (Buffer.add_char b '-' ; iter true (i+1))
  in
  let hid0 = iter true 0 in
  let rec iter n =
    let hid = Printf.sprintf "%s%s"
      hid0 (if n = 1 then "" else string_of_int n)
    in
    try
      ignore(Str_map.find hid stog.stog_art_by_human_id);
      iter (n+1)
    with Not_found -> hid
  in
  iter 1
;;
