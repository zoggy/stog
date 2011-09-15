(** *)

type contents_kind = Text | Html
type date = int * int * int (** day 1..31 * month 1..12 * year *)
type time = Unix.tm ;;

type message = {
    mes_time : time ;
    mes_subject : string ;
    mes_from : string ;
    mes_to : string list ;
    mes_body : string ;
  }

type 'a tree = 'a * 'a list ;;

type article =
  {
    art_human_id : string;
    art_kind : contents_kind ;
    art_body : string ;
    art_date : date ;
    art_keywords : string list ;
    art_topics : string list ;
    art_title : string ;
    art_location : string ;
    art_files : string list ; (** list of files in [art_location] *)
    art_comments : message tree list ;
  }
and article_id = article Stog_tmap.key

let dummy_article =
  { art_human_id = "dummy" ;
    art_kind = Text ;
    art_body = "" ;
    art_date = (01, 01, 2011) ;
    art_keywords = [] ;
    art_topics = [] ;
    art_title = "Dummy title";
    art_location = "/tmp" ;
    art_files = [] ;
    art_comments = [] ;
  }
;;

module Str_map = Map.Make (struct type t = string let compare = compare end);;
module Art_set = Set.Make (struct type t = article_id let compare = Stog_tmap.compare_key end);;
  module Int_map = Map.Make (struct type t = int let compare = compare end);;

module Graph = Stog_graph.Make_with_map
  (struct
     type t = article_id
     let compare = Stog_tmap.compare_key
   end
  )
  (struct type t = string option let compare = Pervasives.compare end);;

type stog = {
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
  }

let create_stog () = {
  stog_articles = Stog_tmap.create dummy_article;
  stog_art_by_human_id = Str_map.empty ;
  stog_tmpl_dir = "tmpl" ;
  stog_title = "Blog title" ;
  stog_body = "" ;
  stog_desc = "" ;
  stog_graph = Graph.create () ;
  stog_arts_by_kw = Str_map.empty ;
  stog_arts_by_topic = Str_map.empty ;
  stog_archives = Int_map.empty ;
  }
;;

let article stog id = Stog_tmap.get stog.stog_articles id;;
let article_by_human_id stog h =
  let id = Str_map.find h stog.stog_art_by_human_id in
  (id, article stog id)
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
  