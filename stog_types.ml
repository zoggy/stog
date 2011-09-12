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

type stog = {
  stog_articles : (article, article) Stog_tmap.t ;
  stog_art_by_human_id : article Str_map.t ;
  stog_tmpl_dir : string ;
  stog_title : string ;
  }

let create_stog () = {
  stog_articles = Stog_tmap.create dummy_article;
  stog_art_by_human_id = Str_map.empty ;
  stog_tmpl_dir = "tmpl" ;
  stog_title = "Blog title" ;
  }
;;
