(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012 Maxence Guesdon. All rights reserved.                   *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation; either version 2 of the         *)
(*    License.                                                                   *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *)
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

type contents_kind = Text | Html
type date = {
  year : int;
  month : int;
  day : int;
}
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
    art_vars : (string * string) list
  }
and article_id = article Stog_tmap.key

type page =
  { page_human_id : string ;
    page_kind : contents_kind ;
    page_body : string ;
    page_title : string ;
    page_vars : (string * string) list ;
  }
and page_id = page Stog_tmap.key

let today () =
  let t = Unix.gmtime (Unix.time()) in
  {
    year = t.Unix.tm_year + 1900;
    month = t.Unix.tm_mon+1;
    day = t.Unix.tm_mday
  }
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
    art_vars = [] ;
  }
;;
let  dummy_page () =
  { page_human_id = "dummypage" ;
    page_kind = Html ;
    page_body = "" ;
    page_title  = "" ;
    page_vars = [] ;
  }
;;

module Str_map = Map.Make (struct type t = string let compare = compare end);;
module Art_set = Set.Make (struct type t = article_id let compare = Stog_tmap.compare_key end);;
module Page_set = Set.Make (struct type t = page_id let compare = Stog_tmap.compare_key end);;
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
  stog_pages : (page, page) Stog_tmap.t ;
  stog_page_by_human_id : page_id Str_map.t ;
  stog_vars : (string * string) list ;
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
  stog_lang : string option ;
  }

let create_stog dir = {
  stog_dir = dir ;
  stog_articles = Stog_tmap.create (dummy_article ());
  stog_art_by_human_id = Str_map.empty ;
  stog_pages = Stog_tmap.create (dummy_page ());
  stog_page_by_human_id = Str_map.empty ;
  stog_tmpl_dir = Filename.concat dir "tmpl" ;
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
  stog_vars = [] ;
  stog_lang = None ;
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

let page stog id = Stog_tmap.get stog.stog_pages id;;
let page_by_human_id stog h =
  let id = Str_map.find h stog.stog_page_by_human_id in
  (id, page stog id)
;;

let set_page stog id page =
  { stog with
    stog_pages = Stog_tmap.modify stog.stog_pages id page }
;;

let add_page stog page =
  let (id, pages) = Stog_tmap.add stog.stog_pages page in
  let map = Str_map.add
    page.page_human_id
    id
    stog.stog_page_by_human_id
  in
  { stog with
    stog_pages = pages ;
    stog_page_by_human_id = map ;
  }
;;
let page_list stog =
  Stog_tmap.fold
  (fun id page acc -> (id, page) :: acc)
  stog.stog_pages
  []
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
