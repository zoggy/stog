(** Computing information from articles. *)

open Stog_types;;

let article_list stog =
  Stog_tmap.fold
  (fun id art acc -> (id, art) :: acc)
  stog.stog_articles []
;;

let sort_articles_by_date arts =
  List.sort
  (fun (_,a2) (_,a1) ->
     Pervasives.compare a2.art_date a1.art_date)
  arts
;;

let compute_map f_words f_update stog =
  let f art_id article map =
    let on_word map w =
      let set =
        try Stog_types.Str_map.find w map
        with Not_found -> Stog_types.Art_set.empty
      in
      let set = Stog_types.Art_set.add art_id set in
      Stog_types.Str_map.add w set map
    in
    List.fold_left on_word map (f_words article)
  in
  f_update stog
  (Stog_tmap.fold f stog.stog_articles Stog_types.Str_map.empty)
;;

let compute_topic_map stog =
  compute_map
  (fun a -> a.art_topics)
  (fun stog map -> { stog with stog_arts_by_topic = map })
  stog
;;

let compute_keyword_map stog =
  compute_map
  (fun a -> a.art_keywords)
  (fun stog map -> { stog with stog_arts_by_kw = map })
  stog
;;

let compute_graph_with_dates stog =
  let arts = sort_articles_by_date (article_list stog) in
  let g = Stog_types.Graph.create () in
  let rec iter g = function
    [] | [_] -> g
  | (art_id, _) :: (next_id, next) :: q ->
      let g = Stog_types.Graph.add g (art_id, next_id, None) in
      iter g ((next_id, next) :: q)
  in
  { stog with stog_graph = iter g arts }
;;

let next_by_date f_next stog art_id =
  let next = f_next stog.stog_graph art_id in
  let next = List.filter (function (_,None) -> true | _ -> false) next in
  match next with
    [] -> None
  | (id,_) :: _ -> Some id

let succ_by_date = next_by_date Stog_types.Graph.succ;;
let pred_by_date = next_by_date Stog_types.Graph.pred;;


let add_topics_in_graph stog =
  let f word art_id g =
    let g =
      match succ_by_date stog art_id with
        None -> g
      | Some id ->
          Stog_types.Graph.add g (art_id, id, Some word)
    in
    g
  in
  let f_topic topic set g =
    Stog_types.Art_set.fold (f topic) set g
  in
  let g = Stog_types.Str_map.fold f_topic
    stog.stog_arts_by_topic stog.stog_graph
  in
  { stog with stog_graph = g }
;;

let compute_archives stog =
  let f_mon art_id m mmap =
    let set =
      try Stog_types.Int_map.find m mmap
      with Not_found -> Stog_types.Art_set.empty
    in
    let set = Stog_types.Art_set.add art_id set in
    Stog_types.Int_map.add m set mmap
  in
  let f_art art_id article ymap =
    let (year,mon, _) = article.art_date in
    let mmap =
      try Stog_types.Int_map.find year ymap
      with Not_found -> Stog_types.Int_map.empty
    in
    let mmap = f_mon art_id mon mmap in
    Stog_types.Int_map.add year mmap ymap
  in
  let arch = Stog_tmap.fold f_art
    stog.stog_articles Stog_types.Int_map.empty
  in
  { stog with stog_archives = arch }
;;

let compute stog =
  let stog = compute_keyword_map stog in
  let stog = compute_topic_map stog in
  let stog = compute_graph_with_dates stog in
  let stog = add_topics_in_graph stog in
  let stog = compute_archives stog in
  stog
;;

