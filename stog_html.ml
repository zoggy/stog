(** *)

open Stog_types;;

let generate_article outdir stog art_id article = ()

let generate_index outdir stog = ()

let generate outdir stog =
  generate_index outdir stog ;
  Stog_tmap.iter (generate_article outdir stog)
    stog.stog_articles
;;

  