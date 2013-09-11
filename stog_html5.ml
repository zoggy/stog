

let void_tags =
  List.fold_right Stog_types.Str_set.add
    [ "area" ; "base" ; "br" ; "col" ; "embed" ; "hr" ; "img" ; "input" ;
      "keygen" ; "link" ; "meta" ; "param" ; "source" ; "track" ; "wbr" ;
    ]
    Stog_types.Str_set.empty
;;

let is_void_tag t = Stog_types.Str_set.mem t void_tags;;

let hack_self_closed =
  let rec iter xml =
    match xml with
      Xtmpl.D _ -> xml
    | Xtmpl.E (("", tag),atts,[]) when not (is_void_tag tag) ->
        Xtmpl.E (("", tag), atts, [Xtmpl.D ""])
    | Xtmpl.E (tag, atts, subs) ->
      Xtmpl.E (tag, atts, List.map iter subs)
  in
  iter
;;