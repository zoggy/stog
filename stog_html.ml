(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Maxence Guesdon. All rights reserved.              *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
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

open Stog_types;;
module Smap = Stog_types.Str_map;;

let languages = ["fr" ; "en" ];;

let current_stog = ref None;;
let plugin_rules = ref [];;
let stage0_funs : (Stog_types.stog -> Stog_types.stog) list ref = ref [];;
let blocks = ref Smap.empty ;;
let counters = ref Smap.empty;;

let bump_counter s_hid name =
  let map =
    try Smap.find s_hid !counters
    with Not_found -> Smap.empty
  in
  let cpt =
    try Smap.find name map + 1
    with Not_found -> 1
  in
  let map = Smap.add name cpt map in
  counters := Smap.add s_hid map !counters;
  cpt
;;

let get_counter s_hid name =
  try Smap.find name (Smap.find s_hid !counters)
  with Not_found -> 0
;;

let set_counter s_hid name v =
  let map =
   try Smap.find s_hid !counters
   with Not_found -> Smap.empty
  in
  let map = Smap.add name v map in
  counters := Smap.add s_hid map !counters
;;

let random_id () =
  Printf.sprintf "%0x-%0x-%0x-%0x"
    (Random.int 0xffff) (Random.int 0xffff)
    (Random.int 0xffff) (Random.int 0xffff)
;;

let add_block ?(on_dup=`Warn) ~hid ~id ~short ~long () =
  let map =
    try Smap.find hid !blocks
    with Not_found -> Smap.empty
    in
  let map =
    try
      ignore (Smap.find id map);
      begin
        let msg = Printf.sprintf "Multiple blocks with id %S for hid=%S" id hid in
        match on_dup with
          `Warn -> Stog_msg.warning msg
        | `Fail -> failwith msg
        | `Ignore -> ()
      end;
      map
    with Not_found ->
        Smap.add id (short, long) map
  in
  blocks := Smap.add hid map !blocks;
;;

let get_in_env = Stog_latex.get_in_env;;

let get_in_args_or_env env args s =
  match Xtmpl.get_arg args s with
    None -> get_in_env env s
  | Some s -> s
;;

let get_hid env =
  let s = get_in_env env ("", Stog_tags.elt_hid) in
  assert (s <> "");
  s
;;

let escape_html s =
  let b = Buffer.create 256 in
  for i = 0 to String.length s - 1 do
    let s =
      match s.[i] with
        '<' -> "&lt;"
      | '>' -> "&gt;"
      | '&' -> "&amp;"
      | c -> String.make 1 c
    in
    Buffer.add_string b s
  done;
  Buffer.contents b
;;

let encode_for_url s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
    | 'A'..'Z' | 'a'..'z' | '0'..'9'
    | '_' | '-' | '.' | '!' | '*' | '+' | '/' ->
        Buffer.add_char b s.[i]
    | c -> Printf.bprintf b "%%%0x" (Char.code c)
  done;
  Buffer.contents b
;;

let elt_dst f_concat ?(encode=true) stog base elt =
  let path =
    match elt.elt_human_id.hid_path with
      [] -> failwith "Invalid human id: []"
    | h :: q -> List.fold_left f_concat h q
  in
  let ext = Stog_misc.filename_extension elt.elt_src in
  let path =
    if elt.elt_lang_dep then
      begin
        let ext_pref =
          match stog.stog_lang with
            None -> ""
          | Some lang -> "."^lang
        in
        Printf.sprintf "%s%s" path ext_pref
      end
    else
      path
  in
  let dst = match ext with "" -> path | _ -> path^"."^ext in
  let dst = if encode then encode_for_url dst else dst in
  f_concat base dst
;;

let elt_dst_file stog elt =
  elt_dst ~encode: false Filename.concat stog stog.stog_outdir elt;;

let elt_url stog elt =
  let url = elt_dst (fun a b -> a^"/"^b) stog stog.stog_base_url elt in
  let len = String.length url in
  let s = "/index.html" in
  let len_s = String.length s in
  if len >= len_s && String.sub url (len - len_s) len_s = s then
    String.sub url 0 (len-len_s)
  else
    url
;;


let url_of_hid stog ?ext hid =
  let elt = Stog_types.make_elt ~hid () in
  let src =
    Printf.sprintf "%s%s" (Stog_types.string_of_human_id hid)
      (match ext with None -> "" | Some s -> "."^s)
  in
  elt_url stog { elt with Stog_types.elt_src = src }
;;

let topic_index_hid topic =
  Stog_types.human_id_of_string ("/topic_" ^ topic);;
let keyword_index_hid kw =
  Stog_types.human_id_of_string ("/kw_"^ kw);;
let month_index_hid ~year ~month =
  Stog_types.human_id_of_string (Printf.sprintf "/%04d_%02d" year month);;


let rec make_fun (name, params, body) acc =
  let f env atts subs =
    let vars = List.map
      (fun (param,default) ->
         match Xtmpl.get_arg atts param with
           None -> (param, [], [ Xtmpl.xml_of_string default])
         | Some v -> (param, [], [ Xtmpl.xml_of_string v ])
      )
      params
    in
    let env = env_of_defs ~env vars in
    let env = Xtmpl.env_add "contents" (fun _ _ _ -> subs) env in
    Xtmpl.apply_to_xmls env body
  in
  (name, f) :: acc


and env_of_defs ?env defs =
  let f x acc =
    match x with
    | (key, [], body) -> (key, fun _ _ _ -> body) :: acc
    | _ ->  make_fun x acc
  in
  (* fold_right instead of fold_left to reverse list and keep associations
     in the same order as in declarations *)
  let l = List.fold_right f defs [] in
  Xtmpl.env_of_list ?env l
;;

(** FIXME: handle module requirements and already added modules *)
(* FIXME: add dependency ? *)
let env_of_used_mod stog ?(env=Xtmpl.env_empty) modname =
  try
    let m = Stog_types.Str_map.find modname stog.stog_modules in
    (*prerr_endline (Printf.sprintf "adding %d definitions from module %S"
      (List.length m.mod_defs) modname);*)
    env_of_defs ~env m.mod_defs
  with Not_found ->
    Stog_msg.warning (Printf.sprintf "No module %S" modname);
    env

let env_of_used_mods stog ?(env=Xtmpl.env_empty) mods =
  Stog_types.Str_set.fold (fun name env -> env_of_used_mod stog ~env name) mods env
;;

let env_add_langswitch env stog elt =
  match stog.stog_lang with
    None ->
      Xtmpl.env_add Stog_tags.langswitch (fun _ _ _ -> []) env
  | Some lang ->
      let map_lang lang =
         let url = elt_url { stog with stog_lang = Some lang } elt in
         let img_url = Printf.sprintf "%s/%s.png" stog.stog_base_url lang in
         Xtmpl.E (("", "a"), [("", "href"), url], [
           Xtmpl.E (("", "img"), [("", "src"), img_url ; ("", "title"), lang ; ("", "alt"), lang], [])])
      in
      let f _env args _subs =
        let languages =
          match Xtmpl.get_arg args ("", "languages") with
            Some s -> Stog_misc.split_string s [','; ';' ; ' ']
          | None -> languages
        in
        let languages = List.filter ((<>) lang) languages in
        List.map map_lang languages
      in
      Xtmpl.env_add Stog_tags.langswitch f env
;;

let elt_env build_rules stog ~env elt_id elt =
  let env = env_of_defs ~env elt.elt_defs in
  let env = env_of_used_mods stog ~env elt.elt_used_mods in
  let rules =
   (("", Stog_tags.elt_hid),
    (fun  _ _ _ -> [Xtmpl.D (Stog_types.string_of_human_id elt.elt_human_id)])) ::
   (build_rules stog elt_id elt)
  in
  let env = Xtmpl.env_of_list ~env rules in
  let env = env_add_langswitch env stog elt in
  env
;;

let make_lang_rules stog =
  match stog.stog_lang with
    None -> []
  | Some lang ->
      let to_remove = List.filter ((<>) lang) languages in
      let f_keep _env _args subs = subs in
      let f_remove _env _args _subs = [] in
      (("", lang), f_keep) ::
      (List.map (fun lang -> (("", lang), f_remove)) to_remove)
;;

let fun_counter env atts subs =
(*  prerr_endline (Printf.sprintf "fun_counter args=%s\nenv=%s"
    (String.concat "\n" (List.map (fun (s, v) -> Printf.sprintf "%S, %S" s v) atts))
    (Xtmpl.string_of_env env));
*)
  match Xtmpl.get_arg atts ("", "counter-name") with
    None -> subs
  | Some name ->
      let hid = get_hid env in
      let cpt = get_counter hid name in
      [Xtmpl.D (string_of_int cpt)]
;;

let fun_include stog elt _env args subs =
  match Xtmpl.get_arg args ("", "file") with
  | Some file ->
      let file =
        if Filename.is_relative file then
          begin
            if Filename.is_implicit file then
              Filename.concat stog.stog_tmpl_dir file
            else
              Filename.concat (Filename.dirname elt.elt_src) file
          end
        else
          file
      in
      let xml =
        match Xtmpl.get_arg args ("", "raw") with
        | Some "true" -> [Xtmpl.D (Stog_misc.string_of_file file)]
        | _ -> [Xtmpl.xml_of_string (Stog_misc.string_of_file file)]
      in
      let args =
        (("", "contents"), String.concat "" (List.map Xtmpl.string_of_xml subs)) ::
        args
      in
      begin
        match Xtmpl.get_arg args ("", "depend") with
          Some "false" -> ()
        | _ -> Stog_deps.add_dep elt (Stog_deps.File file);
      end;
      [Xtmpl.E (("", Xtmpl.tag_env), args, xml)]
  | None ->
      failwith "Missing 'file' argument for include command"
;;

let fun_inc stog elt env args subs =
  let href =
    match Xtmpl.get_arg args ("", "href") with
      None -> failwith "Missing href for inc rule"
    | Some href -> href
  in
  let (hid, id) =
    try
      let p = String.index href '#' in
      let len = String.length href in
      let hid = String.sub href 0 p in
      (hid, String.sub href (p+1) (len - (p+1)))
    with
        Not_found ->
        failwith "Missing block id for inc rule"
  in
  try
    let s_hid = match hid with "" -> get_hid env | s ->  s in
    let hid = Stog_types.human_id_of_string s_hid in
    Stog_deps.add_dep elt (Stog_deps.Elt s_hid);
    let (_, elt) = Stog_types.elt_by_human_id stog hid in
    match Stog_types.find_block_by_id elt id with
      Some xml -> [xml]
    | None ->
        failwith
        (Printf.sprintf "No id %S in element %S"
         id (Stog_types.string_of_human_id hid))
  with
    Failure s ->
      Stog_msg.error s;
      [Xtmpl.D ("??"^href^"??")]
;;

let fun_image _env args legend =
  let width = Xtmpl.opt_arg args ("", "width") in
  let src = Xtmpl.opt_arg args ("", "src") in
  let cls = Printf.sprintf "img%s"
    (match Xtmpl.get_arg args ("", "float") with
       Some "left" -> "-float-left"
     | Some "right" -> "-float-right"
     | Some s -> failwith (Printf.sprintf "unhandled image position: %s" s)
     | None -> ""
    )
  in
  let pred (s,_) = not (List.mem s [("", "width") ; ("", "src") ; ("", "float")]) in
  let atts = List.filter pred args in
  [
    Xtmpl.E (("", "div"), [ ("", "class"), cls ],
     (Xtmpl.E (("", "img"),
       [ ("", "class"), "img" ; ("", "src"), src; ("", "width"), width ] @ atts,
       [])
     ) ::
     (match legend with
        [] -> []
      | xml -> [ Xtmpl.E (("", "div"), [("", "class"), "legend"], xml) ]
     )
    )
  ]
;;

let fun_list env args subs =
  let sep = Xtmpl.opt_arg args ("", "sep") in
  let xml = Xtmpl.xml_of_string sep in
  (* insert the separator between all children of the node *)
  let rec iter acc = function
    [] -> List.rev acc
  | h :: q ->
      let acc =
        match acc with
          [] -> [h]
        | _ -> h :: xml :: acc
      in
      iter acc q
  in
  (* and finally return the list of xml trees *)
  iter [] subs
;;

let elt_by_href ?typ stog env href =
  let (hid, id) =
    try
      let p = String.index href '#' in
      let len = String.length href in
      let hid = String.sub href 0 p in
      (hid, Some (String.sub href (p+1) (len - (p+1))))
    with
      Not_found -> (href, None)
  in
  let elt =
    try
      let hid = match hid with "" -> get_hid env | s ->  s in
      let hid = Stog_types.human_id_of_string hid in
      let (_, elt) = Stog_types.elt_by_human_id ?typ stog hid in
      Some elt
    with
      Failure s ->
        Stog_msg.error ~info: "Stog_html.elt_by_href" s;
        None
  in
  match elt with
    None -> None
  | Some elt -> Some (elt, hid, id)
;;

let fun_elt_href ?typ src_elt href stog env args subs =
  let report_error msg = Stog_msg.error ~info: "Stog_html.fun_elt_href" msg in
  let quotes =
    match Xtmpl.get_arg args ("", "quotes") with
      None -> false
    | Some s -> Stog_io.bool_of_string s
  in
  let (elt, text) =
    let info = elt_by_href ?typ stog env href in
    let text =
      match info with
        None -> [Xtmpl.D "??"]
      | Some (elt, hid, id) ->
          begin
            (* use absolute hid, from element *)
            let hid = Stog_types.string_of_human_id elt.elt_human_id in
            Stog_deps.add_dep src_elt (Stog_deps.Elt hid);
          end;
          match subs, id with
          | [], None ->
              let quote = if quotes then "\"" else "" in
              let s = Printf.sprintf "%s%s%s" quote elt.elt_title quote in
              [Xtmpl.xml_of_string s]
          | text, None -> text
          | _, Some id ->
              let hid = Stog_types.string_of_human_id elt.elt_human_id in
              let title =
                try
                  let id_map = Smap.find hid !blocks in
                  try
                    let (short, long) = Smap.find id id_map in
                    match Xtmpl.get_arg args ("", "long") with
                      Some "true" -> long
                    | _ -> short
                  with Not_found ->
                      let msg = Printf.sprintf "Unknown block hid=%S, id=%S" hid id in
                      report_error msg;
                      Xtmpl.D "??"
                with Not_found ->
                    let msg = Printf.sprintf "Unknown element %S in block map" hid in
                    report_error msg;
                    Xtmpl.D "??"
              in
              match subs with
                [] ->
                  if quotes then
                    [ Xtmpl.D "\"" ; title ; Xtmpl.D "\""]
                  else
                    [title]
              | text -> text
    in
    (info, text)
  in
  match elt with
    None -> [Xtmpl.E (("", "span"), [("", "class"), "unknown-ref"], text)]
  | Some (elt, _, id) ->
      let href = Printf.sprintf "%s%s" (elt_url stog elt)
        (match id with None -> "" | Some s -> "#"^s)
      in
      [
        Xtmpl.E (("", "a"), [("", "href"), href], text)
      ]
;;

let fun_elt ?typ src_elt stog env args subs =
  let href =
    match Xtmpl.get_arg args ("", "href") with
      None ->
        let msg = Printf.sprintf "Missing href for <%s>"
          (match typ with None -> "elt" | Some s -> s)
        in
        failwith msg
    | Some s -> s
  in
  fun_elt_href ?typ src_elt href stog env args subs
;;

let fun_post = fun_elt ~typ: "post";;
let fun_page = fun_elt ~typ: "page";;

(* FIXME: add adependency ? *)
let fun_archive_tree stog _env _ =
  let mk_months map =
    List.sort (fun (m1, _) (m2, _) -> compare m2 m1)
    (Stog_types.Int_map.fold
     (fun month data acc -> (month, data) :: acc)
     map
     []
    )
  in
  let years =
    Stog_types.Int_map.fold
      (fun year data acc -> (year, mk_months data) :: acc)
      stog.stog_archives
      []
  in
  let years = List.sort (fun (y1,_) (y2,_) -> compare y2 y1) years in

  let f_mon year (month, set) =
    let hid = month_index_hid ~year ~month in
    let href = url_of_hid stog ~ext: "html" hid in
    let month_str = Stog_intl.get_month stog.stog_lang month in
    Xtmpl.E (("", "li"), [], [
       Xtmpl.E (("", "a"), [("", "href"), href], [ Xtmpl.D month_str ]) ;
       Xtmpl.D (Printf.sprintf "(%d)" (Stog_types.Elt_set.cardinal set))
     ]
    )
  in
  let f_year (year, data) =
    Xtmpl.E (("", "li"), [], [
       Xtmpl.D (string_of_int year) ;
       Xtmpl.E (("", "ul"), [], List.map (f_mon year) data) ;
      ]
    )
  in
  [ Xtmpl.E (("", "ul"), [], List.map f_year years) ]
;;

let highlight = Stog_misc.highlight;;

let fun_hcode ?(inline=false) ?lang stog _env args code =
  let language, language_options =
    match lang with
      None ->
        (
         match Xtmpl.get_arg args ("", "lang-file") with
           None ->
             begin
               let lang = Xtmpl.opt_arg args ~def: "txt" ("", "lang") in
               match lang with
                 "txt" -> (lang, None)
               | _ -> (lang, Some (Printf.sprintf "--syntax=%s" lang))
             end
         | Some f ->
             let lang = Xtmpl.opt_arg args ~def: "" ("", "lang") in
             let opts = Printf.sprintf "--config-file=%s" f in
             (lang, Some opts)
        )
    | Some "ocaml" ->
        let lang_file = Filename.concat stog.stog_dir "ocaml.lang" in
        let opts = if Sys.file_exists lang_file then
            Printf.sprintf "--config-file=%s" lang_file
          else
            "--syntax=ocaml"
        in
        ("ocaml", Some opts)
    | Some lang ->
        (lang, Some (Printf.sprintf "--syntax=%s" lang))
  in
  let code =
    let l = List.map
      (
       function Xtmpl.D code -> code
       | x -> Xtmpl.string_of_xml x
      )
      code
    in
    String.concat "" l
  in
  let code = Stog_misc.strip_blank_lines code in
  let xml_code =
    match language_options with
      None -> Xtmpl.D code
    | Some opts ->
        let code = highlight ~opts code in
        Xtmpl.xml_of_string code
  in
  if inline then
    [ Xtmpl.E (("", "span"), [("", "class"), "icode"], [xml_code]) ]
  else
    [ Xtmpl.E (("", "pre"),
       [ ("", "class"), Printf.sprintf "code-%s" language], [xml_code])
    ]
;;

let fun_ocaml = fun_hcode ~lang: "ocaml";;
let fun_command_line = fun_hcode ~lang: "sh";;
let fun_icode = fun_hcode ~inline: true ;;

let concat_name ?(sep=":") (prefix, name) =
  match prefix with
    "" -> name
  | p -> p ^ sep ^ name
;;

let make_fun_section sect_up cls sect_down env args subs =
  let hid = get_hid env in
  List.iter
    (fun cls_down ->
       set_counter hid (concat_name ~sep: ":" cls_down) 0
     )
    sect_down;
  let att_id = (("", "id"), "<id/>") in
  let class_name = concat_name ~sep: "-" cls in
  let body =
    [ Xtmpl.E (("", "div"), [("", "class"), class_name],
       (
        (Xtmpl.E (("", "div"),
          (("", "class"), class_name^"-title") :: att_id :: [],
          [Xtmpl.E (("", "title"),[],[])])) ::
        subs
       )
      )
    ]
  in
  let f (prefix, cls) =
    match get_in_env env (prefix, (concat_name (prefix, cls))^"-counter") with
      s when not (Stog_io.bool_of_string s) -> ""
    | _ ->
        Printf.sprintf "<counter counter-name=%S/>"
        (concat_name (prefix, cls))
  in
  let counters = String.concat "."
    (List.filter ((<>) "") (List.rev_map f (cls::sect_up)))
  in
  let counter_name =
    let (pref, name) = cls in
    match get_in_env env (pref, (concat_name cls)^"-counter") with
      s when not (Stog_io.bool_of_string s) -> ""
    | _ -> concat_name cls
  in
  let label = String.capitalize (snd cls) in
  [ Xtmpl.E (("", Stog_tags.block),
     (("", "label"), label) :: (("", "class"), class_name) :: (("", "counter-name"), counter_name) ::
     (("", "with-contents"), "true") :: args,
     [
       Xtmpl.E (("", "long-title-format"), [],
        [Xtmpl.xml_of_string (Printf.sprintf "%s%s<title/>" counters (if counters = "" then "" else ". "))]);
       Xtmpl.E (("", "short-title-format"), [],
        (match counter_name with
          "" -> [Xtmpl.xml_of_string "<title/>"]
         | _ -> [Xtmpl.xml_of_string counters]
        ));
       Xtmpl.E (("", "contents"), [], body) ;
     ]
    )
  ]
;;

let fun_search_form stog _env _ _ =
  let tmpl = Filename.concat stog.stog_tmpl_dir "search.tmpl" in
  [ Xtmpl.xml_of_file tmpl ]
;;

let fun_blog_url stog _env _ _ = [ Xtmpl.D stog.stog_base_url ];;

(* FIXME: add dependency ? *)
let fun_graph =
  let generated = ref false in
  let report_error msg = Stog_msg.error ~info: "Stog_html.fun_graph" msg in
  fun stog _env _ _ ->
    let png_name = "site-graph.png" in
    let small_png_name = "small-"^png_name in
    let svg_file = (Filename.chop_extension png_name) ^ ".svg" in
    let src = Printf.sprintf "%s/%s" stog.stog_base_url svg_file in
    let small_src = Printf.sprintf "%s/%s" stog.stog_base_url small_png_name in
    begin
      match !generated with
        true -> ()
      | false ->
          generated := true;
          let dot_code = Stog_info.dot_of_graph (elt_url stog) stog in

          let tmp = Filename.temp_file "stog" "dot" in
          Stog_misc.file_of_string ~file: tmp dot_code;

          let com = Printf.sprintf "dot -Gcharset=utf-8 -Tpng -o %s %s"
            (Filename.quote (Filename.concat stog.stog_outdir png_name))
            (Filename.quote tmp)
          in
          let svg_code = Stog_misc.dot_to_svg dot_code in
          Stog_misc.file_of_string ~file: (Filename.concat stog.stog_outdir svg_file) svg_code;
          match Sys.command com with
            0 ->
              begin
                (try Sys.remove tmp with _ -> ());
                let com = Printf.sprintf "convert -scale 120x120 %s %s"
                  (Filename.quote (Filename.concat stog.stog_outdir png_name))
                  (Filename.quote (Filename.concat stog.stog_outdir small_png_name))
                in
                match Sys.command com with
                  0 -> ()
                | _ ->
                    report_error (Printf.sprintf "Command failed: %s" com)
              end
          | _ ->
              report_error (Printf.sprintf "Command failed: %s" com)
    end;
    [
      Xtmpl.E (("", "a"), [("", "href"), src], [
         Xtmpl.E (("", "img"), [("", "src"), small_src ; ("", "alt"), "Graph"], [])
       ])
    ]
;;

let fun_if env args subs =
  let pred ((prefix, name), v) =
    let nodes = [ Xtmpl.E ((prefix, name), [], []) ] in
    let nodes2 = Xtmpl.apply_to_xmls env nodes in
    let v2 = if nodes = nodes2 then [] else nodes2 in
    (*
    prerr_endline (Printf.sprintf "fun_if: pred: att=(%s,%s), nodes=%S nodes2=%S, v=%S, v2=%S"
     prefix name (Xtmpl.string_of_xmls nodes)
     (Xtmpl.string_of_xmls nodes2) v (Xtmpl.string_of_xmls v2));
    *)
    let v_xml = Xtmpl.xml_of_string v in
    let v2_xml = Xtmpl.E (("", Xtmpl.tag_main), [], v2) in
    (*
    prerr_endline (Printf.sprintf "v_xml=%S, v2_xml=%S"
     (Xtmpl.string_of_xml v_xml) (Xtmpl.string_of_xml v2_xml));
    *)
    v_xml = v2_xml
  in
  let cond = List.for_all pred args in
  let subs = List.filter
    (function Xtmpl.D _ -> false | _ -> true)
    subs
  in
  match cond, subs with
  | true, [] -> failwith "<if>: missing children"
  | true, h :: _
  | false, _ :: h :: _ -> [h]
  | false, []
  | false, [_] -> []
;;


let fun_twocolumns env args subs =
  (*prerr_endline (Printf.sprintf "two-columns, length(subs)=%d" (List.length subs));*)
  let empty = [] in
  let subs = List.fold_right
    (fun xml acc ->
       match xml with
         Xtmpl.D _ -> acc
       | Xtmpl.E (_, _, subs) -> subs :: acc
    ) subs []
  in
  let left, right =
    match subs with
      [] -> empty, empty
    | [left] -> left, empty
    | left :: right :: _ -> left, right
  in
  [ Xtmpl.E (("", "table"), [("", "class"), "two-columns"],
     [ Xtmpl.E (("", "tr"), [],
        [ Xtmpl.E (("", "td"), [("", "class"), "two-columns-left"], left) ;
          Xtmpl.E (("", "td"), [("", "class"), "two-columns-right"], right) ;
        ]);
     ])
  ]
;;

let fun_ncolumns env args subs =
  let subs = List.fold_right
    (fun xml acc ->
       match xml with
         Xtmpl.D _ -> acc
       | Xtmpl.E (_, _, subs) -> subs :: acc
    ) subs []
  in
  let tds =
    let f (n,acc) xmls =
       let acc =
        (Xtmpl.E (("", "td"),
          [("", "class"), Printf.sprintf "n-columns column-%d" n], xmls)
        ) :: acc
      in
      (n+1, acc)
    in
    List.rev (snd (List.fold_left f (0,[]) subs))
  in
  [ Xtmpl.E (("", "table"), [("", "class"), "n-columns"],
     [ Xtmpl.E (("", "tr"), [], tds) ]
    );
  ]
;;

let fun_exta env args subs =
  [ Xtmpl.E (("", "span"), [("", "class"),"ext-a"],
     [ Xtmpl.E (("", "a"), args, subs) ])
  ]
;;

type toc = Toc of string * string * Xmlm.name * toc list (* name, title, class, subs *)

let fun_prepare_toc tags env args subs =
  let depth =
    match Xtmpl.get_arg args ("", "depth") with
      None -> max_int
    | Some s -> int_of_string s
  in
  let rec iter d acc = function
  | Xtmpl.D _ -> acc
  | Xtmpl.E (tag, atts, subs) when List.mem tag tags ->
      begin
        match Xtmpl.get_arg atts ("", "id"), Xtmpl.get_arg atts ("", "title") with
          None, _ | _, None ->
            (*prerr_endline "no name nor title";*)
            acc
        | Some name, Some title ->
            if d > depth
            then acc
            else
              (
               let subs = List.rev (List.fold_left (iter (d+1)) [] subs) in
               (*prerr_endline (Printf.sprintf "depth=%d, d=%d, title=%s" depth d title);*)
               (Toc (name, title, tag, subs)) :: acc
              )
      end
  | Xtmpl.E (_,_,subs) -> List.fold_left (iter d) acc subs
  in
  let toc = List.rev (List.fold_left (iter 1) [] subs) in
  (*(
   match toc with
     [] -> prerr_endline "empty toc!"
   | _ -> prerr_endline (Printf.sprintf "toc is %d long" (List.length toc));
  );*)
  let rec xml_of_toc = function
    Toc (name, title, cl, subs) ->
      let cl =
        match cl with
          ("", s) -> s
        | (p, s) -> p ^"-"^ s
      in
      Xtmpl.E (("", "li"), [("", "class"), "toc-"^cl],
       [ Xtmpl.E (("", "a"), [("", "href"), "#"^name],
         [ Xtmpl.xml_of_string title ]) ]
       @
       ( match subs with
          [] -> []
        | _ ->
               [ Xtmpl.E (("", "ul"), [("", "class"), "toc"], List.map xml_of_toc subs) ]
       )
       )
  in
  let xml = Xtmpl.E (("", "ul"), [("", "class"), "toc"], List.map xml_of_toc toc) in
  let atts = [ ("", "toc-contents"), Xtmpl.string_of_xml xml ] in
  [ Xtmpl.E (("", Xtmpl.tag_env), atts, subs) ]
;;

let fun_toc env args subs =
  subs @ [Xtmpl.E (("", "toc-contents"), [], [])]
;;

type block =
  { blk_id : string ;
    blk_label : Xtmpl.tree list option ;
    blk_class : string option ;
    blk_title : Xtmpl.tree list ;
    blk_cpt_name : string option ;
    blk_long_f : Xtmpl.tree list ;
    blk_short_f : Xtmpl.tree list ;
    blk_body : Xtmpl.tree list ;
  }

let block_body_of_subs stog blk = function
  [] ->
    let tmpl_file =
      match blk.blk_class with
        None -> "block.tmpl"
      | Some c -> Printf.sprintf "block-%s.tmpl" c
    in
    let tmpl = Filename.concat stog.Stog_types.stog_tmpl_dir tmpl_file in
    [ Xtmpl.xml_of_file tmpl ]
| l -> l
;;

let read_block_from_subs stog =
  let s_xmls l = String.concat "" (List.map Xtmpl.string_of_xml l) in
  let rec f blk = function
    Xtmpl.D _ -> blk
  | Xtmpl.E (("", tag), _, xmls) ->
      begin
        match tag, xmls with
          "id", [Xtmpl.D id] -> { blk with blk_id = id }
        | "id", _ ->
            Stog_msg.warning
            (Printf.sprintf "Ignoring id of block: %S" (s_xmls xmls));
            blk
        | "label", _ -> { blk with blk_label = Some xmls }
        | "class", [Xtmpl.D cls] -> { blk with blk_class = Some cls }
        | "class", _ ->
            Stog_msg.warning
            (Printf.sprintf  "Ignoring class of block: %S" (s_xmls xmls));
            blk
        | "counter-name", [Xtmpl.D s] -> { blk with blk_cpt_name = Some s }
        | "counter-name", _ ->
            Stog_msg.warning
            (Printf.sprintf "Ignoring counter-name of block: %S" (s_xmls xmls));
            blk
        | "title", _ -> { blk with blk_title = xmls }
        | "long-title-format", _ -> { blk with blk_long_f = xmls }
        | "short-title-format", _ -> { blk with blk_short_f = xmls }
        | "contents", _ -> { blk with blk_body = block_body_of_subs stog blk xmls }
        | _, _ ->
            Stog_msg.warning
            (Printf.sprintf "Ignoring block node %S" tag);
            blk
      end
  | Xtmpl.E _ -> blk
  in
  List.fold_left f
;;

let read_block stog args subs =
  let with_contents =
    match Xtmpl.get_arg args ("", "with-contents") with
      Some "true" -> true
    | None | Some _ -> false
  in
  let blk_id =
    match Xtmpl.get_arg args ("", "id") with
      Some id -> id
    | None -> random_id ()
  in
  let blk_label =
    match Xtmpl.get_arg args ("", "label") with
      None -> None
    | Some s -> Some [ Xtmpl.xml_of_string s ]
  in
  let blk_class = Xtmpl.get_arg args ("", "class") in
  let blk_title =
    match Xtmpl.get_arg args ("", "title") with
      None -> [ Xtmpl.xml_of_string " " ]
    | Some s -> [ Xtmpl.xml_of_string s ]
  in
  let blk_cpt_name = Xtmpl.get_arg args ("", "counter-name") in
  let xml_title = Xtmpl.E (("", "title"), [], []) in
  let xml_label =  Xtmpl.E (("", "label"), [], []) in
  let xml_cpt s = Xtmpl.E (("", "counter"), [("", "counter-name"), s], []) in
  let blk_long_f =
    match Xtmpl.get_arg args ("", "long-title-format") with
      None ->
        (xml_label ::
         (match blk_cpt_name with
            None -> []
          | Some c -> [Xtmpl.D " " ; xml_cpt c]
         ) @ [ Xtmpl.D ": " ; xml_title ]
        )
    | Some s -> [ Xtmpl.xml_of_string s ]
  in
  let blk_short_f =
    match Xtmpl.get_arg args ("", "short-title-format") with
      None ->
        begin
          match blk_label, blk_cpt_name with
            None, None ->  [ xml_title ]
          | Some _, None -> [ xml_label ]
          | None, Some s -> [ xml_cpt s ]
          | Some _, Some s -> [ xml_label ; Xtmpl.D " " ; xml_cpt s ]
        end
    | Some s -> [ Xtmpl.xml_of_string s ]
  in
  let blk = {
    blk_id ; blk_label ; blk_class ; blk_title ;
    blk_cpt_name ; blk_long_f ; blk_short_f ;
    blk_body = [] ;
    }
  in
  match with_contents with
    false -> { blk with blk_body = block_body_of_subs stog blk subs }
  | true -> read_block_from_subs stog blk subs
;;

let fun_block1 stog env args subs =
  match Xtmpl.get_arg args ("", "href") with
    Some s when s <> "" ->
      begin
        match Xtmpl.get_arg args ("", Stog_tags.elt_hid) with
          Some _ -> raise Xtmpl.No_change
        | None ->
            let hid = get_hid env in
            [ Xtmpl.E (("", Stog_tags.block), [("", Stog_tags.elt_hid), hid ; ("", "href"), s], subs)]
      end
  | _ ->
      let hid = get_hid env in
      let block = read_block stog args subs in
      begin
        match block.blk_cpt_name with
          None -> ()
        | Some name -> ignore(bump_counter hid name)
      end;
      let env = Xtmpl.env_add_att "id" block.blk_id env in
      let env = Xtmpl.env_add "title" (fun _ _ _ -> block.blk_title) env in
      let env = Xtmpl.env_add "label"
        (fun _ _ _ -> match block.blk_label with None -> [] | Some xml -> xml) env
      in
      let env = Xtmpl.env_add_att "class" (Stog_misc.string_of_opt block.blk_class) env in
      let env = Xtmpl.env_add_att "counter-name" (Stog_misc.string_of_opt block.blk_cpt_name) env in
      let long =
        Xtmpl.E (("", Xtmpl.tag_main), [], Xtmpl.apply_to_xmls env block.blk_long_f)
      in
      let short =
         Xtmpl.E (("", Xtmpl.tag_main), [], Xtmpl.apply_to_xmls env block.blk_short_f)
      in
      add_block ~hid ~id: block.blk_id ~short ~long ();
      let env = Xtmpl.env_add "title" (fun _ _ _ -> [long]) env in
      Xtmpl.apply_to_xmls env block.blk_body
;;

let fun_block2 env atts subs =
  match Xtmpl.get_arg atts ("", "href") with
    None -> subs
  | Some href ->
      let hid = match Xtmpl.get_arg atts ("", Stog_tags.elt_hid) with
          None -> assert false
        | Some hid -> hid
      in
      let url = Printf.sprintf "%s#%s" hid href in
      let quotes =
        match Xtmpl.get_arg atts ("", "quotes") with
          None -> "false"
        | Some s -> s
      in
      [Xtmpl.E (("", Stog_tags.elt), [("", "href"), url ; ("", "quotes"), quotes], [])]
;;

let intro_of_elt stog elt =
  let rec iter acc = function
    [] -> raise Not_found
  | (Xtmpl.E (("",tag), _, _)) :: _ when tag = Stog_tags.sep -> List.rev acc
  | h :: q -> iter (h::acc) q
  in
  try
    let xml = iter [] elt.elt_body in
    xml @
    [
      Xtmpl.E (("", "a"), [("", "href"), elt_url stog elt],
       [ Xtmpl.E (("", "img"),
          [ ("", "src"), Printf.sprintf "%s/next.png" stog.stog_base_url;
            ("", "alt"), "next"],
          [])]
      )
    ]
  with
    Not_found -> elt.elt_body
;;

let html_of_topics stog elt env args _ =
  let sep = Xtmpl.xml_of_string (Xtmpl.opt_arg args ~def: ", " ("", "set")) in
  let tmpl = Stog_tmpl.get_template stog ~elt Stog_tmpl.topic "topic.tmpl" in
  let f w =
    let env = Xtmpl.env_of_list ~env [ ("", Stog_tags.topic), (fun _ _ _ -> [Xtmpl.D w]) ] in
    Xtmpl.apply_to_xmls env [tmpl]
  in
  Stog_misc.list_concat ~sep
  (List.map (fun w ->
      let href = url_of_hid stog ~ext: "html" (topic_index_hid w) in
      Xtmpl.E (("", "a"), [("", "href"), href ], f w))
   elt.elt_topics
  )
;;

let html_of_keywords stog elt env args _ =
  let sep = Xtmpl.xml_of_string (Xtmpl.opt_arg args ~def: ", " ("", "set")) in
  let tmpl = Stog_tmpl.get_template stog ~elt Stog_tmpl.keyword "keyword.tmpl" in
  let f w =
    let env = Xtmpl.env_of_list ~env [ ("", Stog_tags.keyword), (fun _ _ _ -> [Xtmpl.D w]) ] in
    Xtmpl.apply_to_xmls env [tmpl]
  in
  Stog_misc.list_concat ~sep
  (List.map (fun w ->
      let href = url_of_hid stog ~ext: "html" (keyword_index_hid w) in
      Xtmpl.E (("", "a"), [("", "href"), href], f w))
   elt.elt_keywords
  )
;;

let rss_date_of_date d =
  let {year; month; day} = d in
  {
    Rss.year = year ; month ; day;
    hour = 8 ; minute = 0 ; second = 0 ;
    zone = 0 ; week_day = -1 ;
  }
;;
(* FIXME: handle RSS files as any other element ? *)
let rec elt_to_rss_item stog elt_id elt =
  let link = elt_url stog elt in
  let pubdate =
    match elt.elt_date with
      None -> assert false
    | Some d -> rss_date_of_date d
  in
  let f_word w =
    { Rss.cat_name = w ; Rss.cat_domain = None }
  in
  let cats =
    (List.map f_word elt.elt_topics) @
    (List.map f_word elt.elt_keywords)
  in
  let env = elt_env build_base_rules stog ~env:Xtmpl.env_empty elt_id elt in
  let author =
    let author = get_in_env env ("", "author") in
    if author = "" then None else Some author in
  let desc_xml = Xtmpl.apply_to_xmls env (intro_of_elt stog elt) in
  let desc = String.concat "" (List.map Xtmpl.string_of_xml desc_xml) in
  Rss.item ~title: elt.elt_title
  ~desc
  ~link
  ~pubdate
  ~cats
  ~guid: { Rss.guid_name = link ; guid_permalink = true }
  ?author
  ()

and generate_rss_feed_file stog ?title link elts file =
  let elts = List.rev (Stog_types.sort_ids_elts_by_date elts) in
  let elts = List.filter
    (fun (_,elt) -> match elt.elt_date with None -> false | _ -> true)  elts
  in
  let items = List.map (fun (id, elt) -> elt_to_rss_item stog id elt) elts in
  let title = Printf.sprintf "%s%s"
    stog.stog_title
    (match title with None -> "" | Some t -> Printf.sprintf ": %s" t)
  in
  let pubdate =
    match elts with
      [] -> None
    | (_,h) :: _ ->
        Some (rss_date_of_date
         (match h.elt_date with None -> assert false | Some d -> d))
  in
  let image =
    try
      let file =
        match Stog_types.get_def stog.stog_defs ("", "rss-image") with
          Some (_,xmls) -> Xtmpl.string_of_xmls xmls
        | None -> ""
      in
      let url = Filename.concat stog.stog_base_url file in
      let image = {
          Rss.image_url = url ;
          image_title = stog.stog_title ;
          image_link = stog.stog_base_url ;
          image_height = None ;
          image_width = None ;
          image_desc = None ;
        }
      in
      Some image
    with
      Not_found -> None
  in
  let desc = String.concat "" (List.map Xtmpl.string_of_xml stog.stog_desc) in
  let channel =
    Rss.channel ~title ~link
    ~desc ?image
    ~managing_editor: stog.stog_email
    ?pubdate ?last_build_date: pubdate
    ~generator: "Stog"
    items
  in
  let channel = Rss.keep_n_items stog.stog_rss_length channel in
  (* break tail-rec to get a better error backtrace *)
  let result = Rss.print_file ~encoding: "UTF-8" file channel in
  result

and build_base_rules stog elt_id elt =
  let f_title elt _ _ _ = [ Xtmpl.xml_of_string elt.elt_title ] in
  let f_url elt _ _ _ = [ Xtmpl.D (elt_url stog elt) ] in
  let f_body elt _ _ _ = elt.elt_body in
  let f_type elt _ _ _ = [Xtmpl.D elt.elt_type] in
  let f_src elt _ _ _ = [Xtmpl.D elt.elt_src] in
  let f_date elt _ _ _ = [ Xtmpl.D (Stog_intl.string_of_date_opt stog.stog_lang elt.elt_date) ] in
  let f_intro elt _ _ _ = intro_of_elt stog elt in
  let mk f env atts subs =
    let nodes = [ Xtmpl.E (("", Stog_tags.elt_hid), [], []) ] in
    let nodes2 = Xtmpl.apply_to_xmls env nodes in
    if nodes2 = nodes then
      []
    else
      (
       match nodes2 with
         [Xtmpl.D s] ->
           let (_, elt) = Stog_types.elt_by_human_id stog (Stog_types.human_id_of_string s) in
           f elt env atts subs
       | _ -> []
      )
  in
  let (previous, next) =
    let html_link elt =
      let href = elt_url stog elt in
      [ Xtmpl.E (("", "a"), [("","href"), href], [ Xtmpl.xml_of_string elt.elt_title ]) ]
    in
    let try_link key search =
      let fallback () =
        match search stog elt_id with
        | None -> []
        | Some id -> html_link (Stog_types.elt stog id)
      in
      match Stog_types.get_def elt.elt_defs key with
        None -> fallback ()
      | Some (_,body) ->
          let hid = Stog_types.human_id_of_string (Xtmpl.string_of_xmls body) in
          try
            let (_, elt) = Stog_types.elt_by_human_id stog hid in
            html_link elt
          with Failure s ->
              Stog_msg.warning s;
              fallback ()
    in
    (try_link ("","previous") Stog_info.pred_by_date,
     try_link ("","next") Stog_info.succ_by_date)
  in
  let l =
    !plugin_rules @
    [
      ("", Stog_tags.archive_tree), (fun _ -> fun_archive_tree stog) ;
      ("", Stog_tags.block), fun_block1 stog ;
      ("", Stog_tags.command_line), fun_command_line ~inline: false stog ;
      ("", Stog_tags.counter), fun_counter ;
      ("", Stog_tags.elements), elt_list elt stog ;
      ("", Stog_tags.elt_body), mk f_body ;
      ("", Stog_tags.elt_date), mk f_date ;
      ("", Stog_tags.elt_intro), mk f_intro ;
      ("", Stog_tags.elt_keywords), mk (html_of_keywords stog) ;
      ("", Stog_tags.elt_src), mk f_src ;
      ("", Stog_tags.elt_title), mk f_title ;
      ("", Stog_tags.elt_topics), mk (html_of_topics stog) ;
      ("", Stog_tags.elt_type), mk f_type ;
      ("", Stog_tags.elt_url), mk f_url ;
      ("", Stog_tags.ext_a), fun_exta ;
      ("", Stog_tags.graph), fun_graph stog ;
      ("", Stog_tags.hcode), fun_hcode stog ~inline: false ?lang: None;
      ("", Stog_tags.icode), fun_icode ?lang: None stog;
      ("", Stog_tags.if_), fun_if ;
      ("", Stog_tags.image), fun_image ;
      ("", Stog_tags.include_), fun_include stog elt ;
      ("", Stog_tags.latex), (Stog_latex.fun_latex stog) ;
      ("", Stog_tags.list), fun_list ;
      ("", Stog_tags.n_columns), fun_ncolumns ;
      ("", Stog_tags.next), (fun _ _ _ -> next);
      ("", Stog_tags.ocaml), fun_ocaml ~inline: false stog;
      ("", Stog_tags.ocaml_eval), Stog_ocaml.fun_eval stog ;
      ("", Stog_tags.previous), (fun _ _ _ -> previous);
      ("", Stog_tags.search_form), fun_search_form stog ;
      ("", Stog_tags.sep), (fun _ _ _ -> []);
      ("", Stog_tags.site_desc), (fun _ _ _ -> stog.stog_desc) ;
      ("", Stog_tags.site_email), (fun _ _ _ -> [ Xtmpl.D stog.stog_email ]) ;
      ("", Stog_tags.site_title), (fun _ _ _ -> [ Xtmpl.D stog.stog_title ]) ;
      ("", Stog_tags.site_url), fun_blog_url stog ;
      ("", Stog_tags.two_columns), fun_twocolumns ;
    ]
  in
  (make_lang_rules stog) @ l

and elt_list elt ?rss ?set stog env args _ =
  let report_error msg = Stog_msg.error ~info: "Stog_html.elt_list" msg in
  let elts =
    match set with
      Some set ->
        let l = Stog_types.Elt_set.elements set in
        List.map (fun id -> (id, Stog_types.elt stog id)) l
    | None ->
        let set = Xtmpl.get_arg args ("", "set") in
        Stog_types.elt_list ?set stog
  in
  let elts =
    match Xtmpl.get_arg args ("", "type") with
      None -> elts
    | Some typ ->
        List.filter (fun (_,elt) -> elt.elt_type = typ) elts
  in
  let max = Stog_misc.map_opt int_of_string
    (Xtmpl.get_arg args ("", "max"))
  in
  let reverse =
    match Xtmpl.get_arg args ("", "reverse") with
      None -> true
    | Some s -> Stog_io.bool_of_string s
  in
  let elts = Stog_types.sort_ids_elts_by_date elts in
  let elts = if reverse then List.rev elts else elts in
  let elts =
    match max with
      None -> elts
    | Some n -> Stog_misc.list_chop n elts
  in
  let tmpl =
    let file =
      match Xtmpl.get_arg args ("", "tmpl") with
        None ->  "elt-in-list.tmpl"
      | Some s -> s
    in
    Stog_tmpl.get_template stog ~elt Stog_tmpl.elt_in_list file
  in
  let f_elt (elt_id, elt) =
    let name = Stog_types.string_of_human_id elt.elt_human_id in
    let env = elt_env build_base_rules stog ~env elt_id elt in
    match Xtmpl.apply_to_xmls env [tmpl] with
      [xml] -> xml
    | _ ->
        report_error ("Error while processing " ^ name);
        assert false
  in
  let xml = List.map f_elt elts in
  (*prerr_endline "elt_list:";
  List.iter
    (fun xml -> prerr_endline (Printf.sprintf "ITEM: %s" (Xtmpl.string_of_xml xml)))
    xml;
     *)
  let rss =
    match rss with
      Some link -> Some link
    | None ->
        match Xtmpl.get_arg args ("", "rss") with
          None -> None
        | Some file ->
            let url = Printf.sprintf "%s/%s" stog.stog_base_url file in
            let file = Filename.concat stog.stog_outdir file in
            let title =Xtmpl.get_arg args ("", "title") in
            generate_rss_feed_file stog ?title url elts file;
            Some url
  in
  match rss with
    None -> xml
  | Some link ->
      (Xtmpl.E (("", "div"), [("", "class"), "rss-button"], [
          Xtmpl.E (("", "a"), [("", "href"), link], [
             Xtmpl.E (("", "img"), [("", "src"), "rss.png" ; ("", "alt"), "Rss feed"], [])]) ;
        ])
      ) :: xml
;;

let apply_stage0_funs stog =
  Stog_msg.verbose "Calling stage 0 functions";
  List.fold_right (fun f stog -> f stog) !stage0_funs stog
;;

module Sset = Set.Make (struct type t = string let compare = Pervasives.compare end);;


let compute_elt build_rules env stog elt_id elt =
  Stog_msg.verbose ~level:2
  (Printf.sprintf "Computing %s %S"
    elt.elt_type (Stog_types.string_of_human_id elt.elt_human_id));
  let xmls =
    match elt.elt_out with
      None ->
        let tmpl =
           let default =
             match elt.elt_type with
              "by-topic" -> Stog_tmpl.by_topic
            | "by-keyword" -> Stog_tmpl.by_keyword
            | "by-month" -> Stog_tmpl.by_month
            | _ -> Stog_tmpl.page
          in
          Stog_tmpl.get_template stog ~elt default (elt.elt_type^".tmpl")
        in
        [tmpl]
    | Some xmls ->
        xmls
  in
  let env = elt_env build_rules stog ~env elt_id elt in
  let result = Xtmpl.apply_to_xmls env xmls in
  { elt with elt_out = Some result }
;;

let make_by_word_indexes stog env f_elt_id elt_type map =
  let f word set stog =
    let hid = f_elt_id word in
    let elt =
      { Stog_types.elt_human_id = hid ;
        elt_type = elt_type ;
        elt_body = [] ;
        elt_date = None ;
        elt_title = word ;
        elt_keywords = [] ;
        elt_topics = [] ;
        elt_published = true ;
        elt_defs = [] ;
        elt_src = Printf.sprintf "%s.html" (Stog_types.string_of_human_id hid) ;
        elt_sets = [] ;
        elt_lang_dep = true ;
        elt_xml_doctype = None ;
        elt_out = None ;
        elt_used_mods = Stog_types.Str_set.empty ;
      }
    in
    let out_file = elt_dst_file stog elt in
    let rss_file = (Filename.chop_extension out_file)^".rss" in
    let url = elt_url stog elt in
    let rss_url = (Filename.chop_extension url)^".rss" in
    generate_rss_feed_file stog ~title: word url
      (List.map (fun id -> (id, Stog_types.elt stog id)) (Stog_types.Elt_set.elements set))
      rss_file;
    let elt =
      { elt with Stog_types.elt_body = elt_list elt ~set ~rss: rss_url stog env [] []}
    in
    Stog_types.add_elt stog elt
  in
  Stog_types.Str_map.fold f map stog
;;

let make_topic_indexes stog env =
  make_by_word_indexes stog env topic_index_hid
  "by-topic" stog.stog_elts_by_topic

;;

let make_keyword_indexes stog env =
  make_by_word_indexes stog env keyword_index_hid
  "by-keyword" stog.stog_elts_by_kw
;;

let make_archive_index stog env =
  let f_month year month set stog =
    let hid = month_index_hid ~year ~month in
    let title =
      let month_str = Stog_intl.get_month stog.stog_lang month in
      Printf.sprintf "%s %d" month_str year
    in
    let elt =
      { Stog_types.elt_human_id = hid ;
        elt_type = "by-month";
        elt_body = [] ;
        elt_date = None ;
        elt_title = title ;
        elt_keywords = [] ;
        elt_topics = [] ;
        elt_published = true ;
        elt_defs = [] ;
        elt_src = Printf.sprintf "%s.html" (Stog_types.string_of_human_id hid) ;
        elt_sets = [] ;
        elt_lang_dep = true ;
        elt_xml_doctype = None ;
        elt_out = None ;
        elt_used_mods = Stog_types.Str_set.empty ;
      }
    in
    let elt = { elt with elt_body = elt_list elt ~set stog env [] [] } in
    Stog_types.add_elt stog elt
  in
  let f_year year mmap stog =
    Stog_types.Int_map.fold (f_month year) mmap stog
  in
  Stog_types.Int_map.fold f_year stog.stog_archives stog
;;


(*
let output_elt_cache stog elt =
  let cache_file = Filename.concat stog.stog_cache_dir elt.elt_src in
  let hid = Stog_types.string_of_human_id elt.elt_human_id in
  let cache_dir = Filename.dirname cache_file in
  Stog_misc.safe_mkdir cache_dir ;
  let t = {
      cache_elt = elt ;
      cache_blocks = try Smap.find hid !blocks with Not_found -> Smap.empty ;
    }
  in
  let oc = open_out_bin cache_file in
  output_value oc t;
  close_out oc
;;
*)

let output_elt stog env elt =
  let file = elt_dst_file stog elt in
  Stog_misc.safe_mkdir (Filename.dirname file);
  match elt.elt_out with
    None ->
      failwith (Printf.sprintf "Element %S not computed!" (Stog_types.string_of_human_id elt.elt_human_id))
  | Some xmls ->
      let oc = open_out file in
      let doctype =
        match elt.elt_xml_doctype with
          None -> "HTML"
        | Some s -> s
      in
      Printf.fprintf oc "<!DOCTYPE %s>\n" doctype;
      List.iter (fun xml -> output_string oc (Xtmpl.string_of_xml xml)) xmls;
      close_out oc;
      Stog_cache.set_elt_env elt stog env;
      Stog_cache.apply_storers stog elt
;;

let output_elts ?elts stog env =
  begin
    match elts with
      None -> Stog_tmap.iter (fun _ elt -> output_elt stog env elt) stog.stog_elts
    | Some l -> List.iter (output_elt stog env) l
  end;
  Stog_cache.output_cache_info stog
;;

let copy_other_files stog =
  let report_error msg = Stog_msg.error ~info: "Stog_html.copy_other_files" msg in
  let copy_file src dst =
    let com = Printf.sprintf "cp -f %s %s" (Filename.quote src) (Filename.quote dst) in
    match Sys.command com with
      0 -> ()
    | n ->
        let msg = Printf.sprintf "Command failed [%d]: %s" n com in
        report_error msg
  in
  let f_file dst path name =
    let dst = Filename.concat dst name in
    let src = Filename.concat path name in
    copy_file src dst
  in
  let rec f_dir dst path name t =
    let dst = Filename.concat dst name in
    let path = Filename.concat path name in
    Stog_misc.safe_mkdir dst;
    iter dst path t
  and iter dst path t =
    Stog_types.Str_set.iter (f_file dst path) t.files ;
    Stog_types.Str_map.iter (f_dir dst path) t.dirs
  in
  iter stog.stog_outdir stog.stog_dir stog.stog_files
;;

type rule_build =
  Stog_types.stog -> Stog_types.elt_id -> Stog_types.elt -> (Xmlm.name * Xtmpl.callback) list
type level_fun =
  Xtmpl.env -> Stog_types.stog -> Stog_types.elt_id -> Stog_types.elt -> Stog_types.elt
;;

type level_fun_on_elt_list =
  Xtmpl.env -> Stog_types.stog -> (Stog_types.elt_id * Stog_types.elt) list ->
  (Stog_types.elt_id * Stog_types.elt) list
;;

module Intmap = Map.Make (struct type t = int let compare = Pervasives.compare end);;
module Set = Set.Make (struct type t = string let compare = Pervasives.compare end);;

type level_fun_kind =
  On_elt of level_fun
| On_elt_list of level_fun_on_elt_list
;;

let levels = ref (Intmap.empty : level_fun_kind list Intmap.t);;

let register_level_fun_kind level k =
  let l =
    try Intmap.find level !levels
    with Not_found -> []
  in
  levels := Intmap.add level (k::l) !levels
;;
let register_level_fun level f =
  register_level_fun_kind level (On_elt f);;
let register_level_fun_on_elt_list level f =
  register_level_fun_kind level (On_elt_list f);;


let compute_level ?elts env level (funs: level_fun_kind list) stog =
  Stog_msg.verbose (Printf.sprintf "Computing level %d" level);
  let f_elt f stog (elt_id, elt) =
    let elt = f env stog elt_id elt in
    Stog_types.set_elt stog elt_id elt
  in
  let elts =
    match elts with
      None ->
        Stog_tmap.fold (fun elt_id elt acc -> (elt_id, elt) :: acc) stog.stog_elts []
    | Some l ->
        List.map
        (fun elt_id -> (elt_id, Stog_types.elt stog elt_id))
        l
  in
  let f_fun stog f =
    match f with
      On_elt f -> List.fold_left (f_elt f) stog elts
    | On_elt_list f ->
        let modified = f env stog elts in
        List.fold_left (fun stog (elt_id, elt) -> Stog_types.set_elt stog elt_id elt) stog modified
  in
  List.fold_left f_fun stog funs
;;

(*
let load_cached_elt file =
  let ic = open_in_bin file in
  let (t : Stog_types.cached_elt) = input_value ic in
  close_in ic;
  let hid = Stog_types.string_of_human_id t.cache_elt.elt_human_id in
  blocks := Smap.add hid t.cache_blocks !blocks;
  t.cache_elt
;;
*)


(** This is list is filled by the {!Cache.load} function.
  After computing cached elements, this list and [cached] in
  {!get_cached_elements} must have the same length and order. *)
let cached_elements = ref [];;

let compute_levels ?(use_cache=true) ?elts env stog =
  if use_cache then
    begin
      let (cached, not_cached) = Stog_cache.get_cached_elements stog env in
      Stog_msg.verbose (Printf.sprintf "%d elements read from cache" (List.length cached));
      let stog = List.fold_left2
        (fun stog elt_id elt -> Stog_types.set_elt stog elt_id elt)
        stog cached !cached_elements
      in
      Intmap.fold (compute_level ~elts: not_cached env) !levels stog
    end
  else
    Intmap.fold (compute_level ?elts env) !levels stog
;;

let generate ?(use_cache=true) ?only_elt stog =
  begin
    match stog.stog_lang with
      None -> ()
    | Some lang -> Stog_msg.verbose (Printf.sprintf "Generating pages for language %s" lang);
  end;
  let stog = apply_stage0_funs stog in
  current_stog := Some stog;
  Stog_misc.safe_mkdir stog.stog_outdir;
  let env = env_of_defs stog.stog_defs in
  let env = env_of_used_mods stog ~env stog.stog_used_mods in
  match only_elt with
    None ->
      let stog = make_topic_indexes stog env in
      let stog = make_keyword_indexes stog env in
      let stog = make_archive_index stog env in
      let stog = compute_levels ~use_cache env stog in
      Stog_ocaml.close_sessions();
      output_elts stog env;
      copy_other_files stog
  | Some s ->
      let hid = Stog_types.human_id_of_string s in
      let (elt_id, _) = Stog_types.elt_by_human_id stog hid in
      let stog = compute_levels ~use_cache ~elts: [elt_id] env stog in
      let (_, elt) = Stog_types.elt_by_human_id stog hid in
      output_elts ~elts: [elt] stog env
;;

(* register default levels *)

let rules_0 = build_base_rules ;;

let get_sectionning_tags stog elt =
  let spec =
    match Stog_types.get_def elt.elt_defs ("", "sectionning") with
      Some x -> Some x
    | None ->
        Stog_types.get_def stog.stog_defs ("", "sectionning")
  in
  match spec with
    None -> [("",Stog_tags.section) ; ("", Stog_tags.subsection)]
  | Some (_,xmls) ->
     let s = Xtmpl.string_of_xmls xmls in
     let l = Stog_misc.split_string s [',' ; ';'] in
     let strip = Stog_misc.strip_string in
      List.fold_right
        (fun s acc ->
           match Stog_misc.split_string s [':'] with
             [] -> acc
           | [s] -> ("", strip s) :: acc
           | [pref ; s] -> (strip pref, strip s) :: acc
           | pref :: q -> (strip pref, strip (String.concat ":" q)) :: acc
        )
      l []
;;

let rules_toc stog elt_id elt =
  let tags = get_sectionning_tags stog elt in
  [ ("", Stog_tags.prepare_toc), (fun_prepare_toc tags);
    ("", Stog_tags.toc), fun_toc ;
  ]
;;

let rules_sectionning stog elt_id elt =
  let tags = get_sectionning_tags stog elt in
  let rec f acc up = function
    [] -> acc
  | tag :: rest ->
    let rule = (tag, make_fun_section up tag rest) in
    f (rule :: acc) (tag :: up) rest
  in
  let rules = f [] [] tags in
  (("", Stog_tags.counter), fun_counter)::
  (("", Stog_tags.block), fun_block1 stog) :: rules
;;

let gather_existing_ids =
  let rec f hid set = function
    Xtmpl.D _ -> set
  | Xtmpl.E (tag, atts, subs) ->
      let set =
        match Xtmpl.get_arg atts ("", "id") with
          None
        | Some "" -> set
        | Some id ->
            if Sset.mem id set then
              failwith (Printf.sprintf
               "id %S defined twice in the same element %S (here for tag %S)" id
               (Stog_types.string_of_human_id hid) (concat_name tag))
            else
               Sset.add id set
      in
      List.fold_left (f hid) set subs
  in
  fun env stog elt_id elt ->
    match elt.elt_out with
      None -> elt
    | Some body ->
        let set =
          let g set xml =
            try f elt.elt_human_id set xml
            with e ->
                prerr_endline (Xtmpl.string_of_xml xml);
                raise e
          in
          List.fold_left g Sset.empty body
        in
        let title = Xtmpl.xml_of_string elt.elt_title in
        let hid = Stog_types.string_of_human_id elt.elt_human_id in
        Sset.iter
          (fun id -> add_block ~on_dup: `Ignore ~hid ~id ~short: title ~long: title ()) set;
        elt
;;

let rules_fun_elt stog elt_id elt =
  [ ("", Stog_tags.elt), fun_elt elt stog ;
    ("", Stog_tags.post), fun_post elt stog ;
    ("", Stog_tags.page), fun_page elt stog ;
    ("", Stog_tags.block), fun_block2 ;
  ] @ (build_base_rules stog elt_id elt)
;;

let rules_inc_elt stog elt_id elt =
  [ ("", Stog_tags.inc), fun_inc stog elt ]
;;

let () = register_level_fun 0 (compute_elt rules_0);;
let () = register_level_fun 50 (compute_elt rules_toc);;
let () = register_level_fun 100 (compute_elt rules_sectionning);;
let () = register_level_fun 120 (gather_existing_ids);;
let () = register_level_fun 150 (compute_elt rules_fun_elt);;
let () = register_level_fun 160 (compute_elt rules_inc_elt);;

module Cache = struct
  type t =
      { cache_elt : elt ;
        cache_blocks : (Xtmpl.tree * Xtmpl.tree) Str_map.t ;
      }

  let name = Stog_cache.stog_cache_name
  let load elt t =
    let hid = Stog_types.string_of_human_id t.cache_elt.elt_human_id in
    blocks := Smap.add hid t.cache_blocks !blocks;
    cached_elements := t.cache_elt :: !cached_elements

  let store elt =
    let hid = Stog_types.string_of_human_id elt.elt_human_id in
    {
      cache_elt = elt ;
      cache_blocks = try Smap.find hid !blocks with Not_found -> Smap.empty ;
    }
end;;

let () = Stog_cache.register_cache (module Cache);;
