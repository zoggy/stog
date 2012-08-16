(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012 Maxence Guesdon. All rights reserved.                   *)
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

let languages = ["fr" ; "en" ];;

let current_stog = ref None;;
let plugin_funs = ref [];;

let url_compat s =
 let s = Stog_misc.lowercase s in
 for i = 0 to String.length s - 1 do
   match s.[i] with
     'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' | '.' -> ()
    | _  -> s.[i] <- '+'
 done;
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
(*
let html_file stog name =
  let ext_pref =
    match stog.stog_lang with
      None -> ""
    | Some lang -> "."^lang
  in
  Printf.sprintf "%s%s.html" name ext_pref
;;
*)
let tag_sep = "sep_";;

let elt_dst f_concat stog base elt =
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
  f_concat base dst
;;

let elt_dst_file stog elt = elt_dst Filename.concat stog stog.stog_outdir elt;;
let elt_url stog elt = elt_dst (fun a b -> a^"/"^b) stog stog.stog_base_url elt;;

let url_of_hid stog ?ext hid =
  let elt = Stog_types.make_elt ~hid () in
  let src =
    Printf.sprintf "%s%s" (Stog_types.string_of_human_id hid)
      (match ext with None -> "" | Some s -> "."^s)
  in
  elt_url stog { elt with Stog_types.elt_src = src }
;;

(*
let link_to ?(from=`Article) file =
  let pref = match from with
      `Article -> "../"
    | `Index -> ""
  in
  Printf.sprintf "%s%s" pref file
;;

let link_to_article stog ?(from=`Article) ?id article =
  link_to ~from
  (Printf.sprintf "%s/%s%s"
   article.art_human_id (html_file stog "index")
   (match id with None -> "" | Some s -> "#"^s)
  )
;;
*)

let topic_index_hid topic = Stog_types.human_id_of_string ("/topic_"^topic);;
let keyword_index_hid kw = Stog_types.human_id_of_string ("/kw_"^ kw);;
let month_index_hid ~year ~month =
  Stog_types.human_id_of_string (Printf.sprintf "/%04d_%02d" year month);;

(*
let page_file stog page = html_file stog page.page_human_id;;
*)
let make_lang_funs stog =
  match stog.stog_lang with
    None -> []
  | Some lang ->
      let to_remove = List.filter ((<>) lang) languages in
      let f_keep _env _args subs = subs in
      let f_remove _env _args _subs = [] in
      (lang, f_keep) ::
      (List.map (fun lang -> (lang, f_remove)) to_remove)
;;

let fun_include tmpl_dir _env args subs =
  match Xtmpl.get_arg args "file" with
    None -> failwith "Missing 'file' argument for include command";
  | Some file ->
      let file =
        if Filename.is_relative file then
          Filename.concat tmpl_dir file
        else
          file
      in
      let xml =
        match Xtmpl.get_arg args "raw" with
        | Some "true" -> [Xtmpl.D (Stog_misc.string_of_file file)]
        | _ -> [Xtmpl.xml_of_string (Stog_misc.string_of_file file)]
      in
      let args =
        ("contents", String.concat "" (List.map Xtmpl.string_of_xml subs)) ::
        args
      in
      [Xtmpl.T (Xtmpl.tag_env, args, xml)]
;;

let fun_image _env args legend =
  let width = Xtmpl.opt_arg args "width" in
  let src = Xtmpl.opt_arg args "src" in
  let cls = Printf.sprintf "img%s"
    (match Xtmpl.get_arg args "float" with
       Some "left" -> "-float-left"
     | Some "right" -> "-float-right"
     | Some s -> failwith (Printf.sprintf "unhandled image position: %s" s)
     | None -> ""
    )
  in
  let pred (s,_) = not (List.mem s ["width" ; "src" ; "float"]) in
  let atts = List.filter pred args in
  [
    Xtmpl.T ("div", [ "class", cls ],
     (Xtmpl.T ("img", [ "class", "img" ; "src", src; "width", width ] @ atts, [])) ::
     (match legend with
        [] -> []
      | xml -> [ Xtmpl.T ("div", ["class", "legend"], xml) ]
     )
    )
  ]
;;

let fun_elt_href ?typ href stog env args subs =
  let (elt, id, text) =
    let (hid, id) =
      try
        let p = String.index href '#' in
        let len = String.length href in
        (String.sub href 0 p, Some (String.sub href (p+1) (len - (p+1))))
      with
        Not_found -> (href, None)
    in
    let elt =
      try
        let hid = Stog_types.human_id_of_string hid in
        let (_, elt) = Stog_types.elt_by_human_id ?typ stog hid in
        Some elt
      with
        Failure s ->
          Stog_msg.error s;
          None
    in
    let text =
      match elt, subs with
        None, _ -> [Xtmpl.D "??"]
      | Some elt, [] -> [Xtmpl.D (Printf.sprintf "\"%s\"" elt.elt_title)]
      | Some _, text -> text
    in
    (elt, id, text)
  in
  match elt with
    None -> [Xtmpl.T ("span", ["class", "unknown-ref"], text)]
  | Some elt ->
      [
        Xtmpl.T ("a", ["href", (elt_url stog elt)], text)
      ]
;;

let fun_elt ?typ stog env args subs =
  let href =
    match Xtmpl.get_arg args "href" with
      None ->
        let msg = Printf.sprintf "Missing href for <%s>"
          (match typ with None -> "elt" | Some s -> s)
        in
        failwith msg
    | Some s -> s
  in
  fun_elt_href ?typ href stog env args subs
;;

let fun_post = fun_elt ~typ: "post";;
let fun_page = fun_elt ~typ: "page";;

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
    Xtmpl.T ("li", [], [
       Xtmpl.T ("a", ["href", href], [ Xtmpl.D month_str ]) ;
       Xtmpl.D (Printf.sprintf "(%d)" (Stog_types.Elt_set.cardinal set))
     ]
    )
  in
  let f_year (year, data) =
    Xtmpl.T ("li", [], [
       Xtmpl.D (string_of_int year) ;
       Xtmpl.T ("ul", [], List.map (f_mon year) data) ;
      ]
    )
  in
  [ Xtmpl.T ("ul", [], List.map f_year years) ]
;;

let fun_rss_feed file args _env _ =
  [
    Xtmpl.T ("link",
     [ "href", file ; "type", "application/rss+xml" ; "rel", "alternate" ; "title", "RSS feed"],
     [])
  ]
;;

let highlight ~opts code =
  let code_file = Filename.temp_file "stog" "code" in
  Stog_misc.file_of_string ~file: code_file code;
  let temp_file = Filename.temp_file "stog" "highlight" in
  let com = Printf.sprintf
    "highlight -O xhtml %s -f %s > %s"
    opts (Filename.quote code_file)(Filename.quote temp_file)
  in
  match Sys.command com with
    0 ->
      let code = Stog_misc.string_of_file temp_file in
      Sys.remove code_file;
      Sys.remove temp_file;
      code
  | _ ->
      failwith (Printf.sprintf "command failed: %s" com)
;;

let fun_hcode ?(inline=false) ?lang stog _env args code =
  let language, language_options =
    match lang with
      None ->
        (
         match Xtmpl.get_arg args "lang-file" with
           None ->
             begin
               let lang = Xtmpl.opt_arg args ~def: "txt" "lang" in
               match lang with
                 "txt" -> (lang, None)
               | _ -> (lang, Some (Printf.sprintf "--syntax=%s" lang))
             end
         | Some f ->
             let lang = Xtmpl.opt_arg args ~def: "" "lang" in
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
    match code with
      [ Xtmpl.D code ] -> code
    | _ ->
       String.concat "" (List.map Xtmpl.string_of_xml code)
    (*failwith (Printf.sprintf "Invalid code: %s"
         (String.concat "" (List.map Xtmpl.string_of_xml code)))*)
  in
  let code = Stog_misc.strip_string code in
  let xml_code =
    match language_options with
      None -> Xtmpl.D code
    | Some opts ->
        let code = highlight ~opts code in
        Xtmpl.xml_of_string code
  in
  if inline then
    [ Xtmpl.T ("span", ["class","icode"], [xml_code]) ]
  else
    [ Xtmpl.T ("pre",
       ["class", Printf.sprintf "code-%s" language], [xml_code])
    ]
;;

let fun_ocaml = fun_hcode ~lang: "ocaml";;
let fun_command_line = fun_hcode ~lang: "sh";;
let fun_icode = fun_hcode ~inline: true ;;

let fun_section cls _env args body =
  let id =
    match Xtmpl.get_arg args "name" with
      None -> []
    | Some name -> ["id", name]
  in
  let title =
    match Xtmpl.get_arg args "title" with
      None -> []
    | Some t ->
        [Xtmpl.T ("div", ["class", cls^"-title"] @ id, [Xtmpl.xml_of_string t])]
  in
  [ Xtmpl.T ("div", ["class", cls], title @ body) ]
;;

let fun_subsection = fun_section "subsection";;
let fun_section = fun_section "section";;

let fun_search_form stog _env _ _ =
  let tmpl = Filename.concat stog.stog_tmpl_dir "search.tmpl" in
  [ Xtmpl.xml_of_string (Stog_misc.string_of_file tmpl) ]
;;

let fun_blog_url stog _env _ _ = [ Xtmpl.D stog.stog_base_url ];;

let fun_graph =
  let generated = ref false in
  fun stog _env _ _ ->
    let png_name = "site-graph.png" in
    let svg_file = (Filename.chop_extension png_name) ^ ".svg" in
    let src = Printf.sprintf "%s/%s" stog.stog_base_url svg_file in
    let small_src = Printf.sprintf "%s/small-%s" stog.stog_base_url png_name in
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
                  (Filename.quote (Filename.concat stog.stog_outdir small_src))
                in
                match Sys.command com with
                  0 -> ()
                | _ ->
                    Stog_msg.error (Printf.sprintf "Command failed: %s" com)
              end
          | _ ->
              Stog_msg.error (Printf.sprintf "Command failed: %s" com)
    end;
    [
      Xtmpl.T ("a", ["href", src], [
         Xtmpl.T ("img", ["src", small_src ; "alt", "Graph"], [])
       ])
    ]
;;

let fun_if env args subs =
  let pred (att, v) =
    let node = Printf.sprintf "<%s/>" att in
    let s = Xtmpl.apply env node in
    (*prerr_endline (Printf.sprintf "fun_if: pred: att=%s, s=%S, v=%s" att s v);*)
    let s = if s = node then "" else s in
    s = v
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

let generate_page stog env contents =
  let tmpl = Filename.concat stog.stog_tmpl_dir "page.tmpl" in
  let f env args body = contents in
  let env = Xtmpl.env_of_list ~env ["contents", f] in
  Xtmpl.apply env (Stog_misc.string_of_file tmpl)
;;

let env_add_langswitch env stog elt =
  let name = "langswitch" in
  match stog.stog_lang with
    None ->
      Xtmpl.env_add name (fun _ _ _ -> []) env
  | Some lang ->
      let map_lang lang =
         let url = elt_url { stog with stog_lang = Some lang } elt in
         let img_url = Printf.sprintf "%s/%s.png" stog.stog_base_url lang in
         Xtmpl.T ("a", ["href", url], [
           Xtmpl.T ("img", ["src", img_url ; "title", lang ; "alt", lang], [])])
      in
      let f _env args _subs =
        let languages =
          match Xtmpl.get_arg args "languages" with
            Some s -> Stog_misc.split_string s [','; ';' ; ' ']
          | None -> languages
        in
        let languages = List.filter ((<>) lang) languages in
        List.map map_lang languages
      in
      Xtmpl.env_add name f env
;;

let fun_twocolumns env args subs =
  (*prerr_endline (Printf.sprintf "two-columns, length(subs)=%d" (List.length subs));*)
  let empty = [] in
  let subs = List.fold_right
    (fun xml acc ->
       match xml with
         Xtmpl.D _ -> acc
       | Xtmpl.T (_,_,subs)
       | Xtmpl.E (_, subs) -> subs :: acc
    ) subs []
  in
  let left, right =
    match subs with
      [] -> empty, empty
    | [left] -> left, empty
    | left :: right :: _ -> left, right
  in
  [ Xtmpl.T ("table", ["class", "two-columns"],
     [ Xtmpl.T ("tr", [],
        [ Xtmpl.T ("td", ["class", "two-columns-left"], left) ;
          Xtmpl.T ("td", ["class", "two-columns-right"], right) ;
        ]);
     ])
  ]
;;

let fun_exta env args subs =
  [ Xtmpl.T ("span", ["class","ext-a"],
     [ Xtmpl.T ("a",args, subs) ])
  ]
;;

type toc = Toc of string * string * string * toc list (* name, title, class, subs *)

let fun_prepare_toc env args subs =
  let depth =
    match Xtmpl.get_arg args "depth" with
      None -> max_int
    | Some s -> int_of_string s
  in
  let rec iter d acc = function
  | Xtmpl.D _ -> acc
  | Xtmpl.T ("section" as cl, atts, subs)
  | Xtmpl.T ("subsection" as cl, atts, subs) ->
      begin
        match Xtmpl.get_arg atts "name", Xtmpl.get_arg atts "title" with
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
               (Toc (name, title, cl, subs)) :: acc
              )
      end
  | Xtmpl.T (_,_,subs) -> List.fold_left (iter d) acc subs
  | Xtmpl.E (tag, subs) ->
      match tag with
      | (("", t), atts) ->
          iter d acc (Xtmpl.T (t, (List.map (fun ((_,a),v) -> (a, v)) atts), subs))
      | _ -> acc
  in
  let toc = List.rev (List.fold_left (iter 1) [] subs) in
  (*(
   match toc with
     [] -> prerr_endline "empty toc!"
   | _ -> prerr_endline (Printf.sprintf "toc is %d long" (List.length toc));
  );*)
  let rec xml_of_toc = function
    Toc (name, title, cl, subs) ->
      Xtmpl.T ("li", ["class", "toc-"^cl],
       [ Xtmpl.T ("a", ["href", "#"^name],
         [ Xtmpl.xml_of_string title ]) ]
       @
       ( match subs with
          [] -> []
        | _ ->
               [ Xtmpl.T ("ul", ["class", "toc"], List.map xml_of_toc subs) ]
       )
       )
  in
  let xml = Xtmpl.T ("ul", ["class", "toc"], List.map xml_of_toc toc) in
  let atts = [ "toc-contents", Xtmpl.string_of_xml xml ] in
  [ Xtmpl.T (Xtmpl.tag_env, atts, subs) ]
;;

let fun_toc env args subs =
  subs @ [Xtmpl.T ("toc-contents", [], [])]
;;

(*
let rec fun_page_id hid outdir stog env args subs =
  let page = get_page stog hid in
  let file = Filename.concat outdir (page_file stog page) in
  let xml = Xtmpl.xml_of_string page.page_body in
  let env = Xtmpl.env_of_list ~env
    (List.map
     (fun (key, value) ->
       (key, fun _ _ _ -> [Xtmpl.xml_of_string value]))
     page.page_vars
    )
  in
  let env = Xtmpl.env_of_list ~env (default_commands ~outdir ~from: `Index stog) in
  let env = env_add_langswitch env stog file in
  let env = List.fold_left
    (fun env (s,v) -> Xtmpl.env_add_att s v env)
    env
    ((Stog_cst.page_title, page.page_title) :: args)
  in
  let s = generate_page stog env [xml] in
  Xtmpl.apply_string_to_file ~head: "<!DOCTYPE html>" env s file;
  []

and fun_page outdir stog env args subs =
    match Xtmpl.get_arg args "id" with
    | Some hid -> fun_page_id hid outdir stog env args subs
    | None ->
      match Xtmpl.get_arg args "href" with
      | Some id -> fun_page_ref id stog env args subs
      | None ->
          let info =
            match subs with
              [] -> "<no children>"
            | l -> String.concat "" (List.map Xtmpl.string_of_xml subs)
          in
          failwith ("Missing id or href for <page>: "^info)
*)
let default_commands ?rss stog =
  let l =
    !plugin_funs @
    [
      "if", fun_if ;
      "include", fun_include stog.stog_tmpl_dir ;
      "image", fun_image ;
      "archive-tree", (fun _ -> fun_archive_tree stog) ;
      "hcode", fun_hcode stog ~inline: false ?lang: None;
      "icode", fun_icode ?lang: None stog;
      "ocaml", fun_ocaml ~inline: false stog;
      "command-line", fun_command_line ~inline: false stog ;
      "post", fun_post stog;
      "section", fun_section ;
      "subsection", fun_subsection ;
      "rssfeed", (match rss with None -> fun _env _ _ -> [] | Some file -> fun_rss_feed file);
      Stog_cst.site_url, fun_blog_url stog ;
      "search-form", fun_search_form stog ;
      Stog_cst.site_title, (fun _ _ _ -> [ Xtmpl.D stog.stog_title ]) ;
      Stog_cst.site_desc, (fun _ _ _ -> stog.stog_desc) ;
      "two-columns", fun_twocolumns ;
      "ext-a", fun_exta ;
      "prepare-toc", fun_prepare_toc ;
      "toc", fun_toc ;
      "graph", fun_graph stog ;
      "page", (fun_page stog) ;
      "latex", (Stog_latex.fun_latex stog) ;
    ]
  in
  (make_lang_funs stog) @ l
;;

let intro_of_elt stog elt =
  let rec iter acc = function
    [] -> raise Not_found
  | (Xtmpl.T ("sep_",_,_)) :: _
  | (Xtmpl.E (((_,"sep_"),_),_)) :: _ -> List.rev acc
  | h :: q -> iter (h::acc) q
  in
  try
    let xml = iter [] elt.elt_body in
    xml @
    [
      Xtmpl.T ("a", ["href", elt_url stog elt],
       [ Xtmpl.T ("img", [ "src", Printf.sprintf "%s/next.png" stog.stog_base_url; "alt", "next"], [])])
    ]
  with
    Not_found -> elt.elt_body
;;

let rss_date_of_date d =
  let {year; month; day} = d in
  {
    Rss.year = year ; month ; day;
    hour = 8 ; minute = 0 ; second = 0 ;
    zone = 0 ; week_day = -1 ;
  }
;;

let elt_to_rss_item stog elt =
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
  let desc = intro_of_elt stog elt in
  let desc =
    Xtmpl.apply_to_xmls
    (Xtmpl.env_of_list (default_commands stog))
    desc
  in
  let desc = String.concat "" (List.map Xtmpl.string_of_xml desc) in
  Rss.item ~title: elt.elt_title
  ~desc
  ~link
  ~pubdate
  ~cats
  ~guid: { Rss.guid_name = link ; guid_permalink = true }
  ()
;;

let generate_rss_feed_file stog ?title link elts file =
  let elts = List.rev (Stog_types.sort_elts_by_date elts) in
  let elts = List.filter
    (fun elt -> match elt.elt_date with None -> false | _ -> true)  elts
  in
  let items = List.map (elt_to_rss_item stog) elts in
  let title = Printf.sprintf "%s%s"
    stog.stog_title
    (match title with None -> "" | Some t -> Printf.sprintf ": %s" t)
  in
  let pubdate =
    match elts with
      [] -> None
    | h :: _ ->
        Some (rss_date_of_date
         (match h.elt_date with None -> assert false | Some d -> d))
  in
  let image =
    try
      let file = List.assoc "rss-image" stog.stog_vars in
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
;;

let copy_file ?(ignerr=false) ?(quote_src=true) ?(quote_dst=true) src dest =
  let com = Printf.sprintf "cp -f %s %s"
    (if quote_src then Filename.quote src else src)
    (if quote_dst then Filename.quote dest else dest)
  in
  match Sys.command com with
    0 -> ()
  | _ ->
      let msg = Printf.sprintf "command failed: %s" com in
      if ignerr then Stog_msg.error msg else failwith msg
;;

let html_of_topics stog elt env args _ =
  let sep = Xtmpl.xml_of_string (Xtmpl.opt_arg args ~def: ", " "set") in
  let tmpl = Filename.concat stog.stog_tmpl_dir "topic.tmpl" in
  let f w =
    let env = Xtmpl.env_of_list ~env [ "topic", (fun _ _ _ -> [Xtmpl.D w]) ] in
    Xtmpl.xml_of_string (Xtmpl.apply_from_file env tmpl)
  in
  Stog_misc.list_concat ~sep
  (List.map (fun w ->
      let href = Stog_types.string_of_human_id (topic_index_hid w) in
      Xtmpl.T ("elt", ["href", href ], [ f w ]))
   elt.elt_topics
  )
;;

let html_of_keywords stog elt env args _ =
  let sep = Xtmpl.xml_of_string (Xtmpl.opt_arg args ~def: ", " "set") in
  let tmpl = Filename.concat stog.stog_tmpl_dir "keyword.tmpl" in
  let f w =
    let env = Xtmpl.env_of_list ~env [ "keyword", (fun _ _ _ -> [Xtmpl.D w]) ] in
    Xtmpl.xml_of_string (Xtmpl.apply_from_file env tmpl)
  in
  Stog_misc.list_concat ~sep
  (List.map (fun w ->
      let href = Stog_types.string_of_human_id (keyword_index_hid w) in
      Xtmpl.T ("a", ["href", href], [ f w ]))
   elt.elt_keywords
  )
;;

let remove_re s =
  let re = Str.regexp "^Re:[ ]?" in
  let rec iter s =
    let p =
      try Some (Str.search_forward re s 0)
      with Not_found -> None
    in
    match p with
      None -> s
    | Some p ->
        assert (p=0);
        let matched_len = String.length (Str.matched_string s) in
        let s = String.sub s matched_len (String.length s - matched_len) in
        iter s
  in
  iter s
;;

let escape_mailto_arg s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
      '&' -> Buffer.add_string b "%26"
    | ' ' -> Buffer.add_string b "%20"
    | '?' -> Buffer.add_string b "%3F"
    | '%' -> Buffer.add_string b "%25"
    | ',' -> Buffer.add_string b "%2C"
    | '\n' -> Buffer.add_string b "%0D%0A"
    | c -> Buffer.add_char b c
  done;
  Buffer.contents b
;;

let normalize_email s =
  let s2 =
    try
      let p = String.index s '<' in
      try
        let p1 = String.index_from s p '>' in
        String.sub s (p+1) (p1-p-1)
      with Not_found -> s
    with
      Not_found -> s
  in
  (*prerr_endline (Printf.sprintf "normalize(%s) = %s" s s2);*)
  s2
;;

(*
let build_mailto stog ?message article =
  let emails =
    match message with
      None -> [stog.stog_email]
    | Some message -> [stog.stog_email ; message.mes_from]
  in
  let emails =
    Stog_misc.list_remove_doubles
    (List.map normalize_email emails)
  in
  let hid = elt.elt_human_id in
  let subject =
    match message with
      None -> Printf.sprintf "%s [%s]" article.art_title (Stog_misc.md5 hid)
    | Some m ->
        Printf.sprintf "Re: %s [%s/%s]"
        (remove_re m.mes_subject)
        (Stog_misc.md5 hid) (Stog_misc.md5 m.mes_id)
  in
  let body = Stog_misc.string_of_file
    (Filename.concat stog.stog_tmpl_dir "comment_body.tmpl")
  in
  let mailto =
    Printf.sprintf
    "mailto:%s?subject=%s&amp;body=%s"
    (Stog_misc.encode_string (escape_mailto_arg (String.concat ", " emails)))
    (escape_mailto_arg subject)
    (escape_mailto_arg body)
  in
  mailto
;;


let html_comment_actions stog article message =
  let href = build_mailto stog ~message article in
  (*let href = Xtmpl.string_of_xml (Xtmpl.D href) in*)
  Xtmpl.T ("a", ["href",  href],
   [ Xtmpl.T ("img", [
        "src", "../comment_reply.png" ;
        "alt", "reply to comment" ;
        "title", "reply to comment" ; ], [])
   ]
  )
;;

let re_citation = Str.regexp "\\(\\(^&gt;[^\n]+\n\\)+\\)";;
let gen_id = let id = ref 0 in (fun () -> incr id; !id);;

let html_of_message_body body =
  let body = escape_html (Stog_misc.strip_string body) in
  let subst s =
    let id = gen_id () in
    let s = Str.matched_group 1 body in
    if Stog_misc.count_char s '\n' <= 2 then
      Printf.sprintf "<div class=\"comment-citation\">%s</div>" s
    else
      Printf.sprintf "<div class=\"comment-citation\" onclick=\"if(document.getElementById('comment%d').style.display=='none') {document.getElementById('comment%d').style.display='block';} else {document.getElementById('comment%d').style.display='none';}\">&gt; ... <img src=\"../expand_collapse.png\" alt=\"+/-\"/></div><div class=\"comment-expand\" id=\"comment%d\">%s</div>"
      id id id id
      s
  in
  let body = Str.global_substitute re_citation subst body in
  Xtmpl.xml_of_string body
;;

let rec html_of_comments outdir stog article tmpl comments env _ _ =
  let f (Node (message, subs)) =
    let env = Xtmpl.env_of_list ~env
      ([
         "date", (fun _ _ _ -> [Xtmpl.D (Stog_date.mk_mail_date (Stog_date.since_epoch message.mes_time))]) ;
         "subject", (fun _ _ _ -> [Xtmpl.D message.mes_subject] );
         "from", (fun _ _ _ -> [Xtmpl.D message.mes_from ]);
         "to", (fun _ _ _ -> [ Xtmpl.D (String.concat ", " message.mes_to)]) ;
         "body", (fun _ _ _ -> [html_of_message_body message.mes_body]) ;
         "comment-actions", (fun _ _ _ -> [html_comment_actions stog article message]) ;
         "comments", html_of_comments outdir stog article tmpl subs ;
       ] @ (default_commands ~outdir ~from:`Index stog)
      )
    in
    Xtmpl.xml_of_string (Xtmpl.apply_from_file env tmpl)
  in
  List.map f comments
;;

let html_of_comments outdir stog article =
  let tmpl = Filename.concat stog.stog_tmpl_dir "comment.tmpl" in
  html_of_comments outdir stog article tmpl article.art_comments
;;
*)
(*
let generate_elt_page stog env contents =
  let tmpl = Filename.concat stog.stog_tmpl_dir (elt.elt_type^".tmpl") in
  let f env args body = contents in
  let env = Xtmpl.env_of_list ~env ["contents", f] in
  Xtmpl.apply env (Stog_misc.string_of_file tmpl)
;;
*)

let commands_of_elt stog elt =
  let url = elt_url stog elt in
  [
    Stog_cst.elt_title, (fun _ _ _ -> [ Xtmpl.D elt.elt_title ]) ;
    "elt-url", (fun _ _ _ -> [ Xtmpl.D url ]) ;
    "elt-body", (fun _ _ _ -> elt.elt_body);
    "elt-type", (fun _ _ _ -> [Xtmpl.D elt.elt_type]);
    "elt-src", (fun _ _ _ -> [Xtmpl.D elt.elt_src]);
    tag_sep, (fun _ _ _ -> []);
    Stog_cst.elt_date, (fun _ _ _ ->
       [ Xtmpl.D (Stog_intl.string_of_date_opt stog.stog_lang elt.elt_date) ]) ;
    "elt-keywords", html_of_keywords stog elt ;
    "elt-topics", html_of_topics stog elt ;
  ]
;;

let generate_elt stog env ?elt_id elt =
  Stog_msg.verbose
  (Printf.sprintf "Generating %S" (Stog_types.string_of_human_id elt.elt_human_id));
  let file = elt_dst_file stog elt in
  let tmpl = Filename.concat stog.stog_tmpl_dir elt.elt_type^".tmpl" in
(*  let art_dir = Filename.dirname html_file in*)
(*
  Stog_misc.mkdir art_dir;
  List.iter (fun f -> copy_file f art_dir) article.art_files;
*)
(*
  let comment_actions =
    let href = build_mailto stog article in
    (*let href = Xtmpl.string_of_xml (Xtmpl.D href) in*)
    [
      Xtmpl.T ("a", ["href", href],
       [Xtmpl.T ("img", [
            "src", "../comment.png" ;
            "alt", "Post a comment" ;
            "title", "Post a comment"], [])])
    ]
  in
*)
  let env = Xtmpl.env_of_list ~env
    (List.map
     (fun (key, value) ->
        (key, fun _ _ _ -> [Xtmpl.xml_of_string value]))
     elt.elt_vars
    )
  in
  let previous, next =
    let html_link elt =
      let href = elt_url stog elt in
      [ Xtmpl.T ("a", ["href", href], [ Xtmpl.D elt.elt_title ]) ]
    in
    let try_link key search =
      let fallback () =
        match elt_id with
          None -> []
        | Some elt_id ->
            match search stog elt_id with
            | None -> []
            | Some id -> html_link (Stog_types.elt stog id)
      in
      if not (List.mem_assoc key elt.elt_vars) then fallback ()
      else
        let hid = Stog_types.human_id_of_string (List.assoc key elt.elt_vars) in
        try
          let (_, elt) = Stog_types.elt_by_human_id stog hid in
          html_link elt
        with Failure s ->
          Stog_msg.warning s;
          fallback ()
    in
    (try_link "previous" Stog_info.pred_by_date,
     try_link "next" Stog_info.succ_by_date)
  in
  let env = Xtmpl.env_of_list ~env
    ([
        "next", (fun _ _ _ -> next);
       "previous", (fun _ _ _ -> previous);
     ] @
     (commands_of_elt stog elt) @
        (*
          "elt-navbar", fun _ _ _ -> [Xtmpl.D "true"] ;
       *)
     (default_commands stog))
  in
  let env = env_add_langswitch env stog elt in
  Stog_misc.safe_mkdir (Filename.dirname file);
  Xtmpl.apply_to_file ~head: "<!DOCTYPE HTML>" env tmpl file
;;


let elt_list ?rss ?set stog env args _ =
  let elts =
    match set with
      Some set ->
        let l = Stog_types.Elt_set.elements set in
        List.map (fun id -> (id, Stog_types.elt stog id)) l
    | None ->
        let set = Xtmpl.get_arg args "set" in
        Stog_types.elt_list ?set stog
  in
  let max = Stog_misc.map_opt int_of_string
    (Xtmpl.get_arg args "max")
  in
  let elts = List.rev (Stog_types.sort_ids_elts_by_date elts) in
  let elts =
    match max with
      None -> elts
    | Some n -> Stog_misc.list_chop n elts
  in
  let tmpl =
    let file =
      match Xtmpl.get_arg args "tmpl" with
        None -> "elt_list.tmpl"
      | Some s -> s
    in
    Filename.concat stog.stog_tmpl_dir file
  in
  let f_elt (elt_id, elt) =
    let env = Xtmpl.env_of_list ~env
      (( "elt-intro", (fun _ _ _ -> intro_of_elt stog elt)) ::
       (commands_of_elt stog elt)
       @ (default_commands stog))
    in
    Xtmpl.xml_of_string (Xtmpl.apply_from_file env tmpl)
  in
  let xml = List.map f_elt elts in
  match rss with
    None -> xml
  | Some link ->
      (Xtmpl.T ("div", ["class", "rss-button"], [
          Xtmpl.T ("a", ["href", link], [
             Xtmpl.T ("img", ["src", "rss.png" ; "alt", "Rss feed"], [])]) ;
        ])
      ) :: xml
;;

let generate_by_word_indexes stog env f_elt_id elt_type map =
  let f word set =
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
        elt_vars = [] ;
        elt_src = Printf.sprintf "%s.html" (Stog_types.string_of_human_id hid) ;
        elt_sets = [] ;
        elt_lang_dep = true ;
      }
    in
    let out_file = elt_dst_file stog elt in
    let rss_file = (Filename.chop_extension out_file)^".rss" in
    let url = elt_url stog elt in
    let rss_url = (Filename.chop_extension url)^".rss" in
    generate_rss_feed_file stog ~title: word url
      (List.map (Stog_types.elt stog) (Stog_types.Elt_set.elements set))
      rss_file;
    let elt =
      { elt with Stog_types.elt_body = elt_list ~set ~rss: rss_url stog env [] []}
    in
    let env = Xtmpl.env_of_list ~env
      ((commands_of_elt stog elt) @ (default_commands ~rss: rss_url stog))
    in
    let env = env_add_langswitch env stog elt in
    generate_elt stog env elt
  in
  Stog_types.Str_map.iter f map
;;

let generate_topic_indexes stog env =
  generate_by_word_indexes stog env topic_index_hid
  "by-topic" stog.stog_elts_by_topic

;;

let generate_keyword_indexes stog env =
  generate_by_word_indexes stog env keyword_index_hid
  "by-keyword" stog.stog_elts_by_kw
;;

let generate_archive_index stog env =
  let f_month year month set =
    let hid = month_index_hid ~year ~month in
    let title =
      let month_str = Stog_intl.get_month stog.stog_lang month in
      Printf.sprintf "%s %d" month_str year
    in
    let elt =
      { Stog_types.elt_human_id = hid ;
        elt_type = "month";
        elt_body = elt_list ~set stog env [] [] ;
        elt_date = None ;
        elt_title = title ;
        elt_keywords = [] ;
        elt_topics = [] ;
        elt_published = true ;
        elt_vars = [] ;
        elt_src = Printf.sprintf "%s.html" (Stog_types.string_of_human_id hid) ;
        elt_sets = [] ;
        elt_lang_dep = true ;
      }
    in
    let env = Xtmpl.env_of_list ~env
      ((commands_of_elt stog elt) @ (default_commands stog))
    in
    let env = env_add_langswitch env stog elt in
    generate_elt stog env elt
  in
  let f_year year mmap =
    Stog_types.Int_map.iter (f_month year) mmap
  in
  Stog_types.Int_map.iter f_year stog.stog_archives
;;

(*
let generate_index_file stog env =
  let basefile = html_file stog "index" in
  let html_file = Filename.concat outdir basefile in
  let tmpl = Filename.concat stog.stog_tmpl_dir "index.tmpl" in
  let rss_basefile = "index.rss" in
  let rss_file = Filename.concat outdir rss_basefile in
  generate_rss_feed_file stog basefile
    (List.map snd (Stog_types.article_list stog)) rss_file;
  let env = Xtmpl.env_of_list ~env
    ([
       "contents", (fun _ _ _ -> [Xtmpl.xml_of_string stog.stog_body]);
       "articles", (article_list outdir ~rss: rss_basefile stog);
     ] @ (default_commands ~outdir ~from:`Index ~rss: rss_basefile stog))
  in
  let env = env_add_langswitch env stog html_file in
  Xtmpl.apply_to_file ~head: "<!DOCTYPE HTML>" env tmpl html_file
;;
let generate_index outdir stog env =
  Stog_misc.mkdir outdir;
 (*
  copy_file ~quote_src: false (Filename.concat stog.stog_tmpl_dir "*.less") outdir;
  copy_file ~quote_src: false (Filename.concat stog.stog_tmpl_dir "*.js") outdir;
  copy_file ~ignerr: true ~quote_src: false (Filename.concat stog.stog_tmpl_dir "*.png") outdir;
  copy_file ~ignerr: true ~quote_src: false (Filename.concat stog.stog_tmpl_dir "*.jpg") outdir;
  copy_file ~ignerr: true ~quote_src: false (Filename.concat stog.stog_dir "*.png") outdir;
  copy_file ~ignerr: true ~quote_src: false (Filename.concat stog.stog_dir "*.jpg") outdir;
 *)
;;
*)

let generate stog =
  begin
    match stog.stog_lang with
      None -> ()
    | Some lang -> Stog_msg.verbose (Printf.sprintf "Generating pages for language %s" lang);
  end;
  current_stog := Some stog;
  let env = List.fold_left
    (fun env (name, v) -> Xtmpl.env_add name (fun _ _ _ -> [Xtmpl.D v]) env)
    Xtmpl.env_empty
    stog.stog_vars
  in
  Stog_tmap.iter
    (fun elt_id elt ->
     prerr_endline (Stog_types.string_of_human_id elt.elt_human_id))
  stog.stog_elts;
  prerr_endline
  (Stog_types.Hid_map.to_string (fun x -> x) stog.stog_elts_by_human_id);
(*  generate_index outdir stog env ; generate_index_file outdir stog env;*)
  generate_topic_indexes stog env;
  generate_keyword_indexes stog env;
  generate_archive_index stog env;
  Stog_tmap.iter (fun elt_id elt -> generate_elt stog env ~elt_id elt) stog.stog_elts
;;


