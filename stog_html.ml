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

let get_in_env = Stog_engine.get_in_env;;

let get_in_args_or_env data env args s =
  match Xtmpl.get_arg args s with
    None -> get_in_env data env s
  | Some s -> (data, s)
;;

let get_hid data env =
  let (data, xmls) = get_in_env data env ("", Stog_tags.elt_hid) in
  let s = Xtmpl.string_of_xmls xmls in
  assert (s <> "");
  (data, s)
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

let url_of_hid stog ?ext hid =
  let elt = Stog_types.make_elt ~hid () in
  let src =
    Printf.sprintf "%s%s" (Stog_types.string_of_human_id hid)
      (match ext with None -> "" | Some s -> "."^s)
  in
  Stog_engine.elt_url stog { elt with Stog_types.elt_src = src }
;;

let topic_index_hid topic =
  Stog_types.human_id_of_string ("/topic_" ^ topic);;
let keyword_index_hid kw =
  Stog_types.human_id_of_string ("/kw_"^ kw);;
let month_index_hid ~year ~month =
  Stog_types.human_id_of_string (Printf.sprintf "/%04d_%02d" year month);;

let plugin_base_rules = ref [];;

let register_base_rule name f =
   plugin_base_rules := (name, f) :: !plugin_base_rules ;;

let include_href name stog elt ?id ~raw ~depend href env =
  let new_id = id in
  let (hid, id) =
    try
      let p = String.index href '#' in
      let len = String.length href in
      let hid = String.sub href 0 p in
      (hid, String.sub href (p+1) (len - (p+1)))
    with
      Not_found ->
        failwith ("Missing #id part of href in <"^name^"> rule")
  in
  try
    let (stog, s_hid) =
      match hid with
        "" -> get_hid stog env
      | s ->  (stog, s)
    in
    let hid = Stog_types.human_id_of_string s_hid in
    let (_, elt) = Stog_types.elt_by_human_id stog hid in
    let stog =
      if depend then Stog_deps.add_dep stog elt (Stog_types.Elt elt) else stog
    in
    let (elt, id) = Stog_types.map_elt_ref stog elt id in
    match Stog_types.find_block_by_id elt id with
    | None ->
        failwith
          (Printf.sprintf "No id %S in element %S"
           id (Stog_types.string_of_human_id hid))
    | Some (Xtmpl.D _) -> assert false
    | Some ((Xtmpl.E (tag, atts, subs)) as xml)->
        let xmls =
          if raw then
            [ Xtmpl.D (Xtmpl.string_of_xml xml) ]
          else
            match new_id with
              None -> [xml]
            | Some new_id ->
                let atts = Xtmpl.atts_replace ("","id") new_id atts in
                [ Xtmpl.E (tag, atts, subs) ]
        in
        (stog, xmls)
  with
    Failure s ->
      Stog_msg.error s;
      (stog, [Xtmpl.D ("??"^href^"??")])
;;

let include_file stog elt ?id ~raw ~depend file args subs =
  let args = Xtmpl.atts_one ~atts: args ("", "contents") subs in
  let (stog, xml) = Stog_tmpl.read_template_file stog elt ~depend ~raw file in
  (stog, [Xtmpl.E (("", Xtmpl.tag_env), args, [xml])])
;;

let fun_include_ name elt stog env args subs =
  let raw = Xtmpl.opt_arg_cdata ~def: "false" args ("", "raw") = "true" in
  let id = Xtmpl.get_arg args ("", "id") in
  let depend = Xtmpl.opt_arg_cdata args ~def: "true" ("", "depend") <> "false" in
  match Xtmpl.get_arg_cdata args ("", "file") with
  | Some file ->
      let (stog, xml) = include_file stog elt ?id ~raw ~depend file args subs in
      (stog, xml)
  | None ->
      match Xtmpl.get_arg_cdata args ("", "href") with
        Some href -> include_href name stog elt ?id ~raw ~depend href env
      | None ->
          failwith ("Missing 'file' or 'href' argument for <"^name^"> rule")
;;

let fun_include = fun_include_ (Stog_tags.include_);;
let fun_late_inc = fun_include_ (Stog_tags.late_inc);;

let fun_inc elt stog env args subs =
  Stog_msg.warning ("<"^Stog_tags.inc^"> rule is deprecated; use <"^Stog_tags.late_inc^"> rule instead");
  fun_late_inc elt stog env args subs
;;

let fun_image acc _env args legend =
  let width = Xtmpl.opt_arg args ("", "width") in
  let src = Xtmpl.opt_arg args ("", "src") in
  let cls = Printf.sprintf "img%s"
    (match Xtmpl.get_arg_cdata args ("", "float") with
      Some s ->
         begin
           match s with
             "left" -> "-float-left"
           | "right" -> "-float-right"
           | s -> failwith (Printf.sprintf "unhandled image position: %s" s)
         end
     | None -> ""
    )
  in
  let cls = [ Xtmpl.D cls ] in
  let cls =
    match Xtmpl.get_arg args ("", "class") with
      None -> cls
    | Some c -> c @ [Xtmpl.D " "] @ cls
  in
  let atts = Xtmpl.atts_remove ("","width") args in
  let atts = Xtmpl.atts_remove ("","src") atts in
  let atts = Xtmpl.atts_remove ("","float") atts in
  let xmls =
    [
      Xtmpl.E (("", "div"), Xtmpl.atts_one ("", "class") cls,
       (Xtmpl.E (("", "img"),
         Xtmpl.atts_of_list ~atts
           [ ("", "class"), [Xtmpl.D "img"] ; ("", "src"), src; ("", "width"), width ],
         [])
       ) ::
         (match legend with
            [] -> []
          | xml -> [ Xtmpl.E (("", "div"), Xtmpl.atts_one ("", "class") [Xtmpl.D "legend"], xml) ]
         )
      )
    ]
  in
  (acc, xmls)
;;

let fun_list acc env args subs =
  let sep = Xtmpl.opt_arg args ("", "sep") in
  let sep = List.rev sep in
  let rec iter acc = function
    [] -> List.rev acc
  | h :: q ->
      let acc =
        match acc with
          [] -> [h]
        | _ -> h :: sep @ acc
      in
      iter acc q
  in
  (* and finally return the list of xml trees *)
  (acc, iter [] subs)
;;

let elt_by_href ?typ stog acc env href =
  let (hid, id) =
    try
      let p = String.index href '#' in
      let len = String.length href in
      let hid = String.sub href 0 p in
      let id = String.sub href (p+1) (len - (p+1)) in
      (hid, id)
    with
      Not_found -> (href, "")
  in
  let (acc, info) =
    try
      let (acc, hid) =
        match hid with
          "" -> get_hid acc env
        | s ->  (acc, s)
      in
      let hid = Stog_types.human_id_of_string hid in
      let (_, elt) = Stog_types.elt_by_human_id ?typ stog hid in
      let (elt, id) = Stog_types.map_elt_ref stog elt id in
      let hid = Stog_types.string_of_human_id elt.elt_human_id in
      (acc, Some (elt, hid, id))
    with
      Failure s ->
        Stog_msg.error ~info: "Stog_html.elt_by_href" s;
        (acc, None)
  in
  match info with
    None -> (acc, None)
  | Some (elt, hid, "") -> (acc, Some (elt, hid, None))
  | Some (elt, hid, id) -> (acc, Some (elt, hid, Some id))
;;

(* FIXME: add adependency ? *)
let fun_archive_tree stog _env _atts _subs =
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
    Xtmpl.E (("", "li"), Xtmpl.atts_empty, [
       Xtmpl.E (("", "a"),
        Xtmpl.atts_one ("", "href") [Xtmpl.D (Stog_types.string_of_url href)],
        [ Xtmpl.D month_str ]
       ) ;
       Xtmpl.D (Printf.sprintf "(%d)" (Stog_types.Elt_set.cardinal set))
     ]
    )
  in
  let f_year (year, data) =
    Xtmpl.E (("", "li"), Xtmpl.atts_empty, [
       Xtmpl.D (string_of_int year) ;
       Xtmpl.E (("", "ul"), Xtmpl.atts_empty, List.map (f_mon year) data) ;
      ]
    )
  in
  (stog, [ Xtmpl.E (("", "ul"), Xtmpl.atts_empty, List.map f_year years) ])
;;

let highlight = Stog_misc.highlight;;

let fun_hcode ?(inline=false) ?lang stog _env args code =
  let language, language_options =
    match lang with
      None ->
        (
         match Xtmpl.get_arg_cdata args ("", "lang-file") with
           None ->
             begin
               let lang = Xtmpl.opt_arg_cdata args ~def: "txt" ("", "lang") in
               match lang with
                 "txt" -> (lang, None)
               | _ -> (lang, Some (Printf.sprintf "--syntax=%s" lang))
             end
         | Some f ->
             let lang = Xtmpl.opt_arg_cdata args ~def: "" ("", "lang") in
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
  let atts =
    match Xtmpl.get_arg_cdata args ("","id") with
      None -> Xtmpl.atts_empty
    | Some id -> Xtmpl.atts_one ("","id") [Xtmpl.D id]
  in
  let xmls =
    if inline then
      [ Xtmpl.E (("", "span"), Xtmpl.atts_one ~atts ("", "class") [Xtmpl.D "icode"], [xml_code]) ]
    else
      [ Xtmpl.E (("", "pre"),
         Xtmpl.atts_one ~atts ("", "class") [Xtmpl.D (Printf.sprintf "code-%s" language)],
         [xml_code])
      ]
  in
  (stog, xmls)
;;

let fun_ocaml = fun_hcode ~lang: "ocaml";;
let fun_command_line = fun_hcode ~lang: "sh";;
let fun_icode = fun_hcode ~inline: true ;;

let concat_name ?(sep=":") (prefix, name) =
  match prefix with
    "" -> name
  | p -> p ^ sep ^ name
;;

let fun_search_form stog _env _ _ =
  let tmpl = Filename.concat stog.stog_tmpl_dir "search.tmpl" in
  (stog, [ Xtmpl.xml_of_file tmpl ])
;;


(* FIXME: add dependency ? *)
let fun_graph =
  let generated = ref false in
  let report_error msg = Stog_msg.error ~info: "Stog_html.fun_graph" msg in
  fun stog _env _ _ ->
    let png_name = "site-graph.png" in
    let small_png_name = "small-"^png_name in
    let svg_file = (Filename.chop_extension png_name) ^ ".svg" in
    let src = Stog_types.url_concat stog.stog_base_url svg_file in
    let small_src = Stog_types.url_concat stog.stog_base_url small_png_name in
    begin
      match !generated with
        true -> ()
      | false ->
          generated := true;
          let dot_code = Stog_info.dot_of_graph (Stog_engine.elt_url stog) stog in

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
    let xmls = [
        Xtmpl.E (("", "a"),
         Xtmpl.atts_one ("", "href") [ Xtmpl.D (Stog_types.string_of_url src)],
         [
           Xtmpl.E (("", "img"),
            Xtmpl.atts_of_list
              [("", "src"), [ Xtmpl.D (Stog_types.string_of_url small_src)] ; ("", "alt"), [Xtmpl.D "Graph"]],
            [])
         ])
      ]
    in
    (stog, xmls)
;;

let fun_if stog env args subs =
  let pred (prefix, name) v (stog, cond) =
    let nodes = [ Xtmpl.E ((prefix, name), Xtmpl.atts_empty, []) ] in
    let (stog, nodes2) = Xtmpl.apply_to_xmls stog env nodes in
    let v2 = if nodes = nodes2 then [] else nodes2 in
    (*
       prerr_endline (Printf.sprintf "fun_if: pred: att=(%s,%s), nodes=%S nodes2=%S, v=%S, v2=%S"
       prefix name (Xtmpl.string_of_xmls nodes)
       (Xtmpl.string_of_xmls nodes2) v (Xtmpl.string_of_xmls v2));
    *)
    (*let v_xml = Xtmpl.xml_of_string v in
    let v2_xml = Xtmpl.E (("", Xtmpl.tag_main), [], v2) in*)
    (*
       prerr_endline (Printf.sprintf "v_xml=%S, v2_xml=%S"
       (Xtmpl.string_of_xml v_xml) (Xtmpl.string_of_xml v2_xml));
    *)
    (stog, cond && (v = v2))
  in
  let (stog, cond) = Xtmpl.Name_map.fold pred args (stog, true) in
  let subs = List.filter
    (function Xtmpl.D _ -> false | _ -> true)
    subs
  in
  let xmls =
    match cond, subs with
    | true, [] -> failwith "<if>: missing children"
    | true, h :: _
    | false, _ :: h :: _ -> [h]
    | false, []
    | false, [_] -> []
  in
  (stog, xmls)
;;

let fun_dummy_ data _ _ subs = (data, subs);;

let fun_twocolumns stog env args subs =
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
  let xmls =
    [ Xtmpl.E (("", "table"), Xtmpl.atts_one ("", "class") [Xtmpl.D "two-columns"],
       [ Xtmpl.E (("", "tr"), Xtmpl.atts_empty,
          [ Xtmpl.E (("", "td"), Xtmpl.atts_one ("", "class") [Xtmpl.D "two-columns-left"], left) ;
            Xtmpl.E (("", "td"), Xtmpl.atts_one ("", "class") [Xtmpl.D "two-columns-right"], right) ;
          ]);
       ])
    ]
  in
  (stog, xmls)
;;

let fun_ncolumns stog env args subs =
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
          Xtmpl.atts_one ("", "class") [Xtmpl.D (Printf.sprintf "n-columns column-%d" n)],
          xmls)
        ) :: acc
      in
      (n+1, acc)
    in
    List.rev (snd (List.fold_left f (0,[]) subs))
  in
  let xmls =
    [ Xtmpl.E (("", "table"), Xtmpl.atts_one ("", "class") [Xtmpl.D "n-columns"],
       [ Xtmpl.E (("", "tr"), Xtmpl.atts_empty, tds) ]
      );
    ]
  in
  (stog, xmls)
;;

let fun_exta stog env args subs =
  (stog,
   [ Xtmpl.E (("", "span"), Xtmpl.atts_one ("", "class") [Xtmpl.D "ext-a"],
      [ Xtmpl.E (("", "a"), args, subs) ])
   ]
  )
;;

type toc = Toc of string * string * Xmlm.name * toc list (* name, title, class, subs *)

let fun_prepare_toc tags stog env args subs =
  let depth =
    match Xtmpl.get_arg_cdata args ("", "depth") with
      None -> max_int
    | Some s -> int_of_string s
  in
  let rec iter d acc = function
  | Xtmpl.D _ -> acc
  | Xtmpl.E (tag, atts, subs) when List.mem tag tags ->
      begin
        match Xtmpl.get_arg_cdata atts ("", "id"),
          Xtmpl.get_arg_cdata atts ("", "title")
        with
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
      Xtmpl.E (("", "li"), Xtmpl.atts_one ("", "class") [Xtmpl.D ("toc-"^cl)],
       [ Xtmpl.E (("", "elt"),
          Xtmpl.atts_of_list
            [("", "href"), [Xtmpl.D ("#"^name)] ; ("","long"), [Xtmpl.D "true"]],
          []) ]
       @
       ( match subs with
          [] -> []
        | _ ->
              [ Xtmpl.E (("", "ul"),
                 Xtmpl.atts_one ("", "class") [Xtmpl.D "toc"],
                 List.map xml_of_toc subs)
              ]
       )
       )
  in
  let xml =
    Xtmpl.E (("", "ul"),
     Xtmpl.atts_one ("", "class") [Xtmpl.D "toc"],
     List.map xml_of_toc toc)
  in
  let atts = Xtmpl.atts_one ("", "toc-contents") [ xml ] in
  (stog, [ Xtmpl.E (("", Xtmpl.tag_env), atts, subs) ])
;;

let fun_toc stog env args subs =
  (stog, subs @ [Xtmpl.E (("", "toc-contents"), Xtmpl.atts_empty, [])])
;;

let concat_xmls ?(sep=[]) l =
  let rec f xml = function
    [] -> xml
  | l -> xml @ sep @ l
  in
  List.fold_right f l []
;;


let fun_elt_path _elt stog env args subs =
  let (stog, s_hid) = get_hid stog env in
  let hid = Stog_types.human_id_of_string s_hid in
  if not hid.hid_absolute then
    failwith (Printf.sprintf "fun_elt_path: Hid %S not absolute" s_hid);

  let root =
    match Xtmpl.get_arg_cdata args ("", "with-root") with
      None -> None
    | Some root_hid ->
        let root_hid = Stog_types.human_id_of_string root_hid in
        ignore(Stog_types.elt_by_human_id stog root_hid);
        Some root_hid
  in
  let rec f acc path =
    (*prerr_endline (Printf.sprintf "path = [%s]" (String.concat "/" path));*)
    match List.rev path with
      [] ->
        begin
          match root with
            None -> acc
          | Some hid -> hid :: acc
        end
    | _ :: q ->
        let hid = { Stog_types.hid_path = path ; hid_absolute = true } in
        f (hid :: acc) (List.rev q)
  in
  let hids =
    (* remove last component of path to keep only "parent path" *)
    match List.rev hid.Stog_types.hid_path with
      [] | [_] -> (match root with None -> [] | Some hid -> [hid])
    | _ :: q -> f [] (List.rev q)
  in
  let map hid =
    try
      let hid =
        (* try to link to /the/path/index *)
        try
          let hid = { hid with hid_path = hid.hid_path @ ["index"] } in
          ignore(Stog_types.elt_by_human_id stog hid);
          hid
        with
          Failure _ ->
            (* if no such element exist, try /the/path *)
            ignore(Stog_types.elt_by_human_id stog hid);
            hid
      in
      let xml = Xtmpl.E
        (("", Stog_tags.elt),
         Xtmpl.atts_one ("","href") [Xtmpl.D (Stog_types.string_of_human_id hid)],
         [])
      in
      [ xml ]
    with Failure _ ->
        match List.rev hid.hid_path with
          [] -> [Xtmpl.D "?"]
        | h :: _ -> [ Xtmpl.D h ]
  in
  let xmls = concat_xmls ~sep: subs (List.map map hids) in
  (stog, xmls)
;;



let intro_of_elt stog elt =
  let rec iter acc = function
    [] -> raise Not_found
  | (Xtmpl.E (("",tag), _, _)) :: _ when tag = Stog_tags.sep -> List.rev acc
  | h :: q -> iter (h::acc) q
  in
  try
    let xml = iter [] elt.elt_body in
    let next_url_s =
      Stog_types.string_of_url (Stog_types.url_concat stog.stog_base_url "next.png")
    in
    xml @
    [
      Xtmpl.E (("", "a"),
         Xtmpl.atts_one ("", "href") [ Xtmpl.D (Stog_types.string_of_url (Stog_engine.elt_url stog elt))],
         [ Xtmpl.E (("", "img"),
            Xtmpl.atts_of_list
              [ ("", "src"), [ Xtmpl.D next_url_s ] ;
                ("", "alt"), [ Xtmpl.D "next"]
              ],
            [])]
        )
    ]
  with
    Not_found -> elt.elt_body
;;

let html_of_topics elt stog env args _ =
  let sep = Xtmpl.opt_arg args ~def: [Xtmpl.D ", "] ("", "sep") in
  let (stog, tmpl) = Stog_tmpl.get_template stog ~elt Stog_tmpl.topic "topic.tmpl" in
  let f stog w =
    let env = Xtmpl.env_of_list ~env
      [ ("", Stog_tags.topic), (fun acc _ _ _ -> (acc, [Xtmpl.D w])) ]
    in
    Xtmpl.apply_to_xmls stog env [tmpl]
  in
  let (stog, l) =
    List.fold_left
      (fun (stog, acc) w ->
         let (stog, xmls) = f stog w in
         let href = url_of_hid stog ~ext: "html" (topic_index_hid w) in
         let xml = Xtmpl.E (("", "a"),
            Xtmpl.atts_one ("", "href") [ Xtmpl.D (Stog_types.string_of_url href) ],
            xmls)
         in
         (stog, [xml] :: acc)
      )
      (stog, []) elt.elt_topics
  in
  let xmls = Stog_misc.list_concat ~sep (List.rev l) in
  (stog, List.flatten xmls)
;;

let html_of_keywords elt stog env args _ =
  let sep = Xtmpl.opt_arg args ~def: [Xtmpl.D ", "] ("", "sep") in
  let (stog, tmpl) = Stog_tmpl.get_template stog ~elt Stog_tmpl.keyword "keyword.tmpl" in
  let f stog w =
    let env = Xtmpl.env_of_list ~env
      [ ("", Stog_tags.keyword), (fun acc _ _ _ -> (acc, [Xtmpl.D w])) ]
    in
    Xtmpl.apply_to_xmls stog env [tmpl]
  in
  let (stog, l) =
    List.fold_left
      (fun (stog, acc) w ->
         let (stog, xmls) = f stog w in
         let href = url_of_hid stog ~ext: "html" (keyword_index_hid w) in
         let xml = Xtmpl.E (("", "a"),
            Xtmpl.atts_one ("", "href") [Xtmpl.D (Stog_types.string_of_url href)],
            xmls)
         in
         (stog, [xml] :: acc)
      )
      (stog, []) elt.elt_keywords
  in
  let xmls = Stog_misc.list_concat ~sep (List.rev l) in
  (stog, List.flatten xmls)
;;

(* FIXME: handle RSS files as any other element ? *)
let rec elt_to_rss_item stog elt_id elt =
  let link = Stog_engine.elt_url stog elt in
  let pubdate =
    match elt.elt_date with
      None -> assert false
    | Some d -> d
  in
  let f_word w =
    { Rss.cat_name = w ; Rss.cat_domain = None }
  in
  let cats =
    (List.map f_word elt.elt_topics) @
    (List.map f_word elt.elt_keywords)
  in
  let (stog, env) = Stog_engine.elt_env stog (Xtmpl.env_empty()) stog elt in
  let (stog, author) =
    let (stog, author) = get_in_env stog env ("", "author") in
    let author = Xtmpl.string_of_xmls author in
    (stog, Stog_misc.opt_of_string author)
  in
  let (stog, desc_xml) = Xtmpl.apply_to_xmls stog env (intro_of_elt stog elt) in
  let desc = Xtmpl.string_of_xmls desc_xml in
  let item =
    Rss.item ~title: elt.elt_title
      ~desc
      ~link
      ~pubdate
      ~cats
      ~guid: (Rss.Guid_permalink link)
      ?author
      ()
  in
  (stog, item)

and generate_rss_feed_file stog ?title link elts file =
  let elts = List.rev (Stog_types.sort_ids_elts_by_date elts) in
  let elts = List.filter
    (fun (_,elt) -> match elt.elt_date with None -> false | _ -> true)  elts
  in
  let (stog, items) = List.fold_left
    (fun (stog, acc) (id, elt) ->
       let (stog, item) = elt_to_rss_item stog id elt in
       (stog, item :: acc)
    )
      (stog, [])
      elts
  in
  let title = Printf.sprintf "%s%s"
    stog.stog_title
    (match title with None -> "" | Some t -> Printf.sprintf ": %s" t)
  in
  let pubdate =
    match elts with
      [] -> None
    | (_,h) :: _ -> h.elt_date
  in
  let image =
    try
      let file =
        match Stog_types.get_def stog.stog_defs ("", "rss-image") with
          Some (_,xmls) -> Xtmpl.string_of_xmls xmls
        | None -> ""
      in
      let url = Stog_types.url_concat stog.stog_base_url file in
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
  Rss.print_file ~encoding: "UTF-8" file channel;
  stog

and build_base_rules stog elt_id =
  let elt = Stog_types.elt stog elt_id in
  let f_title elt acc _ _ _ =
    (acc, [ Xtmpl.xml_of_string elt.elt_title ])
  in
  let f_url elt stog _ _ _ =
    (stog,[ Xtmpl.D (Stog_types.string_of_url (Stog_engine.elt_url stog elt)) ])
  in
  let f_body elt acc _ _ _ = (acc, elt.elt_body) in
  let f_type elt acc _ _ _ = (acc, [Xtmpl.D elt.elt_type]) in
  let f_src elt acc _ _ _ = (acc, [Xtmpl.D elt.elt_src]) in
  let f_date elt stog _ _ _ =
    (stog,[ Xtmpl.D (Stog_intl.string_of_date_opt stog.stog_lang elt.elt_date) ])
  in
  let f_datetime elt stog _ _ _ =
    (stog,[ Xtmpl.D (Stog_intl.string_of_datetime_opt stog.stog_lang elt.elt_date) ])
  in
  let f_intro elt stog _ _ _ =
    (stog, intro_of_elt stog elt)
  in
  let mk f stog env atts subs =
    let env =
      match Xtmpl.get_arg atts ("", Stog_tags.elt_hid) with
        None -> env
      | Some hid -> Xtmpl.env_add_att Stog_tags.elt_hid hid env
    in
    let nodes = [ Xtmpl.E (("", Stog_tags.elt_hid), Xtmpl.atts_empty, []) ] in
    let (stog, nodes2) = Xtmpl.apply_to_xmls stog env nodes in
    if nodes2 = nodes then
      (stog, [])
    else
      (
       match nodes2 with
         [Xtmpl.D s] ->
           let (_, elt) = Stog_types.elt_by_human_id stog (Stog_types.human_id_of_string s) in
           f elt stog env atts subs
       | _ -> (stog, [])
      )
  in
  let (previous, next) =
    let html_link stog elt =
      let href = Stog_engine.elt_url stog elt in
      [ Xtmpl.E (("", "a"),
         Xtmpl.atts_one ("","href") [Xtmpl.D (Stog_types.string_of_url href)],
         [ Xtmpl.xml_of_string elt.elt_title ]) ]
    in
    let try_link key search stog _ _ _ =
      let fallback () =
        match search stog elt_id with
        | None -> []
        | Some id -> html_link stog (Stog_types.elt stog id)
      in
      let xmls =
        match Stog_types.get_def elt.elt_defs key with
          None -> fallback ()
        | Some (_,body) ->
            let hid = Stog_types.human_id_of_string (Xtmpl.string_of_xmls body) in
            try
              let (_, elt) = Stog_types.elt_by_human_id stog hid in
              html_link stog elt
            with Failure s ->
                Stog_msg.warning s;
                fallback ()
      in
      (stog, xmls)
    in
    (try_link ("","previous") Stog_info.pred_by_date,
     try_link ("","next") Stog_info.succ_by_date)
  in
  let l =
    !plugin_base_rules @
    [
      ("", Stog_tags.archive_tree), fun_archive_tree ;
      ("", Stog_tags.command_line), fun_command_line ~inline: false ;
      ("", Stog_tags.elements), (fun acc -> elt_list elt acc) ;
      ("", Stog_tags.elt_body), mk f_body ;
      ("", Stog_tags.elt_date), mk f_date ;
      ("", Stog_tags.elt_datetime), mk f_datetime ;
      ("", Stog_tags.elt_intro), mk f_intro ;
      ("", Stog_tags.elt_keywords), mk html_of_keywords ;
      ("", Stog_tags.elt_path), mk fun_elt_path ;
      ("", Stog_tags.elt_src), mk f_src ;
      ("", Stog_tags.elt_title), mk f_title ;
      ("", Stog_tags.elt_topics), mk html_of_topics ;
      ("", Stog_tags.elt_type), mk f_type ;
      ("", Stog_tags.elt_url), mk f_url ;
      ("", Stog_tags.ext_a), fun_exta ;
      ("", Stog_tags.graph), fun_graph ;
      ("", Stog_tags.hcode), fun_hcode ~inline: false ?lang: None;
      ("", Stog_tags.icode), fun_icode ?lang: None ;
      ("", Stog_tags.if_), fun_if ;
      ("", Stog_tags.dummy_), fun_dummy_ ;
      ("", Stog_tags.image), fun_image ;
      ("", Stog_tags.include_), mk fun_include ;
      ("", Stog_tags.latex), Stog_latex.fun_latex ;
      ("", Stog_tags.list), fun_list ;
      ("", Stog_tags.n_columns), fun_ncolumns ;
      ("", Stog_tags.next), next;
      ("", Stog_tags.ocaml), fun_ocaml ~inline: false ;
      ("", Stog_tags.ocaml_eval), Stog_ocaml.fun_eval  ;
      ("", Stog_tags.previous), previous;
      ("", Stog_tags.search_form), fun_search_form ;
      ("", Stog_tags.two_columns), fun_twocolumns ;
    ]
  in
  l

and elt_list elt ?rss ?set stog env args _ =
  let report_error msg = Stog_msg.error ~info: "Stog_html.elt_list" msg in
  let elts =
    match set with
      Some set ->
        let l = Stog_types.Elt_set.elements set in
        List.map (fun id -> (id, Stog_types.elt stog id)) l
    | None ->
        let set = Xtmpl.get_arg_cdata args ("", "set") in
        Stog_types.elt_list ?set stog
  in
  let (stog, elts) =
    match Xtmpl.get_arg_cdata args ("", "filter") with
      None -> (stog, elts)
    | Some s ->
        let filter = Stog_filter.filter_of_string s in
        Stog_filter.filter_elts stog env filter elts
  in
  let elts =
    match Xtmpl.get_arg_cdata args ("", "type") with
      None -> elts
    | Some s ->
        let types = Stog_misc.split_string s [',' ; ';'] in
        List.filter (fun (_,elt) -> List.mem elt.elt_type types) elts
  in
  let max = Stog_misc.map_opt int_of_string
    (Xtmpl.get_arg_cdata args ("", "max"))
  in
  let reverse =
    match Xtmpl.get_arg_cdata args ("", "reverse") with
      None -> true
    | Some s -> Stog_io.bool_of_string s
  in
  let (stog, elts) =
    match Xtmpl.get_arg_cdata args ("", "sort") with
      None -> (stog, Stog_types.sort_ids_elts_by_date elts)
    | Some s ->
        let fields = Stog_misc.split_string s [','] in
        let elts = List.map
          (fun (id, e) -> (id, e, Stog_engine.env_of_defs ~env e.elt_defs))
          elts
        in
        Stog_types.sort_ids_elts_by_rules stog fields elts
  in
  let elts = if reverse then List.rev elts else elts in
  let elts =
    match max with
      None -> elts
    | Some n -> Stog_misc.list_chop n elts
  in
  let (stog, tmpl) =
    let file =
      match Xtmpl.get_arg_cdata args ("", "tmpl") with
        None ->  "elt-in-list.tmpl"
      | Some s -> s
    in
    Stog_tmpl.get_template stog ~elt Stog_tmpl.elt_in_list file
  in
  let f_elt (stog, acc) (elt_id, elt) =
    let name = Stog_types.string_of_human_id elt.elt_human_id in
    let (stog, env) = Stog_engine.elt_env stog env stog elt in
    let rules = build_base_rules stog elt_id in
    let env = Xtmpl.env_of_list ~env rules in
    let (stog, xmls) = Xtmpl.apply_to_xmls stog env [tmpl] in
    match xmls with
      [xml] -> (stog, xml :: acc)
    | _ ->
        report_error ("Error while processing " ^ name);
        assert false
  in
  let (stog, xmls) = List.fold_left f_elt (stog, []) elts in
  let xmls = List.rev xmls in
  (*prerr_endline "elt_list:";
  List.iter
    (fun xml -> prerr_endline (Printf.sprintf "ITEM: %s" (Xtmpl.string_of_xml xml)))
    xml;
     *)
  let (stog, rss) =
    match rss with
      Some link -> (stog, Some link)
    | None ->
        match Xtmpl.get_arg_cdata args ("", "rss") with
          None -> (stog, None)
        | Some file ->
            let url = Stog_types.url_concat stog.stog_base_url file in
            let file = Filename.concat stog.stog_outdir file in
            let title =Xtmpl.get_arg_cdata args ("", "title") in
            let stog = generate_rss_feed_file stog ?title url elts file in
            (stog, Some url)
  in
  let xml =
    match rss with
      None -> xmls
    | Some link ->
        (Xtmpl.E (("", "div"),
          Xtmpl.atts_one ("", "class") [Xtmpl.D "rss-button"],
          [
            Xtmpl.E (("", "a"), Xtmpl.atts_one ("", "href") [Xtmpl.D (Stog_types.string_of_url link)],
             [
               Xtmpl.E (("", "img"),
                Xtmpl.atts_of_list [("", "src"), [Xtmpl.D "rss.png"] ; ("", "alt"), [Xtmpl.D "Rss feed"]],
                [])
             ]) ;
          ])
        ) :: xmls
  in
  (stog, xml)
;;

module Sset = Stog_types.Str_set;;

let make_by_word_indexes stog env f_elt_id elt_type map =
  let f word set stog =
    let hid = f_elt_id word in
    let elt =
      { Stog_types.elt_human_id = hid ;
        elt_parent = None ;
        elt_children = [] ;
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
    let out_file = Stog_engine.elt_dst_file stog elt in
    let rss_file = (Filename.chop_extension out_file)^".rss" in
    let url = Stog_engine.elt_url stog elt in
    let rss_url =
      let url_s = Stog_types.string_of_url url in
      Stog_types.url_of_string ((Filename.chop_extension url_s)^".rss")
    in
    let stog = generate_rss_feed_file stog ~title: word url
      (List.map (fun id -> (id, Stog_types.elt stog id)) (Stog_types.Elt_set.elements set))
      rss_file
    in
    let (stog, body) = elt_list elt ~set ~rss: rss_url stog env Xtmpl.atts_empty [] in
    let elt = { elt with Stog_types.elt_body = body } in
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
        elt_parent = None ;
        elt_children = [] ;
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
    let (stog, body) = elt_list elt ~set stog env Xtmpl.atts_empty [] in
    let elt = { elt with elt_body = body } in
    Stog_types.add_elt stog elt
  in
  let f_year year mmap stog =
    Stog_types.Int_map.fold (f_month year) mmap stog
  in
  Stog_types.Int_map.fold f_year stog.stog_archives stog
;;

let add_elts env stog _ =
  let stog = make_archive_index stog env in
  let stog = make_keyword_indexes stog env in
  let stog = make_topic_indexes stog env in
  stog
;;

module Intmap = Stog_types.Int_map

let fun_level_base = Stog_engine.fun_apply_stog_elt_rules build_base_rules;;

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

let rules_toc stog elt_id =
  let elt = Stog_types.elt stog elt_id in
  let tags = get_sectionning_tags stog elt in
  [ ("", Stog_tags.prepare_toc), (fun_prepare_toc tags);
    ("", Stog_tags.toc), fun_toc ;
  ]
;;

let fun_level_toc = Stog_engine.fun_apply_stog_elt_rules rules_toc ;;

type cutpoint =
  {
    cut_tag : string * string ;
    cut_elt_type : string ;
    cut_hid_sep : string ;
    cut_insert_link : bool ;
  }
;;

let cutpoint_of_atts elt atts =
  let typ = Xtmpl.opt_arg_cdata atts ~def: elt.elt_type ("","type") in
  let tag =
    match Xtmpl.get_arg_cdata atts ("","tag") with
      None -> failwith "Missing 'tag' attribute for <cut-elt> node"
    | Some s ->
        match Stog_misc.split_string s [':'] with
          [] | [_] -> ("", s)
        | h :: q -> (h, String.concat ":" q)
  in
  let sep = Xtmpl.opt_arg_cdata atts ~def: "-" ("", Stog_tags.hid_sep) in
  let insert_link = not (Xtmpl.opt_arg_cdata atts ~def: "true" ("","insert-link") = "false") in
  { cut_tag = tag ; cut_elt_type = typ ;
    cut_hid_sep = sep ; cut_insert_link = insert_link ;
  }
;;

let cut_elts =
  let id_set =
    let rec iter set = function
      Xtmpl.D _ -> set
    | Xtmpl.E (tag, atts, subs) ->
        let set =
          match Xtmpl.get_arg_cdata atts ("", "id") with
            None
          | Some "" -> set
          | Some id -> Sset.add id set
        in
        List.fold_left iter set subs
    in
    fun elt ->
      let xmls =
        match elt.elt_out with
          None -> elt.elt_body
        | Some xmls -> xmls
      in
      List.fold_left iter Sset.empty xmls
  in
  let string_of_tag = function
    ("",t) -> "<"^t^">"
  | (n, t) -> "<"^n^":"^t^">"
  in
  let set_id_map stog hid atts new_hid with_id =
    if hid <> new_hid then
      match Xtmpl.get_arg_cdata atts ("","id") with
        None -> stog
      | Some id ->
          let new_id = if with_id then Some id else None in
          id_map_add stog hid id new_hid new_id
    else
      stog
  in
  let rec iter elt new_hid cutpoints stog new_elts xml =
    match xml with
      Xtmpl.D _ -> (stog, [xml], new_elts)
    | Xtmpl.E (("","cut-elt"), atts, xmls) ->
        let cutpoints = (cutpoint_of_atts elt atts) :: cutpoints in
        let (stog, xmls, new_elts) = List.fold_right
          (fold elt new_hid cutpoints) xmls (stog, [], new_elts)
        in
        (stog, xmls, new_elts)

    | Xtmpl.E (tag, atts, xmls) ->
        let cp_opt =
          try Some (List.find (fun cp -> cp.cut_tag = tag) cutpoints)
          with Not_found -> None
        in
        match cp_opt with
          None ->
            (* not a cut point *)
            let (stog, xmls, new_elts) = List.fold_right
              (fold elt new_hid cutpoints) xmls (stog, [], new_elts)
            in
            (stog, [Xtmpl.E (tag, atts, xmls)], new_elts)
        | Some cp ->
            try
              let title =
                match Xtmpl.get_arg_cdata atts ("","title") with
                  None ->
                    Stog_msg.warning ("Missing title on cutpoint; not cutting node "^(string_of_tag tag));
                    raise Not_found
                | Some s -> s
              in
              let id =
                match Xtmpl.get_arg_cdata atts ("","id") with
                  None ->
                    Stog_msg.warning ("Missing id on cutpoint; not cutting node "^(string_of_tag tag));
                    raise Not_found
                | Some s -> s
              in
              let new_hid_s = (Stog_types.string_of_human_id new_hid) ^ cp.cut_hid_sep ^ id in
              let new_hid = Stog_types.human_id_of_string new_hid_s in
              let stog = set_id_map stog elt.elt_human_id atts new_hid false in
              let (stog, xmls, new_elts) =
                List.fold_right (fold elt new_hid cutpoints)
                  xmls (stog, [], new_elts)
              in
              let elt =
                { elt with
                  elt_human_id = new_hid ;
                  elt_parent = Some elt.elt_human_id ;
                  elt_type = cp.cut_elt_type ;
                  elt_title = title ;
                  elt_body = xmls ;
                  elt_out = None ;
                }
              in
              let xml =
                if cp.cut_insert_link then
                  [ Xtmpl.E (("","div"),
                     Xtmpl.atts_one ("","class") [Xtmpl.D ("cutlink "^(snd tag))],
                     [Xtmpl.E (("","elt"), Xtmpl.atts_one ("","href") [Xtmpl.D new_hid_s],[])]
                    )
                  ]
                else
                  []
              in
              (stog, xml, elt :: new_elts)
            with
              Not_found ->
                (* not enough information to cut *)
                let (stog, xmls, new_elts) =
                  List.fold_right (fold elt new_hid cutpoints)
                    xmls (stog, [], new_elts)
                in
                (stog, xmls, new_elts)

  and fold elt new_hid cutpoints xml (stog, xmls, new_elts) =
    let (stog, xmls2, new_elts) = iter elt new_hid cutpoints stog new_elts xml in
    (stog, xmls2 @ xmls, new_elts)
  in
  let cut_elt stog elt =
    match elt.elt_out with
      None -> (stog, elt, [])
    | Some body ->
        let (stog, body, new_elts) =
          List.fold_right (fold elt elt.elt_human_id [])
            body (stog, [], [])
        in
        let children =
          match new_elts with
            [] -> elt.elt_children
          | _ -> elt.elt_children @ (List.map (fun elt -> elt.elt_human_id) new_elts)
        in
        (stog,
         { elt with elt_out = Some body ; elt_children = children },
         new_elts)
  in
  let add_id_mappings stog src_hid dst_hid set =
    Sset.fold
      (fun id stog -> id_map_add stog src_hid id dst_hid (Some id))
      set stog
  in
  let set_elt_id_mappings orig_hid all_ids stog elt =
    let ids = id_set elt in
    let stog = add_id_mappings stog orig_hid elt.elt_human_id ids in
    add_id_mappings stog elt.elt_human_id orig_hid (Sset.diff all_ids ids)
  in
  let f_elt env stog elt_id =
    let elt = Stog_types.elt stog elt_id in
    let (stog, elt2, new_elts) = cut_elt stog elt in
    match new_elts with
      [] ->
        (* no new elements means the original element was not modified either *)
        stog
    | _ ->
        let all_ids = id_set elt in
        let stog =
          List.fold_left (set_elt_id_mappings elt.elt_human_id all_ids)
          stog new_elts
        in
        let stog = Stog_types.set_elt stog elt_id elt2 in
        let stog = List.fold_left Stog_types.add_elt stog new_elts in
        stog
  in
  fun env stog elts ->
    List.fold_left (f_elt env) stog elts
;;

let rules_inc_elt stog elt_id =
  let elt = Stog_types.elt stog elt_id in
  [ ("", Stog_tags.inc), fun_inc elt ;
    ("", Stog_tags.late_inc), fun_late_inc elt ]
;;
let fun_level_inc_elt =
  Stog_engine.fun_apply_stog_elt_rules rules_inc_elt ;;

let fun_level_clean =
  let f env stog elts =
    Stog_ocaml.close_sessions ();
    let env = Xtmpl.env_of_list ~env
      [ ("", Stog_tags.sep), (fun d _ _ _ -> (d, [])) ]
    in
    List.fold_left
      (fun stog elt_id -> Stog_engine.apply_stog_env_elt stog env elt_id)
      stog elts
  in
  Stog_engine.Fun_stog f
;;


let level_funs = [
    "base", fun_level_base ;
    "toc", fun_level_toc ;
    "cut", Stog_engine.Fun_stog cut_elts ;
    "inc", fun_level_inc_elt ;
    "clean", fun_level_clean ;
    "add-elts", (Stog_engine.Fun_stog add_elts) ;
  ]

let default_levels =
  List.fold_left
    (fun map (name, levels) -> Stog_types.Str_map.add name levels map)
    Stog_types.Str_map.empty
    [ "base", [ 0 ; 61 ] ;
      "add-elts", [ 30 ] ;
      "toc", [ 50 ] ;
      "cut", [ 60 ] ;
      "inc", [ 160 ] ;
      "clean", [ 500 ] ;
    ]
;;

let mk_levels modname level_funs default_levels =
  fun ?(levels=[]) () ->
    let levels =
      List.fold_left
        (fun map (name, levels) ->
           Stog_types.Str_map.add name levels map
        )
        default_levels
        levels
    in
    let f name levels map =
      let level_fun =
        try List.assoc name level_funs
        with Not_found ->
            let msg = Printf.sprintf
              "Level function %S unknown in module %S" name modname
            in
            failwith msg
      in
      List.fold_left
        (fun map level -> Intmap.add level level_fun map)
        map levels
    in
    Stog_types.Str_map.fold f levels Intmap.empty
;;

let module_name = "html";;

let make_module ?levels () =
  let levels = mk_levels module_name level_funs default_levels ?levels () in
  let module M =
  struct
    type data = unit
    let modul = {
        Stog_engine.mod_name = module_name ;
        mod_levels = levels ;
        mod_data = () ;
       }

    type cache_data = unit

    let cache_load _ data elt t = ()
    let cache_store _ data elt = ()
  end
  in
  (module M : Stog_engine.Module)
;;

let f stog =
  let levels =
    try Some (Stog_types.Str_map.find module_name stog.Stog_types.stog_levels)
    with Not_found -> None
  in
  make_module ?levels ()
;;

let () = Stog_engine.register_module module_name f;;
