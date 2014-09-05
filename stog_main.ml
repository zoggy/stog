(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Maxence Guesdon. All rights reserved.              *)
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

open Stog_types;;

let output_dir = ref "stog-output";;

let site_url = ref None ;;
let tmpl_dirs = ref [] ;;
let mod_dirs = ref [] ;;
let use_cache = ref true;;
let depcut = ref false;;
let local = ref false;;

let stog_defs = ref [] ;;

let lang = ref None;;
let default_lang_to_set = ref None;;

let plugins = ref [];;
let packages = ref [];;
let only_doc = ref None;;

let publish_only = ref None ;;

type mode = Generate | Server
let mode = ref Generate

let debug = ref false;;

let add_stog_def s =
  match Stog_misc.split_string s [':'] with
    [] -> ()
  | [name] -> stog_defs := !stog_defs @ [(("", name), Xtmpl.atts_empty, [])]
  | name :: q ->
      let contents = Xtmpl.xml_of_string (String.concat ":" q) in
      stog_defs := !stog_defs @ [(("", name), Xtmpl.atts_empty, [contents])]

let set_stog_options stog =
  let stog = { stog with Stog_types.stog_outdir = !output_dir } in
  let stog =
    match !site_url, !local with
      None, false -> stog
    | None, true ->
        let d =
          if Filename.is_relative stog.stog_outdir then
            Filename.concat (Sys.getcwd()) stog.stog_outdir
          else
            stog.stog_outdir
        in
        let url = "file://" ^ d in
        let url = Stog_types.url_of_string url in
        { stog with Stog_types.stog_base_url = url }
    | Some s, false -> { stog with Stog_types.stog_base_url = s }
    | Some _, true ->
        failwith "Please choose --local or --site-url but not both"
  in
  let stog = { stog with stog_tmpl_dirs = List.rev (stog.stog_tmpl_dirs @ !tmpl_dirs) } in
  let stog = { stog with stog_mod_dirs = List.rev (stog.stog_mod_dirs @ !mod_dirs) } in
  let stog =
    match !lang with
      None -> stog
    | Some s -> { stog with Stog_types.stog_lang = Some s }
  in
  let stog = { stog with Stog_types.stog_depcut = !depcut } in
  let stog = { stog with Stog_types.stog_defs = stog.stog_defs @ !stog_defs } in
  let stog =
    match !publish_only with
      None -> stog
    | _ ->
        { stog with
          stog_publish_only = !publish_only ;
        }
  in
  (* add default template directory if there is at least one
    other template directory, so that the default one is
    not polluted by template files automatically created when missing. *)
  let stog =
    match stog.stog_tmpl_dirs with
      [] -> stog
    | dirs -> { stog with stog_tmpl_dirs = dirs @ [Stog_install.templates_dir] }
  in
  stog
;;

let run_from_dirs dirs =
  try
    let stogs = List.map Stog_io.read_stog dirs in
    (*prerr_endline "directories read";*)
    let stog = Stog_types.merge_stogs stogs in
    (*prerr_endline "directories merged";*)
    let stog = set_stog_options stog in
    let stog = Stog_info.remove_not_published stog in
    (*prerr_endline "removed not published articles";*)
    let stog = Stog_info.compute stog in
    (*prerr_endline "graph computed";*)
    let modules = Stog_engine.modules () in
    let modules = List.map
      (fun (name, f) ->
         Stog_msg.verbose ~level: 2 ("Initializing module "^name);
         f stog
      )
        modules
    in
    let only_docs =
      match !only_doc with
        None -> None
      | Some s -> Some [s]
    in
    let def_style =
      (("", Stog_tags.default_style), Xtmpl.atts_empty,
       [ Xtmpl.xml_of_string ~add_main: false
         "<link href=\"&lt;site-url/&gt;/style.css\" rel=\"stylesheet\" type=\"text/css\"/>"
       ])
    in
    let stog = { stog with stog_defs = stog.stog_defs @ [ def_style ] } in
    if Stog_server_mode.server_mode() then
      Stog_server_mode.run_server stog
    else
      Stog_engine.generate ~use_cache: !use_cache ?only_docs stog modules

  with Stog_types.Path_trie.Already_present l ->
      let msg = "Path already present: "^(String.concat "/" l) in
      failwith msg
;;

let run_from_files files =
  try
    let dir = Sys.getcwd () in
    let load_doc file =
      let file =
        if Filename.is_relative file then
          Filename.concat dir file
        else
          file
      in
      let dir = Filename.dirname file in
      let stog = Stog_types.create_stog dir in
      let stog = { stog with stog_tmpl_dirs = [dir] } in
      let doc = Stog_io.doc_of_file stog file in
      (*let doc = { doc with doc_src = file } in*)
      Stog_types.add_doc stog doc
    in
    let stogs = List.map load_doc files in
    let stog = Stog_types.merge_stogs stogs in
    let stog = set_stog_options stog in
    let stog = Stog_info.remove_not_published stog in
    (* remove add-docs levels from base module *)
    let stog = { stog with
        stog_levels = Stog_types.Str_map.add
          Stog_html.module_name ["add-docs", []] stog.stog_levels ;
      }
    in
    let modules = Stog_engine.modules () in
    let modules = List.map
      (fun (name, f) ->
         Stog_msg.verbose ~level: 2 ("Initializing module "^name);
         f stog
      )
        modules
    in
    let stog =
      let def_style =
        (("", Stog_tags.default_style), Xtmpl.atts_empty,
         [ Xtmpl.xml_of_string ~add_main: false
           "<style><include file=\"&lt;doc-type/&gt;-style.css\" raw=\"true\"/></style>"
         ])
      in
      { stog with stog_defs = stog.stog_defs @ [ def_style ] }
    in
    let stog = Stog_io.read_modules stog in
    if Stog_server_mode.server_mode() then
      Stog_server_mode.run_server stog
    else
      Stog_engine.generate ~use_cache: false ~gen_cache: false stog modules

  with Stog_types.Path_trie.Already_present l ->
      let msg = "Path already present: "^(String.concat "/" l) in
      failwith msg
;;

let options = [
    "-version",
    Arg.Unit (fun () -> print_endline (Printf.sprintf "%s" Stog_install.version); exit 0),
    " print version and exit";

    "-D", Arg.Set debug, " debug mode" ;

    "-v", Arg.Unit Stog_msg.incr_verbose_level, " increase verbose level by one";
    "--verbose", Arg.Int Stog_msg.set_verbose_level, "<n> set verbose level to <n>";

    "-d", Arg.Set_string output_dir,
    "<dir> set output directory instead of "^ !output_dir ;

    "--site-url", Arg.String (fun s -> site_url := Some (Stog_types.url_of_string s)),
    "<s> use <s> as site url instead of the one specified in the input stog" ;

    "--local", Arg.Set local,
    " set site-url as file://<destination directory>" ;

    "--tmpl", Arg.String (fun s -> tmpl_dirs := s :: !tmpl_dirs ),
    "<dir> add <dir> as template directory";

    "--mods", Arg.String (fun s -> mod_dirs := s :: !mod_dirs ),
    "<dir> add <dir> as module directory";

    "--lang", Arg.String (fun s -> lang := Some s),
    "<s> generate pages for language <s>" ;

    "--default-lang", Arg.String (fun s -> default_lang_to_set := Some s),
    "<lang> use <lang> as default language (dates, ...); default is \"en\"" ;

    "--plugin", Arg.String (fun s -> plugins := !plugins @ [s]),
    "<file> load plugin (ocaml object file)" ;

    "--package", Arg.String (fun s -> packages := !packages @ [s]),
    "<pkg[,pkg2[,...]]> load package (a plugin loaded with ocamlfind)";

    "--only", Arg.String (fun s -> use_cache := false ; only_doc := Some s),
    "<doc-id> generate only the page for the given document; imply --nocache" ;

    "--nocache", Arg.Clear use_cache,
    " do not use cache to prevent computing unmodified documents" ;

    "--depcut", Arg.Set depcut,
    " use only 1 level of dependency when getting cached documents";

    "--stog-ocaml-session", Arg.Set_string Stog_ocaml.stog_ocaml_session,
    "<command> use <command> as stog-ocaml-session program";

    "--def", Arg.String add_stog_def,
    "name:contents add a global rule name with the given contents" ;

    "--publish-only",
    Arg.String (fun s -> publish_only := Some (Stog_filter.filter_of_string s)),
    "<filter> only keep documents verifying the given condition" ;

    "--hackcmxs", Arg.Set Stog_dyn.hack_cmxs,
    " when a package to load depends on .cmxa or .cmx file, try to build .cmxs.\n*** Server options ***\n\n";

    "--port", Arg.Set_int Stog_server_mode.port,
    "<p> set port to listen on (default is "^(string_of_int !Stog_server_mode.port)^")" ;

    "--host", Arg.Set_string Stog_server_mode.host,
    "<host> set hostname to listen on (default is "^ !Stog_server_mode.host ^")" ;
  ];;

let usage ?(with_options=true) ()=
  Printf.sprintf
    "Usage: %s [options] <directory>\n    or %s [options] <files>%s"
    Sys.argv.(0)  Sys.argv.(0)
    (if with_options then "\nwhere options are:" else "")
;;

let file_kind file =
  try (Unix.stat file).Unix.st_kind
  with Unix.Unix_error (e,s1,s2) ->
      failwith (Printf.sprintf "%s: %s %s" (Unix.error_message e) s1 s2)
;;

let main () =
  let remain = ref [] in
  Arg.parse (Arg.align options) (fun s -> remain := s :: !remain) (usage()) ;

  try
    Stog_dyn.load_packages !packages;
    Stog_dyn.check_files_have_extension !plugins;
    Stog_dyn.load_files !plugins;
    begin
      match !default_lang_to_set with
        None -> ()
      | Some abbrev -> Stog_intl.set_default_lang abbrev
    end;
    begin
      match List.rev !remain with
        [] -> failwith (usage ~with_options: false ())
      | h :: q ->
          let k = file_kind h in
          List.iter
            (fun f ->
              if file_kind f <> k then
                 failwith (usage ~with_options: false ()))
            q;
          match k with
            Unix.S_REG -> run_from_files (h::q)
          | Unix.S_DIR -> run_from_dirs (h::q)
          | _ -> failwith ("Invalid file type for "^h)
    end;
    let err = Stog_msg.errors () in
    let warn = Stog_msg.warnings () in
    begin
      match err, warn with
        0, 0 -> ()
      | _, _ ->
          let msg = Printf.sprintf "%d error%s, %d warning%s"
            err (if err > 1 then "s" else "")
              warn (if warn > 1 then "s" else "")
          in
          prerr_endline msg;
    end;
    exit err
  with
    e when !debug -> raise e
  | Stog_engine.Cant_open_cache_file cache_file ->
      let msg = "Could open cache file "^cache_file^"\nYou should run stog once with --nocache" in
      Stog_misc.safe_main (fun () -> failwith msg)
  | e -> Stog_misc.safe_main (fun () -> raise e)
;;

let () = main ()
