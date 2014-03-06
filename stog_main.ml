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
let tmpl_dir = ref None ;;
let use_cache = ref true;;
let depcut = ref false;;
let local = ref false;;

let stog_defs = ref [] ;;

let lang = ref None;;
let default_lang_to_set = ref None;;

let plugins = ref [];;
let packages = ref [];;
let only_elt = ref None;;

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
  let stog =
    match !tmpl_dir with
      None -> stog
    | Some s -> { stog with Stog_types.stog_tmpl_dir = s }
  in
  let stog =
    match !lang with
      None -> stog
    | Some s -> { stog with Stog_types.stog_lang = Some s }
  in
  let stog = { stog with Stog_types.stog_depcut = !depcut } in
  let stog = { stog with Stog_types.stog_defs = stog.stog_defs @ !stog_defs } in
  stog
;;

let options = [
    "-version",
    Arg.Unit (fun () -> print_endline (Printf.sprintf "%s" Stog_config.version); exit 0),
    " print version and exit";

    "-v", Arg.Unit Stog_msg.incr_verbose_level, " increase verbose level by one";
    "--verbose", Arg.Int Stog_msg.set_verbose_level, "<n> set verbose level to <n>";

    "-d", Arg.Set_string output_dir,
    "<dir> set output directory instead of "^ !output_dir ;

    "--site-url", Arg.String (fun s -> site_url := Some (Stog_types.url_of_string s)),
    "<s> use <s> as site url instead of the one specified in the input stog" ;

    "--local", Arg.Set local,
    " set site-url as file://<destination directory>" ;

    "--tmpl", Arg.String (fun s -> tmpl_dir := Some s),
    "<dir> use <dir> as template directory instead tmpl of stog dir";

    "--lang", Arg.String (fun s -> lang := Some s),
    "<s> generate pages for language <s>" ;

    "--default-lang", Arg.String (fun s -> default_lang_to_set := Some s),
    "<lang> use <lang> as default language (dates, ...); default is \"en\"" ;

    "--plugin", Arg.String (fun s -> plugins := !plugins @ [s]),
    "<file> load plugin (ocaml object file)" ;

    "--package", Arg.String (fun s -> packages := !packages @ [s]),
    "<pkg[,pkg2[,...]]> load package (a plugin loaded with ocamlfind)";

    "--only", Arg.String (fun s -> use_cache := false ; only_elt := Some s),
    "<elt-id> generate only the page for the given element; imply --nocache" ;

    "--nocache", Arg.Clear use_cache,
    " do not use cache to prevent computing unmodified elements" ;

    "--depcut", Arg.Set depcut,
    " use only 1 level of dependency when getting cached elements";

    "--stog-ocaml-session", Arg.Set_string Stog_ocaml.stog_ocaml_session,
    "<command> use <command> as stog-ocaml-session program";

    "--def", Arg.String add_stog_def,
    "name:contents add a global rule name with the given contents" ;

    "--hackcmxs", Arg.Set Stog_dyn.hack_cmxs,
    " when a package to load depend on .cmxa or .cmx file, try to build .cmxs.";
  ];;

let usage = Printf.sprintf
  "Usage: %s [options] directory\nwhere options are:"
  Sys.argv.(0)
;;

let main () =
  let remain = ref [] in
  Arg.parse (Arg.align options) (fun s -> remain := s :: !remain) usage ;

  Stog_dyn.load_packages !packages;
  Stog_dyn.load_files !plugins;
  begin
    match !default_lang_to_set with
      None -> ()
    | Some abbrev -> Stog_intl.set_default_lang abbrev
  end;
  begin
    match List.rev !remain with
      [] -> failwith usage
    | dirs ->
        let stogs = List.map Stog_io.read_stog dirs in
        (*prerr_endline "directories read";*)
        let stog = Stog_types.merge_stogs stogs in
        (*prerr_endline "directories merged";*)
        let stog = Stog_info.remove_not_published stog in
        (*prerr_endline "removed not published articles";*)
        let stog = Stog_info.compute stog in
        (*prerr_endline "graph computed";*)
        let stog = set_stog_options stog in
        let modules = Stog_engine.modules () in
        let modules = List.map
          (fun (name, f) ->
             Stog_msg.verbose ~level: 2 ("Initializing module "^name);
             f stog
          )
            modules
        in
        let only_elts =
          match !only_elt with
            None -> None
          | Some s -> Some [s]
        in
        try Stog_engine.generate ~use_cache: !use_cache ?only_elts stog modules
        with Stog_types.Hid_trie.Already_present l ->
          let msg = "Path already present: "^(String.concat "/" l) in
          failwith msg
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
;;
Stog_misc.safe_main main;;