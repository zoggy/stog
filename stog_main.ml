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

let output_dir = ref "stog-output";;

let site_url = ref None ;;
let tmpl_dir = ref None ;;

let lang = ref None;;
let default_lang_to_set = ref None;;

let plugins = ref [];;
let packages = ref [];;
let only_elt = ref None;;

let set_stog_options stog =
  let stog =
    match !site_url with
      None -> stog
    | Some s -> { stog with Stog_types.stog_base_url = s }
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
  let stog = { stog with Stog_types.stog_outdir = !output_dir } in
  stog
;;

let options = [
    "-v", Arg.Unit Stog_msg.incr_verbose_level, " increase verbose level by one";
    "--verbose", Arg.Int Stog_msg.set_verbose_level, "<n> set verbose level to <n>";

    "-d", Arg.Set_string output_dir,
    "<dir> set output directory instead of "^ !output_dir ;

    "--site-url", Arg.String (fun s -> site_url := Some s),
    "<s> use <s> as site url instead of the one specified in the input stog" ;

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

    "--only", Arg.String (fun s -> only_elt := Some s),
    "<elt-id> generate only the page for the given element" ;
  ];;

let usage = Printf.sprintf
  "Usage: %s [options] directory\nwhere options are:"
  Sys.argv.(0)
;;

let main () =
  let remain = ref [] in
  Arg.parse (Arg.align options) (fun s -> remain := s :: !remain) usage ;

  Stog_dyn.load_files !plugins;
  !Stog_dyn.load_packages !packages;
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
        Stog_html.generate ?only_elt: !only_elt stog
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