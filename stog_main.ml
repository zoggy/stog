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

let output_dir = ref "stog-output";;

let site_url = ref None ;;
let tmpl_dir = ref None ;;

let lang = ref None;;
let default_lang_to_set = ref None;;

let plugins = ref [];;

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
  stog
;;

let options = [
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
  ];;

let usage = Printf.sprintf
  "Usage: %s [options] directory\nwhere options are:"
  Sys.argv.(0)
;;

let main () =
  let remain = ref [] in
  Arg.parse options (fun s -> remain := s :: !remain) usage ;

  !Stog_dyn.load_files !plugins;
  begin
    match !default_lang_to_set with
      None -> ()
    | Some abbrev -> Stog_intl.set_default_lang abbrev
  end;
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
      Stog_html.generate !output_dir stog
;;
Stog_misc.safe_main main;;