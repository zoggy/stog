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

(** *)

open Stog_types

let init_common ?(set_fields=fun stog -> stog) stogs =
  let stog = Stog_types.merge_stogs stogs in
  let stog = set_fields stog in
  let stog = Stog_info.remove_not_published stog in
  let stog = Stog_info.compute stog in
  let modules = Stog_engine.modules () in
  let modules = List.map
    (fun (name, f) ->
       Stog_msg.verbose ~level: 2 ("Initializing module "^name);
       f stog
    )
      modules
  in
  (stog, modules)

let from_dirs ?set_fields dirs =
  let stogs = List.map Stog_io.read_stog dirs in
  let (stog, modules) = init_common ?set_fields stogs in
  let def_style =
    (("", Stog_tags.default_style), Xtmpl.atts_empty,
     [ Xtmpl.xml_of_string ~add_main: false
       "<link href=\"&lt;site-url/&gt;/style.css\" rel=\"stylesheet\" type=\"text/css\"/>"
     ])
  in
  let stog = { stog with stog_defs = stog.stog_defs @ [ def_style ] } in
  (stog, modules)

let from_files ?set_fields files =
  let dir = Sys.getcwd () in
  let load_doc file =
    let file =
      if Filename.is_relative file then
        Filename.concat dir file
      else
        file
    in
    let dir = Filename.dirname file in
    let stog = Stog_types.create_stog ~source: `File dir in
    let stog = { stog with stog_tmpl_dirs = [dir] } in
    let doc = Stog_io.doc_of_file stog file in
    Stog_types.add_doc stog doc
  in
  let stogs = List.map load_doc files in
  let remove_add_docs stog =
    (* remove add-docs levels from base module *)
    { stog with
      stog_levels = Stog_types.Str_map.add
        Stog_html.module_name ["add-docs", []] stog.stog_levels ;
    }
  in
  let set_fields =
    match set_fields with
      None -> remove_add_docs
    | Some f -> fun stog -> remove_add_docs (f stog)
  in
  let (stog, modules) = init_common ~set_fields stogs in
  let stog = Stog_io.read_modules stog in
  let def_style =
    (("", Stog_tags.default_style), Xtmpl.atts_empty,
     [ Xtmpl.xml_of_string ~add_main: false
       "<style><include file=\"&lt;doc-type/&gt;-style.css\" raw=\"true\"/></style>"
     ])
  in
  let stog = { stog with stog_defs = stog.stog_defs @ [ def_style ] } in
  (stog, modules)
