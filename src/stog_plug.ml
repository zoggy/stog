(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2015 INRIA All rights reserved.                         *)
(*    Author: Maxence Guesdon, INRIA Saclay                                      *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU General Public License for more details.                               *)
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

let plugin_config_file stog plugin_name =
  Filename.concat
    (Stog_config.config_dir stog.Stog_types.stog_dir)
    ("config-"^plugin_name)
;;

let register_lang = Stog_intl.register_lang;;

let register_html_base_rule = Stog_html.register_base_rule;;

let doc_by_href = Stog_html.doc_by_href

let mk_block_node ~id ?label ?clas ~title ?counter ~short_fmt ~long_fmt body =
  let b = Stog_blocks.mk_block
    ~id ?label ?clas ~title ?counter ~short_fmt ~long_fmt body
  in
  Stog_blocks.node_of_block b
;;

let set_print_verbose = Stog_msg.set_print_verbose;;
let verbose = Stog_msg.verbose;;

let set_print_warning = Stog_msg.set_print_warning;;
let warning = Stog_msg.warning;;

let set_print_error = Stog_msg.set_print_error;;
let error = Stog_msg.error;;

type dependency = Stog_types.doc Stog_types.dependency

let add_dep = Stog_deps.add_dep


