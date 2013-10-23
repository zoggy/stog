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

let plugin_config_file stog plugin_name =
  Filename.concat
    (Stog_config.config_dir stog.Stog_types.stog_dir)
    ("config-"^plugin_name)
;;

let register_lang = Stog_intl.register_lang;;

let register_rule name f =
  Stog_html.plugin_rules := (name, f) :: !Stog_html.plugin_rules ;;

let unregister_rule name =
  let rec iter acc = function
    [] -> (List.rev acc, None)
  | (s,f) :: q ->
      if s = name then
        ((List.rev acc) @ q, Some f)
      else
        iter ((s,f) :: acc) q
  in
  let (rules, found) = iter [] !Stog_html.plugin_rules in
  Stog_html.plugin_rules := rules;
  found
;;

let stog () =
  match !Stog_html.current_stog with
    None -> failwith "Current stog not initialized"
  | Some x -> x
;;

let elt_by_href = Stog_html.elt_by_href

let add_block = Stog_html.add_block;;

let set_print_verbose = Stog_msg.set_print_verbose;;
let verbose = Stog_msg.verbose;;

let set_print_warning = Stog_msg.set_print_warning;;
let warning = Stog_msg.warning;;

let set_print_error = Stog_msg.set_print_error;;
let error = Stog_msg.error;;

let register_stage0_fun f =
  Stog_html.stage0_funs := f :: !Stog_html.stage0_funs
;;

type rule_build =
  Stog_types.stog -> Stog_types.elt_id -> Stog_types.elt -> (Xmlm.name * Xtmpl.callback) list
type level_fun =
  Xtmpl.env -> Stog_types.stog -> Stog_types.elt_id -> Stog_types.elt -> Stog_types.elt
;;
type level_fun_on_elt_list =
  Xtmpl.env -> Stog_types.stog -> (Stog_types.elt_id * Stog_types.elt) list ->
  (Stog_types.elt_id * Stog_types.elt) list * Stog_types.elt list
;;


let register_level_fun = Stog_html.register_level_fun;;
let compute_elt = Stog_html.compute_elt;;

let register_level_fun_on_elt_list = Stog_html.register_level_fun_on_elt_list;;

let register_cache = Stog_cache.register_cache ;;

type dependency = Stog_deps.dependency =
  | File of string
  | Elt of string

let add_dep = Stog_deps.add_dep;;


