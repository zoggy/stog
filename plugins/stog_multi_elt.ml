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

let get_path_sep elt =
  match Stog_types.get_def elt.elt_defs ("",Stog_tags.path_sep) with
    None -> "/"
  | Some (_, [ Xtmpl.D s ]) -> s
  | Some (_, xmls) ->
      failwith ("Invalid "^(Stog_tags.path_sep^": "^(Xtmpl.string_of_xmls xmls)))
;;

let mk_elt path_sep elt_id (stog,elt) = function
  Xtmpl.D _ -> (stog,elt)
| Xtmpl.E (("","contents"), atts, subs) ->
    begin
      match Xtmpl.get_arg_cdata atts ("","type") with
        None ->
          let msg = "Missing type attribute in <contents> in "^
            (Stog_types.string_of_path elt.elt_path)
            in
            Stog_msg.error msg;
            (stog, elt)
      | Some typ ->
          let atts = Xtmpl.atts_remove ("","type") atts in
          (match elt.elt_body with
            [] -> ()
           | _ ->
               Stog_msg.warning
                 (Printf.sprintf "Element %s: more than one <contents> node"
                  (Stog_types.string_of_path elt.elt_path)
                 )
          );
          let elt = { elt with elt_body = subs ; elt_type = typ } in
          let elt = Stog_io.fill_elt_from_atts_and_subs elt atts subs in
          (stog, elt)
    end
| Xtmpl.E ((_,typ),atts,subs) ->
    let path =
      match Xtmpl.get_arg_cdata atts ("","path") with
        Some path -> Stog_types.path_of_string path
      | None ->
          match Xtmpl.get_arg_cdata atts ("","id") with
            None ->
              let msg = "No id and no path attributes for an element in "^
                (Stog_types.string_of_path elt.elt_path)
              in
              failwith msg
          | Some id ->
              Stog_cut.mk_path elt.elt_path path_sep id
    in
    let new_elt = { elt with elt_out = None ; elt_type = typ ; elt_path = path } in
    let new_elt = Stog_io.fill_elt_from_atts_and_subs new_elt atts subs in
    (Stog_types.add_elt stog new_elt, elt)
;;

let f_multi_elt stog elt_id =
  let elt = Stog_types.elt stog elt_id in
  let xmls =
    match elt.elt_out with
      None -> elt.elt_body
    | Some xmls -> xmls
  in
  let elt = { elt with elt_body = [] ; elt_out = None } in
  let path_sep = get_path_sep elt in
  let (stog, elt) = List.fold_left (mk_elt path_sep elt_id) (stog, elt) xmls in
  Stog_types.set_elt stog elt_id elt
  (* remove original elt ? *)
;;

let fun_level_init =
  let f_elt stog elt_id =
    let elt = Stog_types.elt stog elt_id in
    match elt.elt_type with
      "multi" -> f_multi_elt stog elt_id
    | _ -> stog
  in
  let f env stog elts = List.fold_left f_elt stog elts in
  Stog_engine.Fun_stog f
;;


let level_funs =
  [
    "init", fun_level_init ;
  ]
;;

let default_levels =
  List.fold_left
    (fun map (name, levels) -> Stog_types.Str_map.add name levels map)
    Stog_types.Str_map.empty
    [
      "init", [ -10 ] ;
    ]
let module_name = "multi-elt";;

let make_module ?levels () =
  let levels = Stog_html.mk_levels module_name level_funs default_levels ?levels () in
  let module M =
  struct
    type data = unit
    let modul = {
        Stog_engine.mod_name = module_name ;
        mod_levels = levels ;
        mod_data = () ;
       }

    type cache_data = unit
    let cache_load _stog data elt t = data
    let cache_store _stog data elt = ()
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