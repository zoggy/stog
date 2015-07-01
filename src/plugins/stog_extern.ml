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

open Stog_types;;

let module_name = "extern";;
let rc_file stog = Stog_plug.plugin_config_file stog module_name;;

module W = Ocf.Wrapper
type action =
  {
    types : string list
        [@ocf W.list W.string, []]
        [@ocf.doc "list of document types concerned"] ;
    name : string
        [@ocf W.string, "cat" ]
        [@ocf.doc "name of run level in which to apply the command"] ;
    command : string
        [@ocf W.string, "cat"]
        [@ocf.doc "command, taking XML document in input and outputting the new XML document"] ;
  } [@@ocf]

type data =
    { actions : action list
      [@ocf W.list action_wrapper, []] ;
    } [@@ocf]

let group data =
  let option_t = Ocf.option data_wrapper data in
  let g = Ocf.as_group option_t in
  (g, option_t)

let load_config stog =
  let (group, t) = group default_data in
  let rc_file = rc_file stog in
  if not (Sys.file_exists rc_file) then Ocf.to_file group rc_file ;
  try
    Ocf.from_file group rc_file;
    Ocf.get t
  with
  | Ocf.Error e -> failwith (Ocf.string_of_error e)
;;

let apply_to_doc types command stog doc_id =
  let doc = Stog_types.doc stog doc_id in
  match List.mem doc.doc_type types with
    false -> None
  | true ->
      let in_file = Filename.temp_file "stog" ".xml.in" in
      let out_file = (Filename.chop_extension in_file) ^ ".out" in
      try
        let xml =
          match doc.doc_out with
          | None -> doc.doc_body
          | Some xml -> xml
        in
        Stog_misc.file_of_string ~file: in_file (Xtmpl.string_of_xmls xml) ;
        let com = Printf.sprintf "cat %s | %s > %s"
          (Filename.quote in_file)
            command
            (Filename.quote out_file)
        in
        let rm () =
          try Sys.remove in_file with _ -> ();
              try Sys.remove out_file with _ -> ()
        in
        match Sys.command com with
          0 ->
            let xml = Xtmpl.xml_of_file out_file in
            rm ();
            let doc = { doc with doc_out = Some [xml] } in
            Some (doc_id, doc)
        | n ->
        failwith (Printf.sprintf "Command exited with %d: %s" n com)
      with
        e ->
          let msg =
            match e with
            | Failure msg | Sys_error msg -> msg
            | _ -> Printexc.to_string e
          in
          Stog_msg.error
            (Printf.sprintf "Doc %S: %s"
             (Stog_path.to_string doc.doc_path) msg);
          None

(** FIXME: parallelize this when we'll use lwt everywhere *)
let apply types command env stog docs =
  let docs = List.map
    (apply_to_doc types command stog)
      (Stog_types.Doc_set.elements docs)
  in
  List.fold_left
    (fun stog -> function
       | None -> stog
       | Some (doc_id, doc) -> Stog_types.set_doc stog doc_id doc)
    stog docs

let level_fun_of_action a =
  (a.name, Stog_engine.Fun_stog (apply a.types a.command))

let level_funs stog =
  let config = load_config stog in
  List.map level_fun_of_action config.actions
;;

let default_levels = Stog_types.Str_map.empty

let make_module stog ?levels () =
  let level_funs = level_funs stog in
  let levels = Stog_html.mk_levels
    module_name level_funs default_levels ?levels ()
  in
  let module M =
  struct
    type data = unit
    let modul = {
        Stog_engine.mod_name = module_name ;
        mod_levels = levels ;
        mod_data = ()
       }

    type cache_data = unit
    let cache_load _stog data doc t = data
    let cache_store _stog data doc = ()
  end
  in
  (module M : Stog_engine.Module)
;;

let f stog =
  let levels =
    try Some (Stog_types.Str_map.find module_name stog.Stog_types.stog_levels)
    with Not_found -> None
  in
  make_module stog ?levels ()
;;

let () = Stog_engine.register_module module_name f;;
