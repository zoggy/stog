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

(** A plugin to change URIs in href attributes to relative ones.
  By now it only rewrite URIs of a document to a block of the
  same doc URI#id to #id. *)


module XR = Xtmpl_rewrite
module Xml = Xtmpl_xml

open Stog_types

let rec rewrite_href url xml =
  match xml with
  | XR.D _ | XR.C _ | XR.PI _ -> xml
  | XR.E node ->
    let atts = Xml.Name_map.mapi
      (fun att v ->
         match att, v with
           (pref,"href"), [XR.D href] ->
             let href = href.Xml.text in
             begin
               let url2 =
                 try
                   let href_url = Stog_url.of_string href in
                   let url2 =
                       Stog_url.with_fragment
                         (Stog_url.remove_query href_url)
                         None
                   in
                   Some (Stog_url.to_string url2)
                 with
                   Failure _ -> None
               in
               match url2 with
                 None -> v
                 | Some url2 ->
                   if String.compare url url2 = 0 then
                     begin
                       let len = String.length url in
                       let len2 = String.length href in
                       if len2 <= len then
                         [XR.cdata ""]
                       else
                         [XR.cdata (String.sub href len (len2 - len)) ]
                       end
                   else
                     [XR.cdata href]
             end
        | _ -> v
      )
      node.XR.atts
    in
    XR.E { node with XR.atts ; subs = List.map (rewrite_href url) node.XR.subs }

let rewrite_doc stog doc =
  let xmls =
    match doc.doc_out with
      None -> doc.doc_body
    | Some b -> b
  in
  let url = Stog_url.to_string (Stog_engine.doc_url stog doc) in
  let xmls = List.map (rewrite_href url) xmls in
  { doc with doc_out = Some xmls }
;;

let rewrite =
  let f_doc doc_id stog =
    let doc = Stog_types.doc stog doc_id in
    let doc = rewrite_doc stog doc in
    Stog_types.set_doc stog doc_id doc
  in
  let f env stog docs = Stog_types.Doc_set.fold f_doc docs stog in
  Stog_engine.Fun_stog f

let level_funs = [ "rewrite", rewrite ]

let default_levels =
  List.fold_left
    (fun map (name, levels) -> Stog_types.Str_map.add name levels map)
    Stog_types.Str_map.empty
    [
      "rewrite", [ 400 ] ;
    ]

let module_name = "rel-href";;

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
  make_module ?levels ()
;;

let () = Stog_engine.register_module module_name f;;