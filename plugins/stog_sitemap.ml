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

let module_name = "sitemap";;
let rc_file stog = Stog_plug.plugin_config_file stog module_name;;

type sitemap_data =
  { default_by_type : (bool * string option * string option) Stog_types.Str_map.t ;
    out_file : string ;
  }

let load_config _ (stog,data) _ =
  let module CF = Config_file in
  let group = new CF.group in
  let default =
    let default =
      Stog_types.Str_map.fold
        (fun s (b,freq,prio) acc ->
          let b = if b then "true" else "false" in
          let freq = Stog_misc.string_of_opt freq in
           let prio = Stog_misc.string_of_opt prio in
           (s, (b, freq, prio)) :: acc) data.default_by_type []
    in
    new CF.list_cp
      (CF.tuple2_wrappers CF.string_wrappers
       (CF.tuple3_wrappers CF.string_wrappers
        CF.string_wrappers CF.string_wrappers)
      ) ~group
      ["in-sitemap"] default
      "pairs (doc type, (true|false, \"\"|always|hourly|daily|weekly|monthly|yearly|never, 0..1.0) telling whether default is to generate documents of the given type in the sitemap, the default change frequency and priority."
  in
  let out_path = new CF.string_cp ~group ["out_file"] data.out_file
    "file where to generate the sitemap"
  in
  let rc_file = rc_file stog in
  group#read rc_file;
  group#write rc_file;

  let default_by_type =
    let f acc (typ, (b, freq, prio)) =
      Stog_types.Str_map.add typ
        (b <> "false",
         Stog_misc.opt_of_string freq,
         Stog_misc.opt_of_string prio)
        acc
    in
    List.fold_left f Stog_types.Str_map.empty default#get
  in
  let data = { out_file = out_path#get ; default_by_type } in
  (stog, data)
;;

type url_entry = {
    url_loc : Stog_url.t ;
    url_lastmod : Stog_types.date ;
    url_freq : string option ;
    url_prio : string option ;
  }

let gen_sitemap stog data entries =
  let f_entry e =
    Xtmpl.(
     node ("","url")
      ((node ("","loc") [cdata (Stog_url.to_string e.url_loc)]) ::
        (node ("","lastmod")
         [cdata (Netdate.format ~fmt: "%d %b %Y %T %z" e.url_lastmod)]
        ) ::
          (match e.url_freq with
             None -> []
           | Some s -> [node ("","changefreq") [cdata s]]) @
          (match e.url_prio with
             None -> []
           | Some s -> [node ("","priority") [cdata s]])
       )
    )
  in
  let atts = Xtmpl.atts_one ("","xmlns")
    [Xtmpl.cdata "http://www.sitemaps.org/schemas/sitemap/0.9"]
  in
  let body =
      Xtmpl.node ("","urlset") ~atts (List.map f_entry entries)
  in
  let xml = Xtmpl.string_of_xml ~xml_atts: false body in
  let file = Filename.concat stog.stog_dir data.out_file in
  Stog_misc.file_of_string ~file xml

let generate =
  let f_doc stog data doc_id doc acc =
    let (default_in, default_freq, default_prio) =
      try Stog_types.Str_map.find doc.doc_type data.default_by_type
      with Not_found -> (true, None, None)
    in
    match
      match Stog_types.get_def doc.doc_defs ("","in-sitemap") with
        None -> default_in
      | Some (_, [Xtmpl.D "false"]) -> false
      | _ -> true
    with
      false -> acc
    | true ->
        let url_lastmod = Stog_types.today () in
        let url_freq =
          match Stog_types.get_def doc.doc_defs ("","sitemap-frequency") with
          | Some (_, [Xtmpl.D s]) -> Stog_misc.opt_of_string s
          | _ -> default_freq
        in
        let url_prio =
          match Stog_types.get_def doc.doc_defs ("","sitemap-priority") with
          | Some (_, [Xtmpl.D s]) -> Stog_misc.opt_of_string s
          | _ -> default_prio
        in
        { url_loc = Stog_engine.doc_url stog doc ;
          url_lastmod ; url_freq ; url_prio ;
        } :: acc
  in
  fun env (stog, data) _docs ->
    let entries = Stog_tmap.fold (f_doc stog data) stog.stog_docs [] in
    gen_sitemap stog data entries ;
    (stog, data)
;;


let level_funs =
  [
    "load-config", Stog_engine.Fun_stog_data load_config ;
    "generate", Stog_engine.Fun_stog_data generate ;
  ]
;;

let default_levels =
  List.fold_left
    (fun map (name, levels) -> Stog_types.Str_map.add name levels map)
    Stog_types.Str_map.empty
    [
      "load-config", [ -2 ] ;
      "generate", [ 1000 ] ;
    ]

let default_data  =
  { out_file = "sitemap.xml" ; 
    default_by_type = Stog_types.Str_map.empty ;
  }

let make_module ?levels () =
  let levels = Stog_html.mk_levels module_name level_funs default_levels ?levels () in
  let module M =
  struct
    type data = sitemap_data
    let modul = {
        Stog_engine.mod_name = module_name ;
        mod_levels = levels ;
        mod_data = default_data ;
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