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

open Stog_types;;
module Smap = Stog_types.Str_map;;

type 'a level_fun =
  | Fun_stog of (stog Xtmpl.env -> stog -> Stog_types.Doc_set.t -> stog)
  | Fun_data of ('a Xtmpl.env -> stog * 'a -> Stog_types.Doc_set.t -> stog * 'a)
  | Fun_stog_data of ((stog * 'a) Xtmpl.env -> stog * 'a -> Stog_types.Doc_set.t -> stog * 'a)

type 'a modul = {
      mod_data : 'a ;
      mod_levels : 'a level_fun Stog_types.Int_map.t ;
      mod_name : string ;
    }

module type Module = sig
    type data
    val modul : data modul

    type cache_data
    val cache_load : stog -> data -> doc -> cache_data -> data
    val cache_store : stog -> data -> doc -> cache_data
  end

type stog_state =
  { st_stog : stog ;
    st_modules : (module Module) list ;
    st_docs : Stog_types.Doc_set.t ;
  };;

let apply_module level env state modul =
  let module E = (val modul : Module) in
  match
    try Some (Int_map.find level E.modul.mod_levels)
    with Not_found -> None
  with
    None -> { state with st_modules = modul :: state.st_modules }
  | Some f ->
      let old_stog = state.st_stog in
      let stog = state.st_stog in
      let (stog, data) =
        try
          match f with
            Fun_stog f ->
              let stog = f env stog state.st_docs in
              (stog, E.modul.mod_data)
          | Fun_data f ->
              f (Obj.magic env) (stog, E.modul.mod_data) state.st_docs
          | Fun_stog_data f ->
              f (Obj.magic env) (stog, E.modul.mod_data) state.st_docs
        with
          Xtmpl.Loop stack ->
            let msg = "Possible loop when applying module "^
              E.modul.mod_name^" on level "^(string_of_int level)^":\n"^
              (Xtmpl.string_of_stack stack)
            in
            failwith msg
      in
      let modul =
        let module E2 = struct
          type data = E.data
          let modul = { E.modul with mod_data = data }
          type cache_data = E.cache_data
          let cache_load = E.cache_load
          let cache_store = E.cache_store
          end
        in
        (module E2 : Module)
      in
      let docs =
        let f id d set =
          match d.doc_out with
            None ->
              (* new element of existing element recreated *)
              Stog_types.Doc_set.add id set
          | _ ->
              try ignore(Stog_tmap.get old_stog.stog_docs id); set
              with _ ->
                  (* new element *)
                  Stog_types.Doc_set.add id set
        in
        Stog_tmap.fold f stog.stog_docs state.st_docs
      in
      { st_stog = stog ;
        st_modules = modul :: state.st_modules ;
        st_docs = docs ;
      }
;;

let (compute_level : 'a Xtmpl.env -> int -> stog_state -> stog_state) =
  fun env level state ->
    Stog_msg.verbose (Printf.sprintf "Computing level %d" level);
    let state = List.fold_left (apply_module level env)
      { state with st_modules = [] } state.st_modules
    in
    state
;;

let levels =
  let add level _ set = Stog_types.Int_set.add level set in
  let f set m =
    let module M = (val m : Module) in
    Stog_types.Int_map.fold add M.modul.mod_levels set
  in
  fun state ->
    List.fold_left f Stog_types.Int_set.empty state.st_modules
;;

(***** Caching *****)


let cache_info_file stog = Filename.concat stog.stog_cache_dir "info";;
let stog_cache_name = "_stog";;

let cache_file name stog doc =
  let cache_dir = Filename.concat stog.stog_cache_dir name in
  Filename.concat cache_dir
    ((String.concat "/" doc.doc_path.Stog_path.path)^"._doc")
;;

let get_cached_docs stog =
  let doc_dir = Filename.concat stog.stog_cache_dir stog_cache_name in
  let files = Stog_find.find_list Stog_find.Ignore [doc_dir]
    [Stog_find.Type Unix.S_REG]
  in
  let load acc file =
    try
      let ic = open_in_bin file in
      let (doc : Stog_types.doc) = input_value ic in
      close_in ic;
      doc :: acc
    with
      Failure s | Sys_error s ->
        Stog_msg.warning s;
        acc
  in
  let docs = List.fold_left load [] files in
  Stog_msg.verbose ~info: "cache" ~level: 5
    ((string_of_int (List.length docs))^" documents found in cache");
  docs
;;

let set_doc_env doc stog doc_envs =
  let digest = Stog_types.stog_md5 stog in
  Stog_path.Map.add doc.doc_path digest doc_envs
;;

let apply_loaders state doc =
  let f_modul e =
    let module E = (val e : Module) in
    let cache_file = cache_file E.modul.mod_name state.st_stog doc in
    let ic = open_in_bin cache_file in
    let t = input_value ic in
    close_in ic;
    let data = E.cache_load state.st_stog E.modul.mod_data doc t in
    let module E2 = struct
      type data = E.data
      let modul = { E.modul with mod_data = data }
      type cache_data = E.cache_data
      let cache_load = E.cache_load
      let cache_store = E.cache_store
     end
    in
    (module E2 : Module)
  in
  let modules = List.map f_modul state.st_modules in
  let state = { state with st_modules = modules } in
  state
;;

let apply_storers state doc =
  let f_modul e =
    let module E = (val e : Module) in
    let cache_file = cache_file E.modul.mod_name state.st_stog doc in
    let cache_dir = Filename.dirname cache_file in
    Stog_misc.safe_mkdir cache_dir ;
    let oc = open_out_bin cache_file in
    let t = E.cache_store state.st_stog E.modul.mod_data doc in
    Marshal.to_channel oc t [Marshal.Closures];
    close_out oc
  in
  List.iter f_modul state.st_modules
;;

let get_cached_documents state env =
  Stog_misc.safe_mkdir state.st_stog.stog_cache_dir;
  let info_file = cache_info_file state.st_stog in
  let (doc_envs, stog) =
    if Sys.file_exists info_file then
      begin
        let ic = open_in_bin info_file in
        let ((doc_envs, deps, id_map) :
           (string Stog_path.Map.t * Stog_types.Depset.t Smap.t *
            (Stog_path.path * string option) Smap.t Stog_path.Map.t)) =
          input_value ic
        in
        close_in ic;
        let stog = { state.st_stog with
            stog_deps = deps ;
            stog_id_map = id_map ;
          }
        in
        (doc_envs, stog)
      end
    else
      (Stog_path.Map.empty, state.st_stog)
  in
  let state = { state with st_stog = stog } in
  let digest = Stog_types.stog_md5 state.st_stog in
  Stog_msg.verbose ~info:"cache" ~level: 5 ("digest(stog)="^digest);

  let docs = get_cached_docs state.st_stog in
  let doc_by_path =
    let map = List.fold_left
      (fun map doc -> Stog_types.Str_map.add
         (Stog_path.to_string doc.doc_path) doc map)
        Stog_types.Str_map.empty docs
    in
    fun path -> Stog_types.Str_map.find path map
  in
  let f (state, cached, kept_deps, kept_id_map) doc =
    let path = doc.doc_path in
    let same_doc_env =
      try
        let d = Stog_path.Map.find path doc_envs in
        Stog_msg.verbose ~info: "cache" ~level: 5
          ("digest(path="^(Stog_path.to_string path)^",stog)="^d);
        d = digest
      with Not_found ->
          Stog_msg.verbose ~info: "cache" ~level: 5
            ("cached doc "^(Stog_path.to_string path)^" not found in stog");
          false
    in
    let use_cache =
      if same_doc_env then
        begin
          let src_cache_file = cache_file stog_cache_name state.st_stog doc in
          let src_cache_time = Stog_misc.file_mtime src_cache_file in
          let deps_time = Stog_deps.max_deps_date state.st_stog doc_by_path
            (Stog_path.to_string doc.doc_path)
          in
          Stog_msg.verbose ~info: "cache" ~level: 5
           (Printf.sprintf "deps_time for %S = %s, last generated on %s" src_cache_file
             (Stog_misc.string_of_time deps_time)
             (match src_cache_time with None -> "" | Some d -> Stog_misc.string_of_time d)
            );
          match src_cache_time with
            None -> false
          | Some t_doc -> deps_time < t_doc
        end
      else
        (
         Stog_msg.verbose ~info: "cache" ~level: 5
           ("path="^(Stog_path.to_string path)^": not same env");
         false
        )
    in
    if use_cache then
      begin
        let state = apply_loaders state doc in
        (* keep deps of this document, as it did not change *)
        let kept_deps =
          let spath = Stog_path.to_string path in
          try Smap.add spath (Smap.find spath state.st_stog.stog_deps) kept_deps
          with Not_found -> kept_deps
        in
        let kept_id_map =
          try Stog_path.Map.add path
            (Stog_path.Map.find path state.st_stog.stog_id_map) kept_id_map
          with
            Not_found -> kept_id_map
        in
        (state, doc :: cached, kept_deps, kept_id_map)
      end
    else
      begin
        (* do not keep deps of this document, as it will be recomputed *)
        (state, cached, kept_deps, kept_id_map)
      end
  in
  let (state, cached, kept_deps, kept_id_map) =
    List.fold_left f
      (state, [], Smap.empty, Stog_path.Map.empty) docs
  in
  let stog = {
      state.st_stog with
      stog_deps = kept_deps ;
      stog_id_map = kept_id_map ;
    }
  in
  let state = { state with st_stog = stog } in
  (state, cached)
;;

let cache_doc state doc =
  let cache_file = cache_file stog_cache_name state.st_stog doc in
  Stog_misc.safe_mkdir (Filename.dirname cache_file);
  let oc = open_out_bin cache_file in
  output_value oc doc ;
  close_out oc ;
  apply_storers state doc
;;

let output_cache_info stog doc_envs =
  let info_file = cache_info_file stog in
  (*Stog_tmap.iter
    (fun doc_id doc ->
       ignore(Stog_deps.max_deps_date stog
        (fun path -> snd (Stog_types.doc_by_path stog (Stog_path.of_string path)))
          (Stog_path.to_string doc.Stog_types.doc_path))
    )
       stog.Stog_types.stog_docs;
  *)
  let v = (doc_envs, stog.stog_deps, stog.stog_id_map) in
  let oc = open_out_bin info_file in
  output_value oc v;
  close_out oc
;;

let state_merge_cdata ?docs state =
 let docs =
    match docs with
      None -> Stog_types.doc_list state.st_stog
    | Some l -> List.map (fun id -> (id, Stog_types.doc state.st_stog id)) l
  in
  let f stog (doc_id, doc) =
    let doc =
      match doc.doc_out with
        None ->
          doc
      | Some xmls ->
          { doc with doc_out = Some (Xtmpl.merge_cdata_list xmls) }
    in
    Stog_types.set_doc stog doc_id doc
  in
  { state with st_stog = List.fold_left f state.st_stog docs }
;;

let set_docs_to_compute ?(use_cache=true) env state =
  match use_cache with
  | _ when not (Stog_types.Doc_set.is_empty state.st_docs) ->
      state
  | false ->
      let st_docs = Stog_tmap.fold
        (fun doc_id doc acc -> Stog_types.Doc_set.add doc_id acc)
        state.st_stog.stog_docs Stog_types.Doc_set.empty
      in
      { state with st_docs }
  | true ->
      let (state, cached) = get_cached_documents state env in
      Stog_msg.verbose (Printf.sprintf "%d documents kept from cache" (List.length cached));
      let f_doc (stog, cached) cached_doc =
        try
          let (doc_id, _) = Stog_types.doc_by_path stog cached_doc.doc_path in
          (* replace document by cached one *)
          let stog = Stog_types.set_doc stog doc_id cached_doc in
          (stog, Stog_types.Doc_set.add doc_id cached)
        with _ ->
            (* document not loaded but cached; keep it as it may be a
               document from a cut-doc rule *)
            let stog = Stog_types.add_doc stog cached_doc in
            let (doc_id, _) = Stog_types.doc_by_path stog cached_doc.doc_path in
            (stog, Stog_types.Doc_set.add doc_id cached)
      in
      let (stog, cached) = List.fold_left f_doc
        (state.st_stog, Stog_types.Doc_set.empty) cached
      in
      let st_docs =
        Stog_tmap.fold
          (fun doc_id doc acc ->
             if Stog_types.Doc_set.mem doc_id cached then
               acc
             else
               Stog_types.Doc_set.add doc_id acc
          )
          state.st_stog.stog_docs
          Stog_types.Doc_set.empty
      in
      { state with st_stog = stog ; st_docs }
;;


let compute_levels ?use_cache env state =
  let levels = levels state in
  let state = set_docs_to_compute ?use_cache env state in
  let state = Stog_types.Int_set.fold (compute_level env) levels state in
  state_merge_cdata state
;;


let rec make_fun (name, params, body) acc =
  let f data env atts subs =
    let vars = Xtmpl.Name_map.fold
      (fun param default acc ->
         match Xtmpl.get_arg atts param with
           None -> (param, Xtmpl.atts_empty, default) :: acc
         | Some v -> (param, Xtmpl.atts_empty, v) :: acc
      )
      params []
    in
    let env = env_of_defs ~env vars in
    let body = [ Xtmpl.E (("",Xtmpl.tag_env), atts, body) ] in
    let f data _ atts xmls =
      match Xtmpl.Name_map.is_empty atts, xmls with
        true, [] -> (data, subs)
      | _ -> raise Xtmpl.No_change
    in
    let env = Xtmpl.env_add "contents" f env in
    Xtmpl.apply_to_xmls data env body
  in
  (name, f) :: acc


and env_of_defs ?env defs =
  let f x acc =
    match x with
    | (key, atts, body) when Xtmpl.Name_map.is_empty atts ->
        (key, fun data _ _ _ -> (data, body)) :: acc
    | _ ->  make_fun x acc
  in
  (* fold_right instead of fold_left to reverse list and keep associations
     in the same order as in declarations *)
  let l = List.fold_right f defs [] in
  Xtmpl.env_of_list ?env l
;;

(** FIXME: handle module requirements and already added modules *)
(* FIXME: add dependency ? *)
let env_of_used_mod stog ?(env=Xtmpl.env_empty()) modname =
  try
    let m = Stog_types.Str_map.find modname stog.stog_modules in
    (*prerr_endline (Printf.sprintf "adding %d definitions from module %S"
      (List.length m.mod_defs) modname);*)
    env_of_defs ~env m.mod_defs
  with Not_found ->
    Stog_msg.warning (Printf.sprintf "No module %S" modname);
    env

let env_of_used_mods stog ?(env=Xtmpl.env_empty()) mods =
  Stog_types.Str_set.fold (fun name env -> env_of_used_mod stog ~env name) mods env
;;

let fun_site_url stog data _env _ _ =
  (data, [ Xtmpl.D (Stog_types.string_of_url stog.stog_base_url) ])
;;

let run ?(use_cache=true) state =
  let stog = state.st_stog in
  let dir =
    if Filename.is_relative stog.stog_dir then
      Filename.concat (Sys.getcwd ()) stog.stog_dir
    else
      stog.stog_dir
  in
  let env = Xtmpl.env_of_list
    [
      ("", Stog_tags.site_desc), (fun data _ _ _ -> (data, stog.stog_desc)) ;
      ("", Stog_tags.site_email), (fun data _ _ _ -> (data, [ Xtmpl.D stog.stog_email ])) ;
      ("", Stog_tags.site_title), (fun data _ _ _ -> (data, [ Xtmpl.D stog.stog_title ])) ;
      ("", Stog_tags.stog_dir), (fun data _ _ _ -> (data, [ Xtmpl.D dir ])) ;
      ("", Stog_tags.site_url), fun_site_url stog ;
    ]
  in
  let env = env_of_used_mods ~env stog stog.stog_used_mods in
  let env = env_of_defs ~env stog.stog_defs in
  compute_levels ~use_cache env state
;;

let encode_for_url s =
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    match s.[i] with
    | 'A'..'Z' | 'a'..'z' | '0'..'9'
    | '_' | '-' | '.' | '!' | '*' | '+' | '/' ->
        Buffer.add_char b s.[i]
    | c -> Printf.bprintf b "%%%0x" (Char.code c)
  done;
  Buffer.contents b
;;

let doc_dst f_concat ?(encode=true) stog base doc =
  let path =
    match doc.doc_path.Stog_path.path with
      [] -> failwith "Invalid path: []"
    | h :: q -> List.fold_left f_concat h q
  in
  let path =
    if doc.doc_lang_dep then
      begin
        let ext_pref =
          match stog.stog_lang with
            None -> ""
          | Some lang -> "."^lang
        in
        Printf.sprintf "%s%s" path ext_pref
      end
    else
      path
  in
  let dst = if encode then encode_for_url path else path in
  f_concat base dst
;;

let doc_dst_file stog doc =
  doc_dst ~encode: false Filename.concat stog stog.stog_outdir doc;;


let doc_url stog doc =
  let url = doc_dst (fun a b -> a^"/"^b) stog
    (Stog_types.string_of_url stog.stog_base_url) doc
  in
  let len = String.length url in
  let s = "/index.html" in
  let len_s = String.length s in
  let url =
    if len >= len_s && String.sub url (len - len_s) len_s = s then
      (String.sub url 0 (len-len_s))^"/"
    else
      url
  in
  url_of_string url
;;

let output_doc state doc =
  let file = doc_dst_file state.st_stog doc in
  Stog_misc.safe_mkdir (Filename.dirname file);
  match doc.doc_out with
    None ->
      failwith
        (Printf.sprintf "Element %S not computed!"
         (Stog_path.to_string doc.doc_path)
        )
  | Some xmls ->
      let oc = open_out file in
      let doctype =
        match doc.doc_xml_doctype with
          None -> "HTML"
        | Some s -> s
      in
      Printf.fprintf oc "<!DOCTYPE %s>\n" doctype;
      let xmls =
        match String.lowercase doctype with
          "html" -> List.map Stog_html5.hack_self_closed xmls
        | _ -> xmls
      in
      List.iter (fun xml -> output_string oc (Xtmpl.string_of_xml xml)) xmls;
      close_out oc;
      cache_doc state doc
;;

let output_docs ?docs state =
  let stog = state.st_stog in
  let docs =
    match docs with
      None ->
        Stog_tmap.fold
          (fun _ doc acc -> output_doc state doc ; doc :: acc)
          stog.stog_docs []
    | Some l -> List.iter (output_doc state) l; l
  in
  let doc_envs = List.fold_left
    (fun doc_envs doc -> set_doc_env doc stog doc_envs)
      Stog_path.Map.empty docs
  in
  output_cache_info stog doc_envs
;;

let copy_other_files stog =
  let report_error msg = Stog_msg.error ~info: "Stog_html.copy_other_files" msg in
  let copy_file src dst =
    let com = Printf.sprintf "cp -f %s %s" (Filename.quote src) (Filename.quote dst) in
    match Sys.command com with
      0 -> ()
    | n ->
        let msg = Printf.sprintf "Command failed [%d]: %s" n com in
        report_error msg
  in
  let f_file dst path name =
    let dst = Filename.concat dst name in
    let src = Filename.concat path name in
    copy_file src dst
  in
  let rec f_dir dst path name t =
    let dst = Filename.concat dst name in
    let path = Filename.concat path name in
    Stog_misc.safe_mkdir dst;
    iter dst path t
  and iter dst path t =
    Stog_types.Str_set.iter (f_file dst path) t.files ;
    Stog_types.Str_map.iter (f_dir dst path) t.dirs
  in
  iter stog.stog_outdir stog.stog_dir stog.stog_files
;;

let generate ?(use_cache=true) ?only_docs stog modules =
  begin
    match stog.stog_lang with
      None -> ()
    | Some lang -> Stog_msg.verbose (Printf.sprintf "Generating pages for language %s" lang);
  end;
  Stog_misc.safe_mkdir stog.stog_outdir;
  let only_docs =
    match only_docs with
      None -> None
    | Some l ->
        let f s =
          let path = Stog_path.of_string s in
          let (doc_id, _) = Stog_types.doc_by_path stog path in
          doc_id
        in
        Some (List.map f l)
  in
  let st_docs =
    match only_docs with
      None -> Stog_types.Doc_set.empty
    | Some ids ->
        let set =
          List.fold_right Stog_types.Doc_set.add
            ids Stog_types.Doc_set.empty
        in
        set
  in
  let state = { st_stog = stog ; st_modules = modules ; st_docs } in
  let state = run ~use_cache state in
  match only_docs with
    None ->
      output_docs state ;
      copy_other_files state.st_stog
  | Some ids ->
      let docs = List.map (Stog_types.doc state.st_stog) ids in
      output_docs ~docs state
;;


(*** Convenient functions to create level_fun's ***)


let get_in_env data env (prefix, s) =
  let node = [ Xtmpl.E((prefix,s), Xtmpl.atts_empty, []) ] in
  let (data, node2) = Xtmpl.apply_to_xmls data env node in
  if node2 = node then (data, []) else (data, node2)
;;

let opt_in_env data env (prefix, s) =
  let node = [ Xtmpl.E((prefix,s), Xtmpl.atts_empty, []) ] in
  let (data, node2) = Xtmpl.apply_to_xmls data env node in
  if node2 = node then (data, None) else (data, Some node2)
;;

let get_in_args_or_env data env args s =
  match Xtmpl.get_arg args s with
    None -> get_in_env data env s
  | Some xmls -> (data, xmls)
;;

let get_path data env =
  let (data, xmls) = get_in_env data env ("", Stog_tags.doc_path) in
  match Xtmpl.merge_cdata_list xmls with
    [Xtmpl.D s] -> (data, Stog_path.of_string s)
  | xmls -> Stog_path.invalid (Xtmpl.string_of_xmls xmls)
;;

let get_path_in_args_or_env data env args =
  let (data,x) = get_in_args_or_env data env args ("", Stog_tags.doc_path) in
  match Xtmpl.merge_cdata_list x with
    [Xtmpl.D s] -> (data, Stog_path.of_string s)
  | xmls -> Stog_path.invalid (Xtmpl.string_of_xmls xmls)
;;

let get_doc_out stog doc =
  match doc.doc_out with
    None ->
      let (stog, tmpl) =
        let default =
          match doc.doc_type with
            "by-topic" -> Stog_tmpl.by_topic
          | "by-keyword" -> Stog_tmpl.by_keyword
          | "by-month" -> Stog_tmpl.by_month
          | "rss" -> Stog_tmpl.rss
          | _ -> Stog_tmpl.page
        in
        Stog_tmpl.get_template stog ~doc default (doc.doc_type^".tmpl")
      in
      (stog, [tmpl])
  | Some xmls ->
      (stog, xmls)
;;

let get_languages data env =
  match opt_in_env data env ("", "languages") with
  | (data, Some [Xtmpl.D s]) -> (data, Stog_misc.split_string s [','; ';' ; ' '])
  | (data, Some xmls) ->
      failwith ("Invalid languages specification: "^(Xtmpl.string_of_xmls xmls))
  | (data, None) -> (data, ["fr" ; "en"])
;;

let env_add_lang_rules data env stog doc =
  match stog.stog_lang with
    None ->
      (data, Xtmpl.env_add Stog_tags.langswitch (fun data _ _ _ -> (data, [])) env)
  | Some lang ->
      let (data, languages) = get_languages data env in
      let map_lang lang =
        let url = doc_url { stog with stog_lang = Some lang } doc in
        let img_url = Stog_types.url_concat stog.stog_base_url (lang^".png") in
        Xtmpl.E (("", "a"),
         Xtmpl.atts_of_list [("", "href"), [ Xtmpl.D (Stog_types.string_of_url url) ]],
         [
           Xtmpl.E (("", "img"),
            Xtmpl.atts_of_list
              [ ("", "src"), [ Xtmpl.D (Stog_types.string_of_url img_url) ] ;
                ("", "title"), [ Xtmpl.D lang ] ;
                ("", "alt"), [ Xtmpl.D lang]
              ],
            [])]
        )
      in
      let f data _env args _subs =
        let languages = List.filter ((<>) lang) languages in
        (data, List.map map_lang languages)
      in
      let env = Xtmpl.env_add Stog_tags.langswitch f env in
      let to_remove = List.filter ((<>) lang) languages in
      let f_keep acc _env _args subs = (acc, subs) in
      let f_remove acc _env _args _subs = (acc, []) in
      let rules =
        (("", lang), f_keep) ::
          (List.map (fun lang -> (("", lang), f_remove)) to_remove)
      in
      (data, Xtmpl.env_of_list ~env rules)
;;

let doc_env data env stog doc =
  let env = env_of_used_mods stog ~env doc.doc_used_mods in
  let env = env_of_defs ~env doc.doc_defs in
(*  prerr_endline
    (Printf.sprintf "doc=%s\ndefs=%s"
      (Stog_path.to_string doc.doc_path)
      (String.concat "\n"
        (List.map (fun ((p,name),_,subs) -> Printf.sprintf "%s:%s=>%s" p name (Xtmpl.string_of_xmls subs))
          doc.doc_defs)
      )
    );
  prerr_endline ("env_of_defs => "^(Xtmpl.string_of_env env));
*)
  let rules = [
      ("", Stog_tags.doc_path),
      (fun  acc _ _ _ ->
         (acc, [Xtmpl.D (Stog_path.to_string doc.doc_path)]))
    ]
  in
  let env = Xtmpl.env_of_list ~env rules in
  let (data, env) = env_add_lang_rules data env stog doc in
  (data, env)

type 'a stog_doc_rules =
  Stog_types.stog -> Stog_types.doc_id -> (Xtmpl.name * 'a Xtmpl.callback) list

let apply_stog_env_doc stog env doc_id =
  let doc = Stog_types.doc stog doc_id in
  let (stog, env) = doc_env stog env stog doc in
  let (stog, xmls) = get_doc_out stog doc in
  (*prerr_endline (Printf.sprintf "%s = %s"
     (Stog_path.to_string doc.doc_path)
     (Xtmpl.string_of_xsmls xmls));*)
  let (stog, xmls) = Xtmpl.apply_to_xmls stog env xmls in
  let doc = { doc with doc_out = Some xmls } in
  Stog_types.set_doc stog doc_id doc
;;

let apply_stog_data_env_doc (stog,data) env doc_id =
  let doc = Stog_types.doc stog doc_id in
  let ((stog, data), env) = doc_env (stog, data) env stog doc in
  let (stog, xmls) = get_doc_out stog doc in
  (*prerr_endline (Printf.sprintf "env=%s" (Xtmpl.string_of_env env));*)
  let ((stog, data), xmls) = Xtmpl.apply_to_xmls (stog, data) env xmls in
  let doc = { doc with doc_out = Some xmls } in
  (Stog_types.set_doc stog doc_id doc, data)

let apply_data_env_doc (stog,data) env doc_id =
  let doc = Stog_types.doc stog doc_id in
  let (data, env) = doc_env data env stog doc in
  let (stog, xmls) = get_doc_out stog doc in
  let (data, xmls) = Xtmpl.apply_to_xmls data env xmls in
  let doc = { doc with doc_out = Some xmls } in
  (Stog_types.set_doc stog doc_id doc, data)
;;

let fun_apply_stog_doc_rules f_rules =
  let f_doc env doc_id stog =
    let rules = f_rules stog doc_id in
    let env = Xtmpl.env_of_list ~env rules in
    apply_stog_env_doc stog env doc_id
  in
  let f env stog docs = Stog_types.Doc_set.fold (f_doc env) docs stog in
  Fun_stog f
;;

let fun_apply_stog_data_doc_rules f_rules =
  let f_doc env doc_id (stog, data) =
    let rules = f_rules stog doc_id in
    let env = Xtmpl.env_of_list ~env rules in
    apply_stog_data_env_doc (stog,data) env doc_id
  in
  let f env (stog, data) docs =
    Stog_types.Doc_set.fold (f_doc env) docs (stog, data)
  in
  Fun_stog_data f
;;

let fun_apply_data_doc_rules f_rules =
  let f_doc env doc_id (stog, data) =
    let rules = f_rules stog doc_id in
    let env = Xtmpl.env_of_list ~env rules in
    apply_data_env_doc (stog,data) env doc_id
  in
  let f env (stog, data) docs =
    Stog_types.Doc_set.fold (f_doc env) docs (stog, data)
  in
  Fun_data f
;;

(*** Engines ***)

type module_fun = Stog_types.stog -> (module Module)

let modules = ref (Stog_types.Str_map.empty : (Stog_types.stog -> (module Module)) Stog_types.Str_map.t);;

let register_module name f_mod =
  modules := Stog_types.Str_map.add name f_mod !modules
;;

let module_by_name name =
  try Some (Stog_types.Str_map.find name !modules)
  with Not_found -> None
;;

let modules () = Stog_types.Str_map.fold
  (fun name f acc -> (name, f) :: acc) !modules []
;;

