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
module Smap = Stog_types.Str_map;;

type 'a level_fun = Xtmpl.env -> (stog * 'a) -> (elt_id * elt) list -> (stog * 'a)

type 'a engine = {
      eng_data : 'a ;
      eng_levels : 'a level_fun Stog_types.Int_map.t ;
      eng_name : string ;
    }

module type Stog_engine = sig
    type data
    val engine : data engine

    type cache_data
    val cache_load : data -> elt -> cache_data -> data
    val cache_store : data -> elt -> cache_data
  end

type stog_state =
  { st_stog : stog ;
    st_engines : (module Stog_engine) list ;
  };;


let compute_level ?elts ?cached env level state =
  Stog_msg.verbose (Printf.sprintf "Computing level %d" level);
  let elts =
    match elts, cached with
      None, None ->
        Stog_tmap.fold (fun elt_id elt acc -> (elt_id, elt) :: acc) state.st_stog.stog_elts []
    | None, Some l ->
        let pred id1 id2 = Stog_tmap.compare_key id1 id2 = 0 in
        Stog_tmap.fold
          (fun elt_id elt acc ->
             if List.exists (pred elt_id) l then acc else (elt_id, elt) :: acc)
             state.st_stog.stog_elts []
    | Some l, _ ->
        List.map
        (fun elt_id -> (elt_id, Stog_types.elt state.st_stog elt_id))
        l
  in
  let f_elt f (stog, data) (elt_id, elt) =
    let elt = f env (stog, data) elt_id elt in
    Stog_types.set_elt stog elt_id elt
  in
  let f_engine state engine =
    let module E = (val engine : Stog_engine) in
    match
      try Some (Int_map.find level E.engine.eng_levels)
      with Not_found -> None
    with
      None -> { state with st_engines = engine :: state.st_engines }
    | Some f ->
        let (stog, data) = f env (state.st_stog, E.engine.eng_data) elts in
        let engine =
          let module E2 = struct
             type data = E.data
             let engine = { E.engine with eng_data = data }
             type cache_data = E.cache_data
             let cache_load = E.cache_load
             let cache_store = E.cache_store
            end
          in
          (module E2 : Stog_engine)
        in
        { st_stog = stog ; st_engines = engine :: state.st_engines }

  in
  let state = List.fold_left f_engine
    { state with st_engines = [] } state.st_engines
  in
  state
(*
  let f_fun stog f =
    match f with
      On_elt f -> List.fold_left (f_elt f) stog elts
    | On_elt_list f ->
        let (modified, added) = f env stog elts in
        let stog = List.fold_left
          (fun stog (elt_id, elt) -> Stog_types.set_elt stog elt_id elt)
          stog modified
        in
        List.fold_left Stog_types.add_elt stog added
  in
  List.fold_left f_fun stog funs
*)
;;

(*
let load_cached_elt file =
  let ic = open_in_bin file in
  let (t : Stog_types.cached_elt) = input_value ic in
  close_in ic;
  let hid = Stog_types.string_of_human_id t.cache_elt.elt_human_id in
  blocks := Smap.add hid t.cache_blocks !blocks;
  t.cache_elt
;;
*)

let levels =
  let add level _ set = Stog_types.Int_set.add level set in
  let f set m =
    let module M = (val m : Stog_engine) in
    Stog_types.Int_map.fold add M.engine.eng_levels set
  in
  fun state ->
    List.fold_left f Stog_types.Int_set.empty state.st_engines
;;

(***** Caching *****)


let cache_info_file stog = Filename.concat stog.stog_cache_dir "info";;
let stog_cache_name = "_stog";;

let cache_file name stog elt =
  let cache_dir = Filename.concat
    stog.stog_cache_dir name
  in
  Filename.concat cache_dir
    ((String.concat "/" elt.elt_human_id.hid_path)^"._elt")
;;

let get_cached_elts stog =
  let elt_dir = Filename.concat stog.stog_cache_dir stog_cache_name in
  let files = Stog_find.find_list Stog_find.Ignore [elt_dir]
    [Stog_find.Type Unix.S_REG]
  in
  let load acc file =
    try
      let ic = open_in_bin file in
      let (elt : Stog_types.elt) = input_value ic in
      close_in ic;
      elt :: acc
    with
      Failure s | Sys_error s ->
        Stog_msg.warning s;
        acc
  in
  List.fold_left load [] files
;;
let stog_env_digest stog env =
  let md5_env =
    try Digest.string (Marshal.to_string env [Marshal.Closures])
    with Invalid_argument msg ->
        let msg = Printf.sprintf
          "%s\n  This may be due to marshalling dynamically loaded code, which is\n  \
          not supported in all ocaml releases (use the trunk development version\n  \
          to get this support)." msg
        in
        Stog_msg.warning msg;
        Digest.string ""
  in
  let md5_stog = Stog_types.stog_md5 stog in
  (Digest.to_hex md5_stog) ^ (Digest.to_hex md5_env)
;;

let set_elt_env elt stog env elt_envs =
  let hid = Stog_types.string_of_human_id elt.elt_human_id in
  let digest = stog_env_digest stog env in
  Smap.add hid digest elt_envs
;;

let apply_loaders state elt =
  let f_engine e =
    let module E = (val e : Stog_engine) in
    let cache_file = cache_file E.engine.eng_name state.st_stog elt in
    let ic = open_in_bin cache_file in
    let t = input_value ic in
    close_in ic;
    let data = E.cache_load E.engine.eng_data elt t in
    let module E2 = struct
      type data = E.data
      let engine = { E.engine with eng_data = data }
      type cache_data = E.cache_data
      let cache_load = E.cache_load
      let cache_store = E.cache_store
     end
    in
    (module E2 : Stog_engine)
  in
  let engines = List.map f_engine state.st_engines in
  let state = { state with st_engines = engines } in
  state
;;

let apply_storers state elt =
  let f_engine e =
    let module E = (val e : Stog_engine) in
    let cache_file = cache_file E.engine.eng_name state.st_stog elt in
    let cache_dir = Filename.dirname cache_file in
    Stog_misc.safe_mkdir cache_dir ;
    let oc = open_out_bin cache_file in
    let t = E.cache_store E.engine.eng_data elt in
    output_value oc t;
    close_out oc
  in
  List.iter f_engine state.st_engines
;;

let get_cached_elements state env =
  Stog_misc.safe_mkdir state.st_stog.stog_cache_dir;
  let info_file = cache_info_file state.st_stog in
  let (elt_envs, stog) =
    if Sys.file_exists info_file then
      begin
        let ic = open_in_bin info_file in
        let ((elt_envs, deps) : (string Smap.t * Stog_types.Depset.t Smap.t)) = input_value ic in
        close_in ic;
        let stog = { state.st_stog with stog_deps = deps } in
        (elt_envs, stog)
      end
    else
      (Smap.empty, state.st_stog)
  in
  let state = { state with st_stog = stog } in
  let digest = stog_env_digest state.st_stog env in

  let elts = get_cached_elts state.st_stog in
  let elt_by_hid =
    let map = List.fold_left
      (fun map elt -> Stog_types.Str_map.add
         (Stog_types.string_of_human_id elt.elt_human_id) elt map)
        Stog_types.Str_map.empty elts
    in
    fun hid -> Stog_types.Str_map.find hid map
  in
  let f (state, cached, kept_deps) elt =
    let hid = Stog_types.string_of_human_id elt.elt_human_id in
    let same_elt_env =
      try
        let d = Smap.find hid elt_envs in
        d = digest
      with Not_found -> false
    in
    let use_cache =
      if same_elt_env then
        begin
          let src_cache_file = cache_file stog_cache_name stog elt in
          let src_cache_time = Stog_misc.file_mtime src_cache_file in
          let deps_time = Stog_deps.max_deps_date state.st_stog elt_by_hid
            (Stog_types.string_of_human_id elt.elt_human_id)
          in
          Stog_msg.verbose ~level: 5
           (Printf.sprintf "deps_time for %S = %s, last generated on %s" src_cache_file
             (Stog_misc.string_of_time deps_time)
             (match src_cache_time with None -> "" | Some d -> Stog_misc.string_of_time d)
            );
          match src_cache_time with
            None -> false
          | Some t_elt -> deps_time < t_elt
        end
      else
         false
    in
    if use_cache then
      begin
        let state = apply_loaders state elt in
        (* keep deps of this element, as it did not changed *)
        let kept_deps =
          try Smap.add hid (Smap.find hid !Stog_deps.deps) kept_deps
          with Not_found -> kept_deps
        in
        (state, elt :: cached, kept_deps)
      end
    else
      begin
        (* do not keep deps of this element, as it will be recomputed *)
        (state, cached, kept_deps)
      end
  in
  let (state, cached, kept_deps) = List.fold_left f (state, [], Smap.empty) elts in
  let stog = { state.st_stog with stog_deps = kept_deps } in
  let state = { state with st_stog = stog } in
  (state, cached)
;;

let cache_elt state elt =
  let cache_file = cache_file stog_cache_name state.st_stog elt in
  Stog_misc.safe_mkdir (Filename.dirname cache_file);
  let oc = open_out_bin cache_file in
  output_value oc elt ;
  close_out oc ;
  apply_storers state elt
;;

let output_cache_info stog =
  let info_file = cache_info_file stog in
  let v = (!elt_envs, !Stog_deps.deps) in
  let oc = open_out_bin info_file in
  output_value oc v;
  close_out oc
;;



let compute_levels ?(use_cache=true) ?elts env state =
  let levels = levels state in
  if use_cache then
    begin
      let (state, cached) = Stog_cache.get_cached_elements state env in
      Stog_msg.verbose (Printf.sprintf "%d elements read from cache" (List.length cached));
      let f_elt (stog, cached) cached_elt =
        try
          let (elt_id, _) = Stog_types.elt_by_human_id stog cached_elt.elt_human_id in
          (* replace element by cached one *)
          let stog = Stog_types.set_elt stog elt_id cached_elt in
          (stog, elt_id :: cached)
        with _ ->
            (* element not loaded but cached; keep it as it may be an
              element from a cut-elt rule *)
            let stog = Stog_types.add_elt stog cached_elt in
            let (elt_id, _) = Stog_types.elt_by_human_id stog cached_elt.elt_human_id in
            (stog, elt_id :: cached)
      in
      let (stog, cached) = List.fold_left f_elt (stog, []) cached in
      let state = { state with st_stog = stog } in
      Stog_types.Int_set.fold (compute_level ~cached env) levels state
    end
  else
    Stog_types.Int_set.fold (compute_level ?elts env) levels state
;;

let run ?(use_cache=true) ?only_elt state =
  let stog = state.st_stog in
  let env = env_of_defs stog.stog_defs in
  let env = env_of_used_mods stog ~env stog.stog_used_mods in
  match only_elt with
    None ->
      let stog = make_topic_indexes stog env in
      let stog = make_keyword_indexes stog env in
      let stog = make_archive_index stog env in
      let state = { state with st_stog = stog } in
      let state = compute_levels ~use_cache env state in
      Stog_ocaml.close_sessions(); (* TODO: move this in a high level fun, e.g. 2000 *)
      state
  | Some elt_id ->
      let state = compute_levels ~use_cache ~elts: [elt_id] env state in
      state
;;

let generate ?(use_cache=true) ?only_elt stog engines =
  begin
    match state.st_stog.stog_lang with
      None -> ()
    | Some lang -> Stog_msg.verbose (Printf.sprintf "Generating pages for language %s" lang);
  end;
  Stog_misc.safe_mkdir state.st_stog.stog_outdir;
  let only_elt =
    match only_elt with
      None -> None
    | Some s ->
        let hid = Stog_types.human_id_of_string s in
        let (elt_id, _) = Stog_types.elt_by_human_id stog hid in
        Some elt_id
  in
  let state = { st_stog = stog ; st_engines = engines } in
  let state = run ~use_cache ?only_elt state in
  match only_elt with
    None ->
      output_elts state env;
      copy_other_files stog
  | Some elt_id ->
      let elt = Stog_types.elt state.st_stog elt_id in
      output_elts ~elts: [elt] state env
;;
