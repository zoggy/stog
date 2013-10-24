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

module type Cache = sig
  type t
  val name : string
  val load : Stog_types.elt -> t -> unit
  val store : Stog_types.elt -> t
end;;

let cache_info_file stog = Filename.concat stog.stog_cache_dir "info";;
let stog_cache_name = "_stog";;

let loaders = ref [];;
let storers = ref [];;
let elt_envs = ref Smap.empty;;

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

let register_cache cache =
  let module Cache = (val cache : Cache) in
  let loader stog elt =
    let cache_file = cache_file Cache.name stog elt in
    let ic = open_in_bin cache_file in
    let (t : Cache.t) = input_value ic in
    close_in ic;
    Cache.load elt t
  in
  loaders := loader :: !loaders;

  let storer stog elt =
    let cache_file = cache_file Cache.name stog elt in
    let cache_dir = Filename.dirname cache_file in
    Stog_misc.safe_mkdir cache_dir ;
    let t = Cache.store elt in
    let oc = open_out_bin cache_file in
    output_value oc t;
    close_out oc
  in
  storers := storer :: !storers
;;

let apply_loaders stog elt =  List.iter (fun f -> f stog elt) !loaders;;
let apply_storers stog elt =  List.iter (fun f -> f stog elt) !storers;;

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

let set_elt_env elt stog env =
  let hid = Stog_types.string_of_human_id elt.elt_human_id in
  let digest = stog_env_digest stog env in
  elt_envs := Smap.add hid digest !elt_envs
;;

let get_cached_elements stog env =
  Stog_misc.safe_mkdir stog.stog_cache_dir;
  let info_file = cache_info_file stog in
  if Sys.file_exists info_file then
    begin
      let ic = open_in_bin info_file in
      let (v : (string Smap.t * Stog_deps.Depset.t Smap.t)) = input_value ic in
      close_in ic;
      elt_envs := fst v ;
      Stog_deps.deps := snd v
    end;
  let digest = stog_env_digest stog env in

  let elts = get_cached_elts stog in
  let elt_by_hid =
    let map = List.fold_left
      (fun map elt -> Stog_types.Str_map.add
         (Stog_types.string_of_human_id elt.elt_human_id) elt map)
        Stog_types.Str_map.empty elts
    in
    fun hid -> Stog_types.Str_map.find hid map
  in
  let f (cached, kept_deps) elt =
    let hid = Stog_types.string_of_human_id elt.elt_human_id in
    let same_elt_env =
      try
        let d = Smap.find hid !elt_envs in
        d = digest
      with Not_found -> false
    in
    let use_cache =
      if same_elt_env then
        begin
          let src_cache_file = cache_file stog_cache_name stog elt in
          let src_cache_time = Stog_misc.file_mtime src_cache_file in
          let deps_time = Stog_deps.max_deps_date stog elt_by_hid
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
        apply_loaders stog elt;
        (* keep deps of this element, as it did not changed *)
        let kept_deps =
          try Smap.add hid (Smap.find hid !Stog_deps.deps) kept_deps
          with Not_found -> kept_deps
        in
        (elt :: cached, kept_deps)
      end
    else
      begin
        (* do not keep deps of this element, as it will be recomputed *)
        (cached, kept_deps)
      end
  in
  let (cached, kept_deps) = List.fold_left f ([], Smap.empty) elts in
  Stog_deps.deps := kept_deps;
  cached
;;

let cache_elt stog elt =
  let cache_file = cache_file stog_cache_name stog elt in
  Stog_misc.safe_mkdir (Filename.dirname cache_file);
  let oc = open_out_bin cache_file in
  output_value oc elt ;
  close_out oc ;
  apply_storers stog elt
;;

let output_cache_info stog =
  let info_file = cache_info_file stog in
  let v = (!elt_envs, !Stog_deps.deps) in
  let oc = open_out_bin info_file in
  output_value oc v;
  close_out oc
;;

