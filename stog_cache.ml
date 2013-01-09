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
  Filename.concat cache_dir elt.elt_src
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
  let f elt_id elt (cached, not_cached, kept_deps) =
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
          let deps_time = Stog_deps.max_deps_date stog
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
        (elt_id :: cached, not_cached, kept_deps)
      end
    else
      begin
        (* do not keep deps of this element, as it will be recomputed *)
        (cached, elt_id :: not_cached, kept_deps)
      end
  in
  let (cached, not_cached, kept_deps) = Stog_tmap.fold f stog.stog_elts ([], [], Smap.empty) in
  Stog_deps.deps := kept_deps;
  (cached, not_cached)
;;

let output_cache_info stog =
  let info_file = cache_info_file stog in
  let v = (!elt_envs, !Stog_deps.deps) in
  let oc = open_out_bin info_file in
  output_value oc v;
  close_out oc
;;

