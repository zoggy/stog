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

let set_elt_env elt env =
  let hid = Stog_types.string_of_human_id elt.elt_human_id in
  let s = Marshal.to_string env [Marshal.Closures] in
  elt_envs := Smap.add hid s !elt_envs
;;


let get_cached_elements stog env =
  let info_file = cache_info_file stog in
  let info_time = Stog_misc.file_mtime info_file in
  let info_time =
    match info_time with
      None -> 0.0
    | Some date ->
        let ic = open_in_bin info_file in
        let (v : (string Smap.t * Stog_deps.Depset.t Smap.t)) = input_value ic in
        close_in ic;
        elt_envs := fst v ;
        Stog_deps.deps := snd v;
        date
  in
  let marsh_env = Marshal.to_string env [Marshal.Closures] in
  let f elt_id elt (cached, not_cached) =
    let hid = Stog_types.string_of_human_id elt.elt_human_id in
    let same_elt_env =
      try
        let e = Smap.find hid !elt_envs in
        e = marsh_env
      with Not_found -> false
    in
    let use_cache =
      if same_elt_env then
        begin
          let src_file = Filename.concat stog.stog_dir elt.elt_src in
          let src_time = Stog_misc.file_mtime src_file in
          let cache_time = Stog_deps.max_deps_date stog
            (Stog_types.string_of_human_id elt.elt_human_id)
          in
          let cache_time = max info_time cache_time in
          match src_time with
            None -> false
          | Some t_elt -> cache_time > t_elt
        end
      else
         false
    in
    if use_cache then
      begin
        apply_loaders stog elt;
        (elt_id :: cached, not_cached)
      end
    else
      begin
        (* remove known deps of this element, as it will be recomputed *)
        Stog_deps.deps := Smap.remove hid !Stog_deps.deps;
        (cached, elt_id :: not_cached)
      end
  in
  Stog_tmap.fold f stog.stog_elts ([], [])
;;

let output_cache_info stog =
  let info_file = cache_info_file stog in
  let v = (!elt_envs, !Stog_deps.deps) in
  let oc = open_out_bin info_file in
  output_value oc v;
  close_out oc
;;

