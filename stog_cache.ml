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

let set_elt_env hid env =
  let s = Marshal.to_string env [Marshal.Closures] in
  elt_envs := Smap.add hid s !elt_envs
;;


let get_cached_elements stog =
  let info_file = cache_info_file stog in
  let info_time = Stog_misc.file_mtime info_file in
  let info_time =
    match info_time with
      None -> Unix.time ()
    | Some date ->
        let ic = open_in_bin info_file in
        let (v : (string Smap.t * Stog_deps.Depset.t Smap.t)) = input_value ic in
        close_in ic;
        elt_envs := fst v ;
        Stog_deps.deps := snd v;
        date
  in
  let f elt_id elt (cached, not_cached) =
    let src_file = Filename.concat stog.stog_dir elt.elt_src in
    let src_time = Stog_misc.file_mtime src_file in
    let cache_time = Stog_deps.max_deps_date stog
      (Stog_types.string_of_human_id elt.elt_human_id)
    in
    let cache_time = max info_time cache_time in
     match src_time with
      None -> (cached, elt_id::not_cached)
    | Some t_elt ->
        if cache_time > t_elt then
          begin
            apply_loaders stog elt;
            (elt_id :: cached, not_cached)
          end
        else
          (cached, elt_id :: not_cached)
  in
  Stog_tmap.fold f stog.stog_elts ([], [])
;;


