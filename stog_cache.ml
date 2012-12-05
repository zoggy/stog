(** *)

open Stog_types;;

module type Cache = sig
  type t
  val name : string
  val load : Stog_types.elt -> t -> unit
  val store : Stog_types.elt -> t
end;;

let loaders = ref [];;
let storers = ref [];;

let cache_file name stog elt =
  let cache_dir = stog.stog_cache_dir ^
    (match name with "" -> "" | s -> "-"^s)
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
