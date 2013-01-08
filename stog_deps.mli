(** *)

type dependency = File of string | Elt of string
module Depset : Set.S with type elt = dependency
val deps : Depset.t Stog_types.Str_map.t ref
val add_dep : Stog_types.elt -> Depset.elt -> unit
val max_deps_date : Stog_types.stog -> string -> float
