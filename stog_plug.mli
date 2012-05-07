(** Interface for plugins. *)

val register_fun : string -> Xtmpl.callback -> unit
val stog : unit -> Stog_types.stog
