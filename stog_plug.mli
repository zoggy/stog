(** Interface for plugins. *)

val register_lang : Stog_intl.lang_abbrev -> Stog_intl.lang_data -> unit
val register_fun : string -> Xtmpl.callback -> unit
val stog : unit -> Stog_types.stog
