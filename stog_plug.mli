(** Interface for plugins. *)

val register_lang : Stog_intl.lang_abbrev -> Stog_intl.lang_data -> unit
val register_fun : string -> Xtmpl.callback -> unit
val stog : unit -> Stog_types.stog

val verbose : ?info:string -> ?level: int -> string -> unit
val set_print_verbose : (string -> unit) -> unit

val warning : ?info:string -> string -> unit
val set_print_warning : (string -> unit) -> unit

val error : ?info:string -> ?fatal: int -> string -> unit
val set_print_error : (string -> unit) -> unit
