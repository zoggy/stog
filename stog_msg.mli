(** Printing warnings and errors. *)

val verbose_level : unit -> int
val set_verbose_level : int -> unit
val incr_verbose_level : unit -> unit
val verbose : ?info:string -> ?level: int -> string -> unit
val set_print_verbose : (string -> unit) -> unit

val warning : ?info:string -> string -> unit
val set_print_warning : (string -> unit) -> unit
val warnings : unit -> int

val error : ?info:string -> ?fatal: int -> string -> unit
val set_print_error : (string -> unit) -> unit
val errors : unit -> int
