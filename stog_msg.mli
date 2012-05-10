(** Printing warnings and errors. *)

val warning : ?info:string -> string -> unit
val set_print_warning : (string -> unit) -> unit
val warnings : unit -> int

val error : ?info:string -> string -> unit
val set_print_error : (string -> unit) -> unit
val errors : unit -> int
