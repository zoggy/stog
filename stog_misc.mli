(** *)

(*i==v=[Misc.safe_main]=1.0====*)
(** [safe_main f] calls [f ()] but handles [Sys_error] and [Failure]
   exceptions by exiting with error code 1.
@author Maxence Guesdon
@version 1.0
@cgname Misc.safe_main*)
val safe_main : (unit -> unit) -> unit
(*/i==v=[Misc.safe_main]=1.0====*)


(*i==v=[File.string_of_file]=1.0====*)
(** [string_of_file filename] returns the content of [filename]
   in the form of one string.
@author Maxence Guesdon
@version 1.0
@raise Sys_error if the file could not be opened.
@cgname File.string_of_file*)
val string_of_file : string -> string
(*/i==v=[File.string_of_file]=1.0====*)


(*i==v=[File.file_of_string]=1.1====*)
(** [file_of_string ~file str] creates a file named
   [filename] whose content is [str].
@author Fabrice Lefessant
@version 1.1
@raise Sys_error if the file could not be opened.
@cgname File.file_of_string*)
val file_of_string : file:string -> string -> unit
(*/i==v=[File.file_of_string]=1.1====*)


(*i==v=[String.split_string]=1.1====*)
(** Separate the given string according to the given list of characters.
@author Maxence Guesdon
@version 1.1
@param keep_empty is [false] by default. If set to [true],
   the empty strings between separators are kept.
@cgname String.split_string*)
val split_string : ?keep_empty:bool -> string -> char list -> string list
(*/i==v=[String.split_string]=1.1====*)


(*i==v=[String.strip_string]=1.0====*)
(** [strip_string s] removes all leading and trailing spaces from the given string.
@author Maxence Guesdon
@version 1.0
@cgname String.strip_string*)
val strip_string : string -> string
(*/i==v=[String.strip_string]=1.0====*)

