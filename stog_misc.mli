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


(*i==v=[String.lowercase]=1.0====*)
(** [lowercase s] lowers the case of the given string, including accentuated characters.
@author Maxence Guesdon
@version 1.0
@cgname String.lowercase*)
val lowercase : string -> string
(*/i==v=[String.lowercase]=1.0====*)


(*i==v=[List.list_chop]=1.0====*)
(** [list_chop n l] returns the [n] first elements of list [l] or the whole
   list if [n >= List.length l].
@author Maxence Guesdon
@version 1.0
@cgname List.list_chop*)
val list_chop : int -> 'h list -> 'h list
(*/i==v=[List.list_chop]=1.0====*)

val mkdir : string -> unit

(*i==v=[String.is_prefix]=1.0====*)
(** [is_prefix pattern s] returns true if string [s] begins with [pattern].
@author Maxence Guesdon
@version 1.0
@cgname String.is_prefix*)
val is_prefix : string -> string -> bool
(*/i==v=[String.is_prefix]=1.0====*)

(*i==v=[List.list_remove_doubles]=1.0====*)
(** [list_remove_doubles ?pred l] remove doubles in the given list [l], according
   to the optional equality function [pred]. Default equality function is [(=)].
@author Maxence Guesdon
@version 1.0
@cgname List.list_remove_doubles*)
val list_remove_doubles : ?pred:('k -> 'k -> bool) -> 'k list -> 'k list
(*/i==v=[List.list_remove_doubles]=1.0====*)

val md5 : string -> string

val count_char : string -> char -> int

val encode_string : string -> string
