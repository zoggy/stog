(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 INRIA All rights reserved.                         *)
(*    Author: Maxence Guesdon, INRIA Saclay                                      *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public                  *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    As a special exception, you have permission to link this program           *)
(*    with the OCaml compiler and distribute executables, as long as you         *)
(*    follow the requirements of the GNU GPL in regard to all of the             *)
(*    software in the executable aside from the OCaml compiler.                  *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** Utilities. *)

(*i==v=[Misc.safe_main]=1.0====*)
(** [safe_main f] calls [f ()] but handles [Sys_error] and [Failure]
   exceptions by exiting with error code 1.
@author Maxence Guesdon
@version 1.0
@cgname Misc.safe_main*)
val safe_main : (unit -> unit) -> unit
(*/i==v=[Misc.safe_main]=1.0====*)


(*i==v=[File.string_of_file]=1.1====*)
(** [string_of_file filename] returns the content of [filename]
   in the form of one string.
@author Maxence Guesdon
@version 1.1
@raise Sys_error if the file could not be opened.
@cgname File.string_of_file*)
val string_of_file : string -> string
(*/i==v=[File.string_of_file]=1.1====*)


(*i==v=[File.file_of_string]=1.1====*)
(** [file_of_string ~file str] creates a file named
   [filename] whose content is [str].
@author Fabrice Lefessant
@version 1.1
@raise Sys_error if the file could not be opened.
@cgname File.file_of_string*)
val file_of_string : file:string -> string -> unit
(*/i==v=[File.file_of_string]=1.1====*)

(** Creating a log function.
  [create_log_fun env_var] get the log level (an integer) from the given
  environment variable, and returns a function to print messages.
  This function takes a level (default is 1) and a function returning
  the message do print. The function is called only if the log level is
  higher than or equal to the given level.
  The [loc] parameter of the returned function can be used to indicate
  an additional string to print before the log message.
  If the environment variable is empty or does not contain an integer,
  then the log level is set to 0.
  @param prefix can be used to indicate a string prefixing every message
  @param print can be given to the function build the log function, to
  indicate an alternative way to display the message; default is to call
  [prerr_endline].
  *)
val create_log_fun :
  ?prefix: string ->
  ?print:(string -> unit) -> string ->
  (?loc: string -> ?level:int -> (unit -> string) -> unit)

(** Same as [create_log_fun] but also return a function to change
       the log level.*)
val create_log_fun_with_set :
  ?prefix: string ->
  ?print:(string -> unit) -> string ->
  (?loc: string -> ?level:int -> (unit -> string) -> unit) *
  (int -> unit)


(*i==v=[String.split_string]=1.2====*)
(** Separate the given string according to the given list of characters.
@author Maxence Guesdon
@version 1.2
@param keep_empty is [false] by default. If set to [true],
   the empty strings between separators are kept.
@cgname String.split_string*)
val split_string : ?keep_empty:bool -> string -> char list -> string list
(*/i==v=[String.split_string]=1.2====*)


(*i==v=[String.strip_string]=1.0====*)
(** [strip_string s] removes all leading and trailing spaces from the given string.
@author Maxence Guesdon
@version 1.0
@cgname String.strip_string*)
val strip_string : string -> string
(*/i==v=[String.strip_string]=1.0====*)

(** [strip_blank_lines s] works as {!strip_string}, but only strips
    full blank lines, without touching spaces or tabulations. *)
val strip_blank_lines : string -> string

(*i==v=[String.lowercase]=1.0====*)
(** [lowercase s] lowers the case of the given string, including accentuated characters.
@author Maxence Guesdon
@version 1.0
@cgname String.lowercase*)
val lowercase : string -> string
(*/i==v=[String.lowercase]=1.0====*)

(*i==v=[List.list_chop]=1.0====*)
(** [list_chop n l] returns the [n] first documents of list [l] or the whole
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

val map_opt : ('a -> 'b) -> 'a option -> 'b option

val list_concat : ?sep: 'a -> 'a list -> 'a list

val dot_to_svg : string -> string

val list_compare : ('a -> 'a -> int) -> 'a list -> 'a list -> int

(** [filename_extension filename] returns extension of [filename]
  or [""] if there is no extension. *)
val filename_extension : string -> string

val safe_mkdir : string -> unit

(*i==v=[String.opt_of_string]=1.0====*)
(** [opt_of_string s] returns [None] if the string if empty
   (length is 0) or [Some s].
@version 1.0
@cgname String.opt_of_string*)
val opt_of_string : string -> string option
(*/i==v=[String.opt_of_string]=1.0====*)

(*i==v=[String.string_of_opt]=1.0====*)
(** [string_of_opt s_opt] returns the empty string if
   [s_opt = None] or [s] if [s_opt = Some s].
@version 1.0
@cgname String.string_of_opt*)
val string_of_opt : string option -> string
(*/i==v=[String.string_of_opt]=1.0====*)

(** Return mdification time of the given file, or None if
  the file does not exist. *)
val file_mtime : string -> float option

(** [path_under ~parent file] returns the path to [file] from [parent].
     @raise Failure if [parent] is not a prefix of [file].*)
val path_under : parent: string -> string -> string

val string_of_time : float -> string

