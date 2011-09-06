
(*i==m=[File.Find]=0.1=t==*)
(** Finding files.
@author Didier Rémy
@version 0.1
@cgname File.Find*)

      type filter =
	  Maxdepth of int
	| Type of Unix.file_kind
	| Follow
	| Regexp of Str.regexp
	| Atime of interval
	| Predicate of (string -> bool)
      and interval =
	  Le of int | Eq of int | Ge of int
      type mode =
	| Ignore
	| Stderr
	| Failure
	| Custom of (Unix.error * string * string -> unit)

      val find : mode -> string list -> filter list ->
        (string -> unit) -> unit

      val find_list : mode -> string list -> filter list -> string list
    
(*/i==m=[File.Find]=0.1=t==*)

