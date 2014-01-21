(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 Maxence Guesdon. All rights reserved.              *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU Library General Public License for more details.                       *)
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

(*i==m=[File.Find]=0.1=t==*)
(** Finding files.
@author Didier Remy
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

