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

(** Tries.

   These tries store data associated to paths. A path is a list of symbols.
   The same path cannot be added twice.
*)

(** Signature of the module created by the {!Make} functor. *)
module type S =
  sig
    type symbol
    type path = symbol list
    type 'a t
    exception Already_present of path
    val empty : 'a t

    (** @raise Already_present if the added path already exist in the trie
      and [fail] is [true] (it is [false] by default. *)
    val add : ?fail: bool -> path -> 'a -> 'a t -> 'a t

    (** Find all data associated to the given path or below, or an empty list
         if such a path does not exist. *)
    val find : path -> 'a t -> 'a list

    val to_string : (symbol -> string) -> 'a t -> string
  end
module Make : functor (P : Map.OrderedType) -> S with type symbol = P.t
