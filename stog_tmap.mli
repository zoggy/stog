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

(*i==m=[TMap]=0.1=t==*)
(** Storing data referenced by a unique id.
Uses maps to store the information,

   Map of elements with automatic id creation and phantom type
   for keys.
@author Maxence Guesdon
@version 0.1
@cgname TMap*)
(** Storing data referenced by a unique id.
Uses maps to store the information,

   Map of elements with automatic id creation and phantom type
   for keys.
   @cgname TMap
   @version 0.1
   @author Maxence Guesdon
*)
module type S =
  sig
    type 'a key
    type ('key, 'a) t

    val create : 'a -> ('key, 'a) t
    val compare_key : 'a key -> 'a key -> int
    val get : ('key, 'a) t -> 'key key -> 'a
    val add : ('key, 'a) t -> 'a -> 'key key * ('key, 'a) t
    val fold : ('key key -> 'a -> 'b -> 'b) -> ('key, 'a) t -> 'b -> 'b
    val find : ('key, 'a) t -> ('a -> bool) -> 'key key
    val int : 'key key -> int
    val iter : ('key key -> 'a -> unit) -> ('key, 'a) t -> unit
    val remove : ('key, 'a) t -> 'key key -> ('key, 'a) t
    val modify : ('key, 'a) t -> 'key key -> 'a -> ('key, 'a) t
    val card : ('key, 'a) t -> int
  end

include S


(*/i==m=[TMap]=0.1=t==*)

