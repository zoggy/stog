
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

