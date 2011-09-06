
(*c==m=[TMap]=0.1=t==*)
(** *)

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

module type Map_par = sig
  type (+'a) t
  type key = int
  val empty : 'a t
  val is_empty : 'a t -> bool
  val add : int -> 'a -> 'a t -> 'a t
  val find : int -> 'a t -> 'a
  val remove : int -> 'a t -> 'a t
  val mem :  int -> 'a t -> bool
  val iter : (int -> 'a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
  val fold : (int -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end

module Functional = functor (Map : Map_par with type key = int) ->
    struct
      exception Found of int;;
      type 'a key = Map.key
        type ('key , 'a) t = {
          counter : int ref;
          (** Having a reference instead of a function will allow us
             to dump structures using marshalling. Using a ref instead
             of an int will make unique ids. This will result in an id
             being usable only in a graph or its "descendents". In case
             we use a new id on an "old" graph of a graph being not
             a descendent of the graph the id was created for, there will
             be a Not_found error instead of a confusion in the elements
             manipulated. This will be easier to debug. *)
          map : 'a Map.t ;
        }

    let compare_key : int -> int -> int = Pervasives.compare;;

    let create _ =
      { counter = ref 0;
        map = Map.empty ;
      };;

    let get t k = Map.find k t.map;;

    let add t v =
      incr t.counter ;
    let id = !(t.counter) in
    assert (id <> 0) ; (* fail if we used too many ids *)
    let t = { t with map = Map.add id v t.map } in
    (id, t)

    let fold f t = Map.fold f t.map;;

    let find t pred =
      try
        Map.iter (fun key e -> if pred e then raise (Found key)) t.map;
        raise Not_found
      with Found key -> key

    let int k = k;;
    let iter f t = Map.iter f t.map;;
    let remove t k = { t with map = Map.remove k t.map };;

    let modify t k v =
    let t = remove t k in
    { t with map = Map.add k v t.map }

    let card t = Map.fold (fun _ _ n -> n + 1) t.map 0;;
  end
;;

module Map =
  Functional (Map.Make (struct type t = int let compare = (Pervasives.compare : int -> int -> int) end));;


include Map


(*/c==m=[TMap]=0.1=t==*)

