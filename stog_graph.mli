(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2013 Maxence Guesdon. All rights reserved.              *)
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

(** Annotated sparse graphs. *)

module type GMap =
  sig
    type key
    type 'a t
    val create : unit -> 'a t
    val get : 'a t -> key -> 'a
    val set : 'a t -> key -> 'a -> 'a t
    val remove : 'a t -> key -> 'a t
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val iter : (key -> 'a -> unit) -> 'a t -> unit
  end

(** This is the output signature of the functor creating a graph module. *)
module type S =
  sig
    (** The type of vertices *)
    type key

    (** The type of edge annotations *)
    type edge_data

    (** A graph *)
    type t

    (** Creating an empty graph. *)
    val create : unit -> t

    (** Marshal the given graph. *)
    val marshal : t -> string

    (** Unmarshal. *)
    val unmarshal : string -> t

    (** [succ g a] returns the successors of a vertice as a list of pairs
         [(successor, edge annotation)]. A vertice [b] can appear more than once in the
         list if there are edges with different annotations between [a] and [b].
         If the vertice does not exist in the graph, the empty list is returned.*)
    val succ : t -> key -> (key * edge_data) list

    (** Same as {!succ} but returns the predecessors. *)
    val pred : t -> key -> (key * edge_data) list

    (** [add g (a, b, data)] adds to the graph [g] an edge from [a] to [b]
         annotated with [data]. The edge data comparison function is used
         to know whether the same edge with the same annotation already exists. If so,
         no new edge is added. *)
    val add : t -> key * key * edge_data -> t

    (** [rem g (a, b) pred] removes from graph [g] the edges from [a] to [b]
         whose annotations satisfy the given predicate [pred]. *)
    val rem : t -> key * key -> (edge_data -> bool) -> t

    (** [rem_all g (a, b)] removes from graph [g] all edges from [a] to [b]. *)
    val rem_all : t -> key * key -> t

    (** [isolate g a] removes all edges from and to vertice [a]. *)
    val isolate : t -> key -> t

    (** [remove_node g a] removes the vertice [a] from the graph [g]. *)
    val remove_node : t -> key -> t

    (** [pred_roots g] returns the list of vertices having no predecessor in the graph. *)
    val pred_roots : ?ignore_deps: edge_data list -> t -> key list

    (** Same as {!pred_roots} but for vertices with no successor. *)
    val succ_roots : t -> key list

    (** [recursive_succs t key] returns the list of all nodes "under" the given
        one; the given predicate can be used to follow only some edges. *)
    val recursive_succs : t -> ?pred: (edge_data -> bool) -> key -> key list

    (** Same as {!recursive_succs} but for predecessors. *)
    val recursive_preds : t -> ?pred: (edge_data -> bool) -> key -> key list

    (** [reverse g] return a graph where all edges of [g] are reversed, i.e. each
         edge from [a] to [b] is replaced by an edge from [b] to [a], keeping the
         associated edge annotations. *)
    val reverse : t -> t

    val fold_succ : t -> (key -> (key * edge_data) list -> 'a -> 'a) -> 'a -> 'a
    val fold_pred : t -> (key -> (key * edge_data) list -> 'a -> 'a) -> 'a -> 'a

    (** [iter_succ g f] calls f on each vertice and its successors as returned
         by {!succ}. *)
    val iter_succ : t -> (key -> (key * edge_data) list -> unit) -> unit

    (** Same as {!iter_succ} but with predecessors of each vertice. *)
    val iter_pred : t -> (key -> (key * edge_data) list -> unit) -> unit

    (** [dot_of_graph ~f_node g] returns the graphviz code to represent the
         given graph.
         @param f_node is used to, from a vertice, return a unique id, a label
         and attribute of a vertice.
         @param f_edge can be specified to indicate a label and attribute from
         an edge annotation.
    *)
    val dot_of_graph :
      ?f_edge:(edge_data -> string * (string * string) list) ->
      f_node:(key -> string * string * (string * string) list) ->
      t -> string

    (** [nodes_by_pred_order g] returns a sorted list of vertices. Vertices with
           no predecessor are first in the list. If an edge exists from [a] to [b],
           then [a] will be before [b] in the list. Vertices beloning to a cycle
           will not appear in the list. *)
    val nodes_by_pred_order : t -> key list

    (** [shortest_path g cost (a, b)] computes the shortest path from [a] to [b]
         according to the [cost] function. This function must return a
         stricly positive value and the edge edge annotation used to get
         this value (it may be possible to have different costs to go from
         [x] to [y] if there are various edges from [x] to [y]). The [cost g x y]
         function must return [None] if there is no edge from [x] to [y].

         The algorithm used is described here:
           {{:http://tide4javascript.com/?s=Dijkstra}Djikstra}.
     *)
    val shortest_path :
      t ->
      (t -> key * key -> (float * edge_data) option) ->
      key * key -> (key * edge_data * key) list
end
;;

(** Making a graph module.
   @param M is the module defining a map with a vertice as key.
   @param Edge is the module defining the type of the edge annotations.
     The [compare] function is used when adding an edge, to prevent adding
     an edge from [x] to [y] when an edge with the same annotation already
     exists from [x] to [y].
   *)
module Make :
  functor (M : GMap) ->
    functor (Edge : Map.OrderedType) ->
      S with type key = M.key and type edge_data = Edge.t
;;

(** This is a convenient functor using the [Map] module of the standard library
  to build the {!GMap} module required by {!Make} from the [P] parameter. *)
module Make_with_map :
  functor (P : Map.OrderedType) ->
    functor (Edge : Map.OrderedType) ->
      S with type key = P.t and type edge_data = Edge.t
;;
