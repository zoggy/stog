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

(** Computing information from stog structure (graphs, ...). *)

(** Compute internals graphs between documents. *)
val compute : Stog_types.stog -> Stog_types.stog

(** Get the next document by date. *)
val succ_by_date :
  Stog_types.stog -> Stog_types.Graph.key -> Stog_types.Graph.key option

(** Get the previous document by date. *)
val pred_by_date :
  Stog_types.stog -> Stog_types.Graph.key -> Stog_types.Graph.key option

(** Get graphviz code from the given stog structure, to create
  a graph showing edges between (clickable) documents; the edges are
  annotated by the topic and keywords. *)
val dot_of_graph : (Stog_types.doc -> Neturl.url) -> Stog_types.stog -> string

(** Remove from the stog structure the documents having
  {!Stog_types.doc.doc_published}[=false]. *)
val remove_not_published : Stog_types.stog -> Stog_types.stog
