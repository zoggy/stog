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

(** Computing stog documents in preview server *)

module Xdiff = Xmldiff

type state = {
  stog : Stog_types.stog;
  stog_modules : (module Stog_engine.Module) list;
  stog_errors : string list;
  stog_warnings : string list;
  doc_dates : float Stog_path.Map.t;
  busy : bool;
}

val run_stog : ?docs:Stog_types.Doc_set.t -> state -> state Lwt.t

(** Start a Lwt thread watching for changes on stog files,
  relaunching computations of documents and sending patches on document
  update.* using [on_update]. [on_error] is called with warnings and
  errors, if any. *)
val watch :
  Stog_types.stog ->
  state option ref ->
  on_update:(Stog_types.stog ->
             Stog_types.stog -> Stog_types.Doc_set.elt -> unit Lwt.t) ->
  on_error:(errors:string list -> warnings:string list -> unit Lwt.t) ->
  'a Lwt.t

(** [refresh load state send_doc on_error] reload stog structure from filesystem
  using [load], sends all documents to clients using [send_doc]. In case of
  error, [on_error] is called with the list of errors. *)
val refresh :
  (unit -> Stog_types.stog) ->
  state option ref ->
  (Stog_types.doc -> unit Lwt.t) -> (string list -> unit Lwt.t) -> unit Lwt.t
