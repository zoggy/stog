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

(** *)

open Stog_types;;
open Lwt;;
module Xdiff = Xmldiff;;

let sleep_duration = 2.0 ;;

type state = {
  stog : stog ;
  stog_modules : (module Stog_engine.Module) list ;
  stog_errors : string list ;
  stog_warnings : string list ;
  doc_dates : float Stog_path.Map.t ;
}

let current_state = ref None

let run_stog state = Lwt.return state

let rec watch_for_change on_update =
  Lwt_unix.sleep sleep_duration >>= fun () ->
    match !current_state with
      None -> watch_for_change on_update
    | Some state ->
        run_stog state >>= fun state ->
          current_state := Some state ;
          watch_for_change on_update

let watch base_url dir on_update =
  let stog = Stog_io.read_stog dir in
  Lwt.catch
     (fun () -> Lwt_unix.mkdir (Filename.concat dir "stog-output") 0o750)
     (fun _ -> Lwt.return_unit)
  >>= fun () ->
  let stog = { stog with
      Stog_types.stog_base_url = (Stog_types.url_of_string base_url) ;
      stog_outdir = "stog-output" ;
    }
  in
  let state = {
      stog ;
      stog_modules = [] ;
      stog_errors = [] ;
      stog_warnings = [] ;
      doc_dates = Stog_path.Map.empty
    }
  in
  let time = Unix.time () in
  run_stog state
  >>= fun state ->
  let docs = Stog_types.doc_list state.stog in
  let state =
    { state with
      doc_dates = List.fold_left
        (fun acc (_, doc) ->
           Stog_path.Map.add doc.doc_path time acc
        )
        state.doc_dates docs
    }
  in
  current_state := Some state ;
  ignore(watch_for_change on_update);
  Lwt.return state

let state () =
  match !current_state with
  | None -> Lwt.fail (Failure "No state yet!")
  | Some s -> Lwt.return s

