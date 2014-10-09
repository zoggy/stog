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
let debug =
  match Sys.getenv "STOG_SERVER_DEBUG" with
    "1" -> fun s -> Lwt_io.write Lwt_io.stderr s
  | _ -> fun _ -> Lwt.return_unit
  | exception _ -> fun _ -> Lwt.return_unit

type state = {
  stog : stog ;
  stog_modules : (module Stog_engine.Module) list ;
  stog_errors : string list ;
  stog_warnings : string list ;
  doc_dates : float Stog_path.Map.t ;
}

let run_stog ?docs state =
  debug "Running stog\n" >>= fun _ ->
  let errors = ref [] in
  let warnings = ref [] in
  let stog = state.stog in
  Stog_msg.set_print_error (fun s -> errors := s :: !errors);
  Stog_msg.set_print_warning (fun s -> warnings := s :: !warnings);
  Lwt.catch
    (fun () ->
       let modules =
         match docs, state.stog_modules with
           Some _, ((_ :: _) as l) -> l
         | None, _
         | _, [] ->
             let modules = Stog_engine.modules () in
             List.map
               (fun (name, f) ->
                  Stog_msg.verbose ~level: 2 ("Initializing module "^name);
                  f stog
               )
               modules
       in
       let stog =
         match docs with
           None -> Stog_info.compute stog
         | Some _ -> stog
       in
       let st_docs =
         match docs with
           None -> Stog_types.Doc_set.empty
         | Some set -> set
       in
       let stog_state = {
           Stog_engine.st_stog = stog ;
           st_modules = modules ;
           st_docs = st_docs ;
         }
       in
       Lwt_preemptive.detach (Stog_engine.run ~use_cache: false) stog_state
         >>= fun stog_state ->
       let state = { state with
           stog = stog_state.Stog_engine.st_stog ;
           stog_modules = stog_state.Stog_engine.st_modules ;
           stog_errors = state.stog_errors @ (List.rev !errors) ;
           stog_warnings = state.stog_warnings @ (List.rev !warnings) ;
         }
       in
       Lwt.return state
    )
    (function
     | Stog_types.Path_trie.Already_present path ->
         Stog_msg.error
           ("Doc path already present: "^(String.concat "/" path)) ;
         Lwt.return state
     | e ->
         let state = { state with
               stog_errors = state.stog_errors @ (List.rev ((Printexc.to_string e) :: !errors)) ;
             stog_warnings = List.rev !warnings ;
           }
         in
         Lwt.return state
    )

let file_stat file =
  Lwt.catch
    (fun () -> Lwt_unix.stat file >>= fun st -> Lwt.return (Some st))
    (fun _ -> Lwt.return None)

let rec watch_for_change current_state on_update on_error =
  Lwt_unix.sleep sleep_duration >>= fun () ->
    debug "watch for changes... " >>= fun _ ->
    match !current_state with
      None -> watch_for_change current_state on_update on_error
    | Some state ->
        let old_stog = state.stog in
        let doc_list = Stog_types.doc_list state.stog in
        let read_errors = ref [] in
        let f (acc_dates, docs, stog) (doc_id, doc) =
          let file = Filename.concat stog.stog_dir doc.doc_src in
          file_stat file >>=
            function
            | None -> Lwt.return (acc_dates, docs, stog)
            | Some st ->
              let date = st.Unix.st_mtime in
              (*prerr_endline ("date for "^file);*)
              let prev_date =
                try Stog_path.Map.find doc.doc_path acc_dates
                with Not_found -> date -. 1.
              in
              if date <= prev_date then
                Lwt.return (acc_dates, docs, stog)
              else
                let doc =
                  match doc.doc_parent with
                    Some _ ->
                      (* doc coming from computation of another doc *)
                      { doc with doc_out = None }
                  | None ->
                      (** FIXME: Use a Lwt version of Stog_io.doc_of_file *)
                      try Stog_io.doc_of_file stog file
                      with
                        e ->
                          let msg =
                            match e with
                              Failure msg | Sys_error msg -> msg
                            | _ -> Printexc.to_string e
                          in
                          read_errors := msg :: !read_errors ;
                          doc
                in
                Lwt.return
                  (
                   Stog_path.Map.add doc.doc_path date acc_dates,
                   Stog_types.Doc_set.add doc_id docs,
                   Stog_types.set_doc stog doc_id doc
                  )
        in
        Lwt_list.fold_left_s f (state.doc_dates, Stog_types.Doc_set.empty, state.stog) doc_list
          >>=
          (fun (dates, docs, stog) ->
             let nb_changes = Stog_types.Doc_set.cardinal docs in
             debug (Printf.sprintf "%d elements modified\n" nb_changes)
               >>= fun () ->
                 match nb_changes with
                   0 -> Lwt.return_unit (* do not change current_state *)
                 | _ ->
                   let state = { state with
                       stog_errors = List.rev !read_errors ;
                       stog_warnings = [] ;
                       stog = stog ; doc_dates = dates ;
                     }
                   in
                   run_stog ~docs state >>=
                     fun state ->
                       Lwt_list.iter_s
                         (on_update old_stog state.stog)
                         (Stog_types.Doc_set.elements docs)
                         >>=
                         (fun () ->
                            current_state := Some state ;
                            match state.stog_errors, state.stog_warnings with
                              [], [] -> Lwt.return_unit
                            | errors, warnings -> on_error ~errors ~warnings
                         )
          ) >>= fun () -> watch_for_change current_state on_update on_error
;;

let watch stog current_state ~on_update ~on_error =
  Lwt.catch
     (fun () -> Lwt_unix.mkdir (Filename.concat (Sys.getcwd()) "stog-output") 0o750)
     (fun _ -> Lwt.return_unit)
  >>= fun () ->
  let stog = { stog with stog_outdir = "stog-output" } in
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
  prerr_endline "state set";
  watch_for_change current_state on_update on_error

