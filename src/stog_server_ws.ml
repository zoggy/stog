(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2015 INRIA All rights reserved.                         *)
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

(** *)

open Stog_types
open Stog_url
open Stog_server_types

module J = Yojson.Safe

let (>>=) = Lwt.bind

let rec wait_forever () = Lwt_unix.sleep 1000.0 >>= wait_forever

let push_message active_cons msg (stream, push) =
  Lwt.catch
    (fun () ->
       push msg >>= fun () ->
       prerr_endline "msg pushed";
       active_cons := (stream, push) :: !active_cons ;
       Lwt.return_unit
    )
    (fun e -> prerr_endline ("push_message: "^(Printexc.to_string e)) ; Lwt.return_unit)
;;

let push_message active_cons ?push (msg : Stog_server_types.server_message) =
  (*prerr_endline ("Number of active connections: "^(string_of_int (List.length !active_cons)));*)
  prerr_endline
    ("pushing message: "^
     (match msg with
        Stog_server_types.Update _ -> "Update"
      | Stog_server_types.Errors (errs,warns) ->
          String.concat "\n" ("Errors:" :: errs @ ["Warnings:"] @ warns)
     ));
  let marshalled = Marshal.to_string  msg [] in
  let hex = Stog_server_types.to_hex marshalled in
  let msg = Websocket_lwt.Frame.create ~content: hex () in
  match push with
    None ->
      let l = !active_cons in
      active_cons := [] ;
      Lwt_list.iter_s (push_message active_cons msg) l
  | Some push ->
        push msg

let send_errors active_cons ~errors ~warnings =
  let msg = Stog_server_types.Errors (errors, warnings) in
  push_message active_cons msg

let send_update_message active_cons path op =
  let msg = Stog_server_types.Update (path, op) in
  push_message active_cons msg

let handle_message read_stog current_state active_cons base_path stream push msg =
  match !current_state with
    None -> Lwt.fail (Failure "No state yet!")
  | Some state ->
      match msg with
      | `Refresh ->
          begin
            let send_doc doc =
              match doc.doc_out with
                None
              | Some [] -> Lwt.return_unit
              | Some (xml :: _) ->
                  let path = Stog_path.to_string doc.doc_path in
                  send_update_message active_cons path (Stog_server_types.Update_all xml)
            in
            try Stog_server_run.refresh read_stog current_state send_doc
              (fun errors -> send_errors active_cons ~errors ~warnings: [])
            with _ -> Lwt.return_unit
          end
      | `Get path ->
          try
            let (_, doc) = Stog_types.doc_by_path state.Stog_server_run.stog (Stog_path.of_string path) in
            match doc.Stog_types.doc_out with
              None
            | Some [] -> Lwt.return_unit
            | Some (tree :: _) ->
                let msg = Stog_server_types.Update (path, (Stog_server_types.Update_all tree)) in
                push_message active_cons ~push msg
          with e ->
              Lwt.return_unit

let handle_messages read_stog current_state active_cons base_path stream push =
  let f frame =
    match frame.Websocket_lwt.Frame.opcode with
    | Websocket_lwt.Frame.Opcode.Close ->
        prerr_endline (Printf.sprintf "A Close frame came when there were %d connections" (List.length !active_cons));
        active_cons := List.filter (fun (_,p) -> p != push) !active_cons ;
        prerr_endline (Printf.sprintf "Now I have only %d." (List.length !active_cons));
        Lwt.return_unit
    | _ ->
        let s = frame.Websocket_lwt.Frame.content in
        match Stog_server_types.client_msg_of_wsdata s with
          None -> Lwt.return_unit
        | Some (`Stog_msg msg) ->
            handle_message read_stog current_state active_cons 
              base_path stream push msg
  in
  Lwt.catch
    (fun _ -> Lwt_stream.iter_s f stream)
    (fun _ -> Lwt.return_unit)

let handle_con read_stog current_state active_cons base_path _id req recv push =
  prerr_endline "new connection";
  let stream = Websocket_lwt.mk_frame_stream recv in
  active_cons := (stream, push) :: !active_cons ;
  handle_messages read_stog current_state active_cons base_path stream push
;;

let run_server read_stog current_state active_cons ws_url base_path =
  let host = Stog_url.host ws_url.priv in
  let port = Stog_url.port ws_url.priv in
  prerr_endline ("Setting up websocket server on host="^host^", port="^(string_of_int port));
  (* set scheme to http to be resolved correctly *)
  let uri =
    let u = Uri.of_string (Stog_url.to_string ws_url.priv) in
    Uri.with_scheme u (Some "http")
  in
  Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system >>= fun endp ->
  let ctx = Conduit_lwt_unix.default_ctx in
  Conduit_lwt_unix.endp_to_server ~ctx endp >>= fun server ->
    Websocket_lwt.establish_standard_server ~ctx ~mode: server
    (handle_con read_stog current_state active_cons base_path)
;;

let diff_cut =
  let set = List.fold_left
    (fun set tag -> Stog_types.Str_set.add tag set)
    Stog_types.Str_set.empty
    [ "pre" ; "ul" ; "p" ; "svg"]
  in
  fun (_,tag) _ _ -> Stog_types.Str_set.mem (String.lowercase tag) set

let send_patch active_cons old_stog stog doc_id =
  let old_doc = Stog_types.doc old_stog doc_id in
  let new_doc = Stog_types.doc stog doc_id in
  let path = Stog_path.to_string new_doc.doc_path in

  prerr_endline ("sending patch (if needed) for path="^path) ;

  match old_doc.doc_out, new_doc.doc_out
    (*Some [Xtmpl.xml_of_string ~add_main: false "<html><body>coucou</body></html>"] *)
  with
  | None, None -> Lwt.return_unit
  | None, Some (t :: _) ->
      (*let s = Marshal.to_string t [] in*)
      let op = Stog_server_types.Update_all t in
      send_update_message active_cons path op
  | Some _, None ->
      let op = Stog_server_types.Patch [] in
      send_update_message active_cons path op
  | Some xtmpl1, Some xtmpl2 when xtmpl1 = xtmpl2 ->
      Lwt.return_unit
  | Some xtmpl1, Some xtmpl2 -> (* xml1 <> xml2 *)
      let xml1 = Xmldiff.xml_of_string (Xtmpl.string_of_xmls xtmpl1) in
      let xml2 = Xmldiff.xml_of_string (Xtmpl.string_of_xmls xtmpl2) in
      Lwt_preemptive.detach
       (fun xml2 ->
           try `Patch (Xmldiff.diff ~cut: diff_cut xml1 xml2)
           with e -> `Error (Printexc.to_string e)
        ) xml2 >>=
        (function
         | `Error msg ->
             send_errors active_cons ~errors: [msg] ~warnings: []
         | `Patch patch ->
             prerr_endline "patch computed";
             let op = Stog_server_types.Patch patch in
             send_update_message active_cons path op;
             (*let s = Marshal.to_string (List.hd xtmpl2) [] in
                Ocsigen_messages.warning (Printf.sprintf "sending marshalled operation, size=%d" (String.length s));
                send_update_message path
                (Stog_server_common.Update_all s);
                *)
          )
    | _, Some [] ->
        Lwt.return_unit
;;
