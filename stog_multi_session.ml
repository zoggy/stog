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

(** *)

open Stog_multi_config

type session =
  { session_id : string ;
    session_create_date : float ;
    session_dir : string ;
    session_stog_dir : string ;
    session_state : Stog_server_run.state option ref ;
    session_ws_cons : (Websocket.Frame.t Lwt_stream.t * (Websocket.Frame.t option -> unit)) list ref ;
    session_auth : string ;
    session_orig_branch : string ;
    session_branch : string ;
    session_preview_url : Neturl.url ;
  }

let () = Random.self_init ()

let new_id () = Printf.sprintf "%04x-%04x-%04x-%04x"
  (Random.int 0x10000) (Random.int 0x10000) (Random.int 0x10000) (Random.int 0x10000)
let new_auth_cookie = new_id ()

let read_stog stog_dir =
  let stog = Stog_init.from_dirs [stog_dir] in
  { stog with Stog_types.stog_outdir = Filename.concat stog_dir "stog-output" }

(*c==v=[Misc.try_finalize]=1.0====*)
let try_finalize f x finally y =
  let res =
    try f x
    with exn -> finally y; raise exn
  in
  finally y;
  res
(*/c==v=[Misc.try_finalize]=1.0====*)

let run command =
  match Sys.command command with
    0 -> ()
  | n -> failwith (Printf.sprintf "Command failed [%d]: %s" n command)

let ssh_git cfg ?dir command =
  let sub = Filename.quote
    (Printf.sprintf "ssh-add %s; git %s" (Filename.quote cfg.ssh_priv_key) command)
  in
  let command = Printf.sprintf "%s ssh-agent bash -c %s"
    (match dir with
       None -> ""
     | Some d -> Printf.sprintf "cd %s &&" (Filename.quote d)
    )
      sub
  in
  run command

let git_clone cfg session_dir =
  let command = Printf.sprintf "clone %s %s"
    (Filename.quote cfg.git_repo_url) (Filename.quote session_dir)
  in
  ssh_git cfg command

let add_user_info session account =
  let s = Printf.sprintf "\n[user]\n\tname = %s\n\temail = %s\n"
    account.name account.email
  in
  let command = Printf.sprintf
    "echo %s >> %s" (Filename.quote s)
      (Filename.quote
       (Filename.concat
        (Filename.concat session.session_dir ".git") "config"))
  in
  run command

let horodate () =
  Unix.(
   let t = localtime (time()) in
   Printf.sprintf "%04d-%02d-%02d-%02dh%02dm%02ds"
     (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday
     t.tm_hour t.tm_min t.tm_sec
  )
let git_current_branch session_dir =
  let command = Printf.sprintf
    "cd %s && git rev-parse --abbrev-ref HEAD" (Filename.quote session_dir)
  in
  try
    let stdin = Unix.open_process_in command in
    let branch = input_line stdin in
    ignore(Unix.close_process_in stdin);
    branch
  with
    _ -> failwith (Printf.sprintf "Exec error: %s" command)

let git_create_branch session_dir account =
  let branch_name = Printf.sprintf "%s--%s" account.login (horodate()) in
  let command = Printf.sprintf "cd %s && git checkout -b %s"
    (Filename.quote session_dir) (Filename.quote branch_name)
  in
  run command;
  branch_name

let create cfg account =
  let session_id = new_id () in
  let session_dir = Filename.concat cfg.dir session_id in
  let session_stog_dir =
    match cfg.stog_dir with
    | None -> session_dir
    | Some s -> Filename.concat session_dir s
  in
  git_clone cfg session_dir ;
  let stog = read_stog session_stog_dir in
  let stog_base_url =
    let url =
      List.fold_left Stog_types.url_concat
        cfg.app_url (Stog_multi_page.path_sessions @ [session_id ; "preview" ])
    in
    Neturl.modify_url ~path: ((Neturl.url_path url)@[""]) url
  in
  let orig_branch = git_current_branch session_dir in
  let branch_name = git_create_branch session_dir account in
  let (state, cons) = Stog_server_preview.new_stog_session stog stog_base_url in
  let session =
    { session_id ;
      session_create_date = Unix.time () ;
      session_dir ;
      session_stog_dir ;
      session_state = state ;
      session_ws_cons = cons ;
      session_auth = new_auth_cookie ;
      session_orig_branch = orig_branch ;
      session_branch = branch_name ;
      session_preview_url = stog_base_url ;
    }
  in
  add_user_info session account ;

  session
