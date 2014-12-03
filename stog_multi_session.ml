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
  }

let () = Random.self_init ()

let new_id () = Printf.sprintf "%04x-%04x-%04x-%04x"
  (Random.int 0x10000) (Random.int 0x10000) (Random.int 0x10000) (Random.int 0x10000)
let new_auth_cookie = new_id ()

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

let ssh_git cfg session command =
  let key_file = Filename.concat session.session_dir "ssh.key" in
  Stog_misc.file_of_string ~file: key_file cfg.ssh_priv_key;
  let sub = Filename.quote
    (Printf.sprintf "ssh-add %s; git %s" (Filename.quote key_file) command)
  in
  let command = Printf.sprintf "ssh-agent bash -c %s" (Filename.quote sub) in
  try_finalize run command Sys.remove key_file

let git_clone cfg session =
  let command = Printf.sprintf "clone %s %s"
    (Filename.quote cfg.git_repo_url) (Filename.quote session.session_dir)
  in
  ssh_git cfg session command

let create cfg account =
  let session_id = new_id () in
  let session_dir = Filename.concat cfg.dir session_id in
  let session_stog_dir =
    match cfg.stog_dir with
    | None -> session_dir
    | Some s -> Filename.concat session_dir s
  in
  let session = 
    { session_id ;
      session_create_date = Unix.time () ;
      session_dir ;
      session_stog_dir ;
      session_state = ref None ;
      session_ws_cons = ref [] ;
      session_auth = new_auth_cookie ;
    }
  in
  git_clone cfg session ;
  session
