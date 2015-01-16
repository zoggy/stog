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

type session_state =
  | Live
  | Stopped
  | Error of string [@@deriving yojson]

type stored =
  { session_create_date : float ;
    mutable session_state : session_state ;
    session_orig_branch : string ;
    session_branch : string ;
    session_author : string ;
  }  [@@deriving yojson]

type stog_info = {
    stog_dir : string ;
    mutable stog_state : Stog_server_run.state option ref ;
    mutable stog_ws_cons : (Websocket.Frame.t Lwt_stream.t * (Websocket.Frame.t option -> unit)) list ref ;
    stog_preview_url : Neturl.url ;
  }

type editor_info = {
    editor_ws_cons : (Websocket.Frame.t Lwt_stream.t * (Websocket.Frame.t option -> unit)) list ref ;
    editor_url : Neturl.url ;
  }
type session =
  { session_id : string ;
    session_dir : string ;
    session_repo_dir : string ;
    session_stored : stored ;
    session_stog : stog_info ;
    session_editor : editor_info ;
  }

let session_info_file session_dir = Filename.concat session_dir "index.json"
let repo_dir_of_session_dir d = Filename.concat d "repo"

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

let git_clone cfg repo_dir =
  let command = Printf.sprintf "clone %s %s"
    (Filename.quote cfg.git_repo_url) (Filename.quote repo_dir)
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
        (Filename.concat session.session_repo_dir ".git") "config"))
  in
  run command

let horodate () =
  Unix.(
   let t = localtime (time()) in
   Printf.sprintf "%04d-%02d-%02d-%02dh%02dm%02ds"
     (t.tm_year + 1900) (t.tm_mon + 1) t.tm_mday
     t.tm_hour t.tm_min t.tm_sec
  )
let git_current_branch repo_dir =
  let command = Printf.sprintf
    "cd %s && git rev-parse --abbrev-ref HEAD" (Filename.quote repo_dir)
  in
  try
    let stdin = Unix.open_process_in command in
    let branch = input_line stdin in
    ignore(Unix.close_process_in stdin);
    branch
  with
    _ -> failwith (Printf.sprintf "Exec error: %s" command)

let git_create_branch repo_dir account =
  let branch_name = Printf.sprintf "%s--%s" account.login (horodate()) in
  let command = Printf.sprintf "cd %s && git checkout -b %s"
    (Filename.quote repo_dir) (Filename.quote branch_name)
  in
  run command;
  branch_name

let stog_info_of_repo_dir cfg session_id repo_dir =
  let stog_dir =
    match cfg.Stog_multi_config.stog_dir with
    | None -> repo_dir
    | Some s -> Filename.concat repo_dir s
  in
  let base_url =
    let url =
      List.fold_left Stog_types.url_concat
        cfg.app_url (Stog_multi_page.path_sessions @ [session_id ; "preview" ])
    in
    Neturl.modify_url ~path: ((Neturl.url_path url)@[""]) url
  in
  { stog_dir ;
    stog_state = ref None ;
    stog_ws_cons = ref [] ;
    stog_preview_url = base_url ;
  }

let editor_info_of_stog_info cfg session_id stog_info =
  let url =
    let url =
      List.fold_left Stog_types.url_concat
        cfg.app_url (Stog_multi_page.path_sessions @ [session_id ; "editor" ])
    in
    Neturl.modify_url ~path: ((Neturl.url_path url)@[""]) url
  in
  { editor_ws_cons = ref [] ;
    editor_url = url ;
  }

let load cfg session_id =
  let session_dir = Filename.concat cfg.dir session_id in
  let stored =
    let file = session_info_file session_dir in
    match Yojson.Safe.from_string (Stog_misc.string_of_file file) with
    | exception Yojson.Json_error err ->
        failwith (Printf.sprintf "File %S: %s" file err)
    | json ->
        match stored_of_yojson json with
        | `Error s -> failwith (Printf.sprintf "File %S: %s" file s)
        | `Ok x -> x
  in
  let repo_dir = repo_dir_of_session_dir session_dir in
  let stog_info = stog_info_of_repo_dir cfg session_id repo_dir in
  let editor_info = editor_info_of_stog_info cfg session_id stog_info in
  let session =
    { session_id ;
      session_dir ;
      session_repo_dir = repo_dir ;
      session_stored = stored ;
      session_stog = stog_info ;
      session_editor = editor_info ;
    }
  in
  session

let store_stored session =
  let file = session_info_file session.session_dir in
  let json = stored_to_yojson session.session_stored in
  Stog_misc.file_of_string ~file (Yojson.Safe.to_string json)

let start_session session =
  let stog = read_stog session.session_stog.stog_dir in
  let (state, cons) = Stog_server_preview.new_stog_session
    stog session.session_stog.stog_preview_url
  in
  session.session_stog.stog_state <- state ;
  session.session_stog.stog_ws_cons <- cons

let create cfg account =
  let session_id = new_id () in
  let session_dir = Filename.concat cfg.dir session_id in
  let repo_dir = repo_dir_of_session_dir session_dir in
  let stog_info = stog_info_of_repo_dir cfg session_id repo_dir in
  let editor_info = editor_info_of_stog_info cfg session_id stog_info in

  git_clone cfg repo_dir ;
  let orig_branch = git_current_branch repo_dir in
  let branch_name = git_create_branch repo_dir account in

  let stored =
    { session_create_date = Unix.time () ;
      session_state = Live ;
      session_orig_branch = orig_branch ;
      session_branch = branch_name ;
      session_author = account.login ;
    }
  in
  let session =
    { session_id ;
      session_dir ;
      session_repo_dir = repo_dir ;
      session_stored = stored ;
      session_stog = stog_info ;
      session_editor = editor_info ;
    }
  in
  add_user_info session account ;
  store_stored session ;
  start_session session ;
  session

let load_previous_sessions cfg =
  let dirs = Stog_find.(
     find_list Ignore [cfg.dir]
       [ Maxdepth 1 ; Type Unix.S_DIR ;
         Predicate (fun s ->
            s <> Filename.current_dir_name &&
            s <> Filename.parent_dir_name &&
            s <> cfg.dir)
       ]
    )
  in
  List.iter prerr_endline dirs;
  let f acc dir =
    let file = session_info_file dir in
    if Sys.file_exists file then
      match load cfg (Filename.basename dir) with
      | session -> session :: acc
      | exception Failure msg -> prerr_endline msg ; acc
      | exception e -> prerr_endline (Printexc.to_string e); acc
    else
      acc
  in
  List.fold_left f [] dirs

