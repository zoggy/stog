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

open Ojs_server
open Lwt.Infix

type git_repo = {
  repo_dir : Ojs_path.t ;
  origin_url : string ;
  origin_branch : string ;
  edit_branch : string ;
  } [@@deriving yojson]

let git_repo ~origin_url ~origin_branch ~edit_branch ~dir =
  { repo_dir = dir ; origin_url ; origin_branch ; edit_branch }


(*c==v=[Misc.try_finalize]=1.0====*)
let try_finalize f x finally y =
  let res =
    try f x
    with exn -> finally y; raise exn
  in
  finally y;
  res
(*/c==v=[Misc.try_finalize]=1.0====*)

let remove_file file = try Sys.remove file with _ -> ()

let run ?(merge_outputs=false) com =
  let stdout = Filename.temp_file "stogserver" ".stdout" in
  let stderr = Filename.temp_file "stogserver" ".stderr" in
  let command = Printf.sprintf "( %s ) %s"
    com
      (match merge_outputs with
       | false -> Printf.sprintf "> %s 2> %s" (Filename.quote stdout) (Filename.quote stderr)
       | true ->  Printf.sprintf "> %s 2>&1" (Filename.quote stdout)
      )
  in
  let result = Sys.command command in
  let s_out = Stog_misc.string_of_file stdout in
  let s_err = if merge_outputs then "" else Stog_misc.string_of_file stderr in
  remove_file stdout ; remove_file stderr ;
  match result with
    0 -> `Ok (s_out, s_err)
  | _ -> `Error (s_out, s_err)

let in_git_repo git ?merge_outputs com =
  let command = Printf.sprintf "cd %s && %s"
    (Filename.quote (Ojs_path.to_string git.repo_dir)) com
  in
  match run ?merge_outputs command with
  | `Error (out,err)-> `Error (com, out, err)
  | (`Ok _) as x -> x

let with_sshkey key_file com =
  let sub = Filename.quote
    (Printf.sprintf "ssh-add %s; %s" (Filename.quote key_file) com)
  in
  Printf.sprintf "ssh-agent bash -c %s" sub

let with_sshkey_opt sshkey com =
  match sshkey with
  | None -> com
  | Some key_file -> with_sshkey key_file com

let clone ?sshkey git =
  let command = Printf.sprintf "git clone %s %s"
    (Filename.quote git.origin_url) (Filename.quote (Ojs_path.to_string git.repo_dir))
  in
  let com = with_sshkey_opt sshkey command in
  match%lwt Lwt_preemptive.detach run com with
  | `Error (_,err) -> Lwt.fail_with (Printf.sprintf "Error while cloning %s:\n%s" git.origin_url err)
  | `Ok _ -> Lwt.return_unit

let set_user_info git ~name ~email =
  let com =
    Printf.sprintf "git config user.name %s && git config user.email %s"
      (Filename.quote name) (Filename.quote email)
  in
  match in_git_repo git com with
  | `Ok _ -> ()
  | `Error (_,_,err) -> failwith ("Command failed: "^com)

let current_branch git =
  let com = "git rev-parse --abbrev-ref HEAD" in
  match in_git_repo git com with
  | `Error (com,_,err) -> failwith (com^"\n"^err)
  | `Ok (s,_) ->
      match Stog_misc.split_string s ['\n' ; '\r'] with
        [] -> failwith ("No current branch returned by "^com)
      | line :: _ -> line

let create_edit_branch git =
  let com = Printf.sprintf "git checkout -b %s" (Filename.quote git.edit_branch) in
  match in_git_repo git com with
  | `Error (_,_,err) -> failwith (com^"\n"^err)
  | `Ok _ -> ()

let status git =
  let git_com = "git status --porcelain -z" in
  match in_git_repo git git_com with
  | `Error (_,msg,_) -> failwith (git_com^"\n"^msg)
  | `Ok (str,_) -> Stog_git_status.parse str

let has_local_changes git =
  let st = status git in
  let pred (s1, s2, _, _) =
    match s1, s2 with
    | `Q, _ | _, `Q
    | `I, _ | _, `I -> false
    | _ -> true
  in
  List.filter pred st <> []

let in_rebase git =
  let git_com = "ls `git rev-parse --git-dir` | grep rebase" in
  match in_git_repo git ~merge_outputs: true git_com with
    `Error (_,_,_) -> false
  | `Ok (_,_) -> true

let pull_in_origin ?sshkey git =
  let ob = git.origin_branch in
  let b = git.edit_branch in
  let pull_origin () =
    let ob = Filename.quote ob in
    let git_com = Printf.sprintf
      "(git checkout %s && git pull origin %s)" ob ob
    in
    let ssh_com = with_sshkey_opt sshkey git_com in
    match in_git_repo git ~merge_outputs: true ssh_com with
    | `Error (com,msg,_) -> failwith (com^"\n"^msg)
    | `Ok _ -> ()
  in
  let checkout_edit_branch () =
    let git_com = Printf.sprintf "git checkout %s" (Filename.quote b) in
    match in_git_repo git ~merge_outputs: true git_com with
    | `Error (com,msg,_) -> failwith (com^"\n"^msg)
    | `Ok _ -> ()
  in
  try_finalize pull_origin () checkout_edit_branch ()

let diff_fetch_orig git =
  let git_com = "git diff  --name-only FETCH_HEAD..ORIG_HEAD" in
  match in_git_repo git ~merge_outputs: true git_com with
    `Error (com,msg,_) -> failwith (git_com^"\n"^msg)
  | `Ok (msg,_) ->
      let lines = Stog_misc.split_string msg ['\n' ; '\r'] in
      List.map Ojs_path.of_string lines

let rebase_from_origin ?sshkey git =
  let st = status git in
  match in_rebase git with
  | false ->
      begin
        pull_in_origin ?sshkey git ;
        let git_com = Printf.sprintf "git rebase %s" (Filename.quote git.origin_branch) in
        match in_git_repo git ~merge_outputs: true git_com with
          `Error (com,msg,_) -> failwith (com^"\n"^msg)
        | `Ok (msg,_) -> (msg, diff_fetch_orig git)
      end
  | true ->
      match List.exists Stog_git_status.is_unmerged st || has_local_changes git with
      | true ->
          begin
            let git_com = "git rebase --continue" in
            match in_git_repo git ~merge_outputs: true git_com with
              `Error (com,msg,_) -> failwith (com^"\n"^msg)
            | `Ok (msg,_) -> (msg, diff_fetch_orig git)
          end
      | false ->
          begin
            let git_com = "git rebase --skip" in
            match in_git_repo git ~merge_outputs: true git_com with
              `Error (com,msg,_) -> failwith (com^"\n"^msg)
            | `Ok ("",_) -> ("Rebase skipped", [])
            | `Ok (msg,_) -> (msg, [])
          end

let differs_from_origin git =
  let git_com = Printf.sprintf "git diff %s" (Filename.quote git.origin_branch) in
  match in_git_repo git ~merge_outputs: true git_com with
    `Error _ -> false
  | `Ok (msg,_) -> Stog_misc.strip_string msg <> ""

let merge_in_origin git =
  if has_local_changes git then
    failwith "There are local changes. Please commit first.";
  let git_com = Printf.sprintf
    "git checkout %s && git merge %s && git checkout %s"
      (Filename.quote git.origin_branch)
      (Filename.quote git.edit_branch)
      (Filename.quote git.edit_branch)
  in
  match in_git_repo git ~merge_outputs: true git_com with
    `Error (_,msg,_) -> failwith (git_com^"\n"^msg)
  | `Ok (msg,_) -> msg

let push_origin ?sshkey git =
  let com = Printf.sprintf
    "git checkout %s && git push origin %s && git checkout %s"
      (Filename.quote git.origin_branch)
      (Filename.quote git.origin_branch)
      (Filename.quote git.edit_branch)
  in
  let git_com = with_sshkey_opt sshkey com in
  match in_git_repo git ~merge_outputs: true git_com with
    `Error (_,msg,_) -> failwith (com^"\n"^msg)
  | `Ok (msg,_) -> msg

let push ?sshkey git =
  if has_local_changes git then
    failwith "There are local changes. Please commit first.";
  let st = status git in
  if List.exists Stog_git_status.is_unmerged st || in_rebase git then
    failwith "You're currently merge files. Please finish merging first (use commit and pull)." ;
  let (rebase_msg, modified_files) = rebase_from_origin ?sshkey git in
  let merge_msg = merge_in_origin git in
  try
    let push_msg = push_origin ?sshkey git in
    Printf.sprintf "%s\n%s\n%s" rebase_msg merge_msg push_msg
  with Failure msg ->
      failwith (Printf.sprintf "%s\n\n%s\n\n%s" rebase_msg merge_msg msg)

let commit git paths msg =
  let st = status git in
  let git_com =
    match in_rebase git with
    | false ->
        let (add, args) =
          match paths with
            [] -> (false, "-a")
          | _ ->
              (true,
               String.concat " "
                 (List.map (fun p -> Filename.quote (Ojs_path.to_string p)) paths)
              )
        in
        Printf.sprintf "%sgit commit -m%s %s"
          (if add then Printf.sprintf "git add %s &&" args else "")
          (Filename.quote msg) args
    | true ->
        match List.filter Stog_git_status.is_unmerged st with
        | [] -> "echo 'Nothing to be done'"
        | unmerged ->
            begin
              let args =
                String.concat " "
                  (List.map (fun (_,_,p,_) -> Filename.quote (Ojs_path.to_string p))
                   unmerged)
              in
              Printf.sprintf "git add %s" args
            end
  in
  match in_git_repo git ~merge_outputs: true git_com with
    `Error (_,msg,_) -> failwith (git_com^"\n"^msg)
  | `Ok("",_) -> "Done"
  | `Ok (msg,_) -> msg

module Make (P: Stog_git_types.P) =
  struct
    class repo
      (broadcall : P.server_msg -> (P.client_msg -> unit Lwt.t) -> unit Lwt.t)
        (broadcast : P.server_msg -> unit Lwt.t) ~id ?sshkey git =
    object(self)
      method id = (id : string)
      method git = (git : git_repo)
      method sshkey = (sshkey : string option)
      val git_mutex = Lwt_mutex.create ()

      method do_git :
         'a . git_repo -> (git_repo -> 'a Lwt.t) -> 'a Lwt.t =
         fun git f ->
         Lwt_mutex.with_lock git_mutex (fun () -> f git)

      method git_action reply_msg f =
         self#do_git git
           (fun _ ->
              Lwt.catch
                (fun () ->
                  Lwt_preemptive.detach f () >>= fun msg ->
                    reply_msg (P.SOk msg))
                (function
                 | Failure msg -> reply_msg (P.SError msg)
                 | e -> reply_msg (P.SError (Printexc.to_string e))
                 )
            )

      method handle_get_status reply_msg =
        let%lwt st = self#do_git git (Lwt_preemptive.detach status) in
        reply_msg (P.SStatus st)

      method handle_commit reply_msg paths msg =
        self#git_action reply_msg (fun () -> commit git paths msg)

      method handle_rebase_from_origin reply_msg =
        self#git_action reply_msg
              (fun () ->
                 let (msg, files) = rebase_from_origin ?sshkey git in
                 let msg_f = match files with
                     [] -> ""
                   | _ ->
                     Printf.sprintf "\nThe following files were modified:\n- %s"
                       (String.concat "\n -" (List.map Ojs_path.to_string files))
                 in
                 Printf.sprintf "%s%s" msg msg_f
              )


      method handle_push reply_msg =
        self#git_action reply_msg (fun () -> push ?sshkey git)

      method handle_message
            (send_msg : P.server_msg -> unit Lwt.t) (msg : P.client_msg) =
        self#handle_call send_msg msg

      method handle_call
            (reply_msg : P.server_msg -> unit Lwt.t) (msg : P.client_msg) =
        match msg with
        | P.Status ->
            self#handle_get_status reply_msg
        | P.Commit (paths, msg) ->
            self#handle_commit reply_msg paths msg
        | P.Rebase_from_origin ->
            self#handle_rebase_from_origin reply_msg
        | P.Push ->
            self#handle_push reply_msg
        | _ ->
            reply_msg (P.SError "Unhandled message in git repo")
      end

class repos
  (broadcall : P.app_server_msg -> (P.app_client_msg -> unit Lwt.t) -> unit Lwt.t)
    (broadcast : P.app_server_msg -> unit Lwt.t)
    (spawn : (P.server_msg -> (P.client_msg -> unit Lwt.t) -> unit Lwt.t) ->
     (P.server_msg -> unit Lwt.t) ->
       id: string -> ?sshkey: string -> git_repo -> repo
    )
    =
    object(self)
      val mutable repos = (SMap.empty : repo SMap.t)

      method repo id =
        try SMap.find id repos
        with Not_found -> failwith (Printf.sprintf "No repository with id %S" id)

      method add_repo ~id ?sshkey git =
        let broadcall msg cb =
          let cb msg =
             match P.unpack_client_msg msg with
             | Some (_, msg) -> cb msg
             | None -> Lwt.return_unit
          in
          broadcall (P.pack_server_msg id msg) cb
        in
        let broadcast msg = broadcast (P.pack_server_msg id msg) in
        let repo = spawn broadcall broadcast ~id ?sshkey git in
        repos <- SMap.add id repo repos;
        repo

      method handle_message
        (send_msg : P.app_server_msg -> unit Lwt.t) (msg : P.app_client_msg) =
          match P.unpack_client_msg msg with
          | Some (id, msg) ->
              let send_msg msg = send_msg (P.pack_server_msg id msg) in
              (self#repo id)#handle_message send_msg msg
          | None -> Lwt.return_unit

      method handle_call
         (return : P.app_server_msg -> unit Lwt.t) (msg : P.app_client_msg) =
        match P.unpack_client_msg msg with
        | Some (id, msg) ->
            let reply_msg msg = return (P.pack_server_msg id msg) in
            (self#repo id)#handle_call reply_msg msg
        | None -> Lwt.return_unit
  end
end