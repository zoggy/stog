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

open Stog_ocaml_types;;

let _ = Toploop.set_paths ();;
let _ = Toploop.initialize_toplevel_env();;
let _ =
  match Hashtbl.find Toploop.directive_table "rectypes" with
    Toploop.Directive_none f -> f ()
  | _ -> assert false;;
let _ = Toploop.max_printer_steps := 20;;




(*let _ = Location.input_name := "";;*)
let stderr_file = Filename.temp_file "stogocamlsession" "err";;
let stdout_file = Filename.temp_file "stogocamlsession" "out";;
let log_file = Filename.temp_file "stogocamlsession" "log";;
let log_oc = open_out log_file;;
let log s = output_string log_oc s ; output_string log_oc "\n";;

let remove_empty_filename =
  let empty = "File \"\", l" in
  let re = Str.regexp_string empty in
  Str.global_replace re "L"
;;


let eval_ocaml_phrase phrase =
  try
    let lexbuf = Lexing.from_string phrase in
    let fd_err = Unix.openfile stderr_file
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
      0o640
    in
    Unix.dup2 fd_err Unix.stderr;
    let fd_out = Unix.openfile stdout_file
      [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC]
      0o640
    in
    Unix.dup2 fd_out Unix.stdout;
    Unix.close fd_out;
    log ("executing phrase: " ^ phrase);
    let phrase = !Toploop.parse_toplevel_phrase lexbuf in
    log "phrase parsed";
    let ok = Toploop.execute_phrase true Format.str_formatter phrase in
    let output =
      { topout = Format.flush_str_formatter () ;
        stderr = remove_empty_filename (Stog_misc.string_of_file stderr_file) ;
        stdout = Stog_misc.string_of_file stdout_file ;
      }
    in
    log ("exec_output: " ^ output.topout);
    log ("err: " ^ output.stderr);
    log ("out: " ^ output.stdout);
    if ok then
      Stog_ocaml_types.Ok output
    else
      Stog_ocaml_types.Handled_error output
  with
  | e ->
      (* Errors.report_error relies on exported compiler lib; on some
         bugged setups, those libs are not in synch with the compiler
         implementation, and the call below fails
         because of an implementation mismatch with the toplevel.

         We are therefore extra careful when calling
         Errors.report_error, and in particular collect backtraces to
         help spot this vicious issue. *)

      let backtrace_enabled = Printexc.backtrace_status () in
      if not backtrace_enabled then Printexc.record_backtrace true;
      begin
        try Errors.report_error Format.str_formatter e
        with exn ->
          log ("an error happened during phrase error reporting:\n"^(Printexc.to_string exn));
          log ("error backtrace:\n%s"^(Printexc.get_backtrace ()));
      end;
      if not backtrace_enabled then Printexc.record_backtrace false;

      let err = Format.flush_str_formatter () in
      Stog_ocaml_types.Exc (Stog_misc.strip_string (remove_empty_filename err))
;;

let eval input =
  try
    let res = eval_ocaml_phrase
      input.Stog_ocaml_types.in_phrase
    in
    res
  with e ->
      raise e
;;

let add_directory =
  match Hashtbl.find Toploop.directive_table "directory" with
    Toploop.Directive_string f -> f
  | _ -> failwith "Directive \"directory\" not found"
;;

let option_package s =
  let packages = String.concat " " (Stog_misc.split_string s [',']) in
  let temp_file = Filename.temp_file "stogocamlsession" ".txt" in
  let com = Printf.sprintf "ocamlfind query %s > %s"
    packages (Filename.quote temp_file)
  in
  match Sys.command com with
    0 ->
      let dirs = Stog_misc.split_string
        (Stog_misc.string_of_file temp_file) ['\n' ; '\r']
      in
      List.iter add_directory dirs;
      (try Sys.remove temp_file with _ -> ())
  | n ->
      (try Sys.remove temp_file with _ -> ());
      failwith (Printf.sprintf "Command %S failed with error code %d" com n)
;;

let parse_options () =
  let usage = Printf.sprintf "Usage: %s [options]\nwhere options are:" Sys.argv.(0) in
  Arg.parse
    [
      "-I", Arg.String add_directory,
      "<dir> add <dir> to the list of include directories" ;

      "-package", Arg.String option_package,
      "<pkg1[,pkg2[,...]]> add ocamlfind packages to the list of include directories" ;

      "-w", Arg.String (Warnings.parse_options false),
      "<list>  Enable or disable warnings according to <list>" ;

      "-warn-error", Arg.String (Warnings.parse_options true),
      "<list>  Enable or disable error status for warnings according to <list>" ;
    ]
    (fun _ -> ())
    usage
;;

let main () =
  parse_options ();
  let ic_input = Unix.in_channel_of_descr (Unix.dup Unix.stdin) in
  let oc_result = Unix.out_channel_of_descr (Unix.dup Unix.stdout) in
  let old_stderr = Unix.dup Unix.stderr in
  let rec loop () =
    let finish =
      try
        let input = Stog_ocaml_types.read_input ic_input in
        let res = eval input in
        Stog_ocaml_types.write_result oc_result res;
        false
      with
        End_of_file -> true
      | e ->
          let msg =
            match e with
              Failure msg -> msg
            | e -> Printexc.to_string e
          in
          let oc = Unix.out_channel_of_descr old_stderr in
          output_string oc msg;
          flush oc;
          false
    in
    if not finish then loop ()
  in
  loop ();
  close_out oc_result
;;

try main ()
with
  Sys_error s | Failure s ->
    prerr_endline s;
    exit 1
| e ->
    prerr_endline (Printexc.to_string e);
    exit 1
;;

