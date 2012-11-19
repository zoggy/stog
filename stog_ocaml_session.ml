(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012 Maxence Guesdon. All rights reserved.                   *)
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

(** Program communicating through pipes to:
  - receive ocaml code to evaluate,
  - send evaluation results.
*)


let _ = Toploop.set_paths ();;
let _ = Toploop.initialize_toplevel_env();;
let _ =
  match Hashtbl.find Toploop.directive_table "rectypes" with
    Toploop.Directive_none f -> f ()
  | _ -> assert false;;
let _ = Toploop.max_printer_steps := 20;;



let log_oc = open_out "ocaml.log";;

(*let _ = Location.input_name := "";;*)
let stderr_file = Filename.temp_file "stogocamlsession" "err";;
let stdout_file = Filename.temp_file "stogocamlsession" "out";;

let remove_empty_filename =
  let empty = "File \"\", l" in
  let re = Str.regexp_string empty in
  Str.global_replace re "L"
;;


let eval_ocaml_phrase ~exc phrase =
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
    Printf.fprintf log_oc "executing phrase: %s\n" phrase;
    let phrase = !Toploop.parse_toplevel_phrase lexbuf in
    Printf.fprintf log_oc "phrase parsed\n";
    ignore(Toploop.execute_phrase true Format.str_formatter phrase);
    let exec_output = Format.flush_str_formatter () in
    let err = Stog_misc.string_of_file stderr_file in
    let err = remove_empty_filename err in
    let out = Stog_misc.string_of_file stdout_file in
    Printf.fprintf log_oc "exec_output: %s\n" exec_output;
    Printf.fprintf log_oc "err: %s\n" err;
    Printf.fprintf log_oc "out: %s\n" out;
    (
     match err with
       "" -> ()
     | s -> Format.pp_print_string Format.str_formatter s
    );
    (
     match out with
       "" -> ()
     | s -> Format.pp_print_string Format.str_formatter s
    );
    Format.pp_print_string Format.str_formatter exec_output;
    Stog_ocaml_types.Ok (Stog_misc.strip_string (Format.flush_str_formatter ()), out)
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
          Printf.fprintf log_oc
            "an error happened during phrase error reporting:\n%s\n%!"
            (Printexc.to_string exn);
          Printf.fprintf log_oc "error backtrace:\n%s\n%!"
            (Printexc.get_backtrace ());
      end;
      if not backtrace_enabled then Printexc.record_backtrace false;

      let err = Format.flush_str_formatter () in
      let msg = Printf.sprintf "ocaml error with code:\n%s\n%s" phrase err in
      if exc then
        failwith msg
      else
        Stog_ocaml_types.Exc (Stog_misc.strip_string (remove_empty_filename err))
;;

let eval input =
  try
    let res = eval_ocaml_phrase
      ~exc: input.Stog_ocaml_types.in_err_exc
      input.Stog_ocaml_types.in_phrase
    in
    res
  with e ->
      raise e
;;

let ic_input = Unix.in_channel_of_descr (Unix.dup Unix.stdin);;
let oc_result = Unix.out_channel_of_descr (Unix.dup Unix.stdout);;
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
        prerr_endline (Printexc.to_string e);
        false
  in
  if not finish then loop ()
;;

loop();;
