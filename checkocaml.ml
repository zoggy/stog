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

(** Detection of OCaml tools and libraries. *)

(* $Id$ *)

(*c==m=[OCaml_conf]=0.8=t==*)


  open Sys
  open Unix

(** {2:configuration Configuration}
You can change these values to modify the behaviour of some functions. *)

(** The prefix of the temporary files created. *)
let temp_files_prefix = ref "config_check"

(** The default extension for the temporary files created. *)
let temp_files_ext = ref "ml"

(** Set this to [true] to enable debug mode.
  Temporary files are not deleted when it is enabled. *)
let debug = ref false

(** The function used to print progress messages and other information. *)
let print = ref (fun s -> print_string s; flush Pervasives.stdout)

(** [!string_of_bool b] should return a message according to the given boolean.
   Default are yes/no. *)
let string_of_bool = ref (function true -> "yes" | false -> "no")

(** The name of the log file. *)
let log_file = ref "config_check.log"

(** The function to print a given fatal error message. Should exit the
   configuration process. *)
let fatal_error = ref
    (fun s ->
      prerr_endline s;
      prerr_endline (Printf.sprintf "You way have a look at %s for details." !log_file);
      exit 1
    )


(** {2:utils Utilities functions} *)

let try_finalize f x finally y =
  let res = try f x with exn -> finally y; raise exn in
  finally y;
  res;;

let rec restart_on_EINTR f x =
  try f x with Unix_error (EINTR, _, _) -> restart_on_EINTR f x


(** [is_suffix ~suf s] returns [true] if the string [suf] is a suffix of [s]. *)
let is_suffix ~suf s =
  let len_suf = String.length suf in
  let len_s = String.length s in
  len_s >= len_suf &&
  String.sub s (len_s - len_suf) len_suf = suf


(** [create_temp_files ?ext ?contents ()] creates a temporary empty file and
   returns its name. The prefix of the file is specified by {!temp_files_prefix}.
   @param ext can be used to indicate a extension different from {!temp_files_ext}.
   @param contents can be used to specify the content of the file.
*)
let create_temp_file ?(ext= !temp_files_ext) ?(contents="") () =
  let (file,oc) = Filename.open_temp_file
      !temp_files_prefix ("." ^ ext)
  in
  output_string oc contents;
  close_out oc;
  file


let remove_empty_strings = List.filter ((<>) "")

let string_of_includes l =
  String.concat " " (List.map (fun s -> "-I "^(Filename.quote s)) (remove_empty_strings l))

(** If the given filename has a [.cmo] (resp. [.cma]) extension,
  then return the same filename with a [.cmx] (resp. [.cmxa]) extension.*)
let byte_ext_to_opt f =
  if Filename.check_suffix f ".cmo" then
    Printf.sprintf "%s.cmx" (Filename.chop_extension f)
  else
    if Filename.check_suffix f ".cma" then
      Printf.sprintf "%s.cmxa" (Filename.chop_extension f)
    else
      f

(** {2:path Handling PATH} *)

exception Path of string * string;;
let path_sep = ":";;
let path_sep_regexp = Str.regexp (Str.quote path_sep);;

(** [list_of_path string] returns the list of directories from the given string in path format. *)
let list_of_path = Str.split path_sep_regexp;;

(** [path_of_list paths] builds a string in path format.
   @raise Path if a path contains the separator. *)
let path_of_list paths =
  (* Un nom de fichier dans un chemin ne doit pas contenir le séparateur... *)
  let check s =
    if Str.string_match path_sep_regexp s 0 then
      let pos = Str.match_beginning() in
      let mes = Printf.sprintf "Separator string found at position %d" pos in
      raise (Path (s, mes)) in
  List.iter check paths;
  String.concat path_sep paths;;

(** [get_path ()] returns the list of directories indicated by the [PATH] environement variable. *)
let get_path () = list_of_path (getenv "PATH");;

(** [find_in_path predicate paths file] returns the list of complete filenames build
   from the directories and the filename, and verifying the given predicate. *)
let find_in_path p paths file =
  List.find_all p (List.map (fun p -> Filename.concat p file) paths);;

(** {2:files Handling files} *)

(** Various tests of a file. *)
type filetest =
  | Fexists (** file exists *)
  | Freadable (** file exists and is readable *)
  | Fwritable (** file exists and is writable *)
  | Fexecutable (** file exists and is executable *)
  | Fdir (** file exists and is a directory *)
  | Freg (** file exists and is a regular file *)
  | Flnk (** file exists and is a symbolic link *)
  | Fnonempty (* file* exists and is non empty *)
  | Fnewer of string (** files exists and is newer than *)
  | Folder of string (** files exists and is older than *)
  | Fequal of string (** files is identical (sams st_ino and st_dev) than *)

let access_map =
  [ Freadable, R_OK; Fwritable, W_OK; Fexecutable, X_OK; Fexists, F_OK; ]

let access_ok_errors =
  [ EACCES ; EROFS; ENOENT; ENOTDIR; ELOOP ]
;;

(** [testfile flags filename] tests whether the given file verifies the given properties. *)
let testfile flags filename =
  let rec split ( (found, left) as both) = function
      [] -> both
    | h :: t ->
        let found_left =
          try List.assoc h access_map  :: found, left
          with Not_found -> found, h::left in
        split found_left t
  in
  let access_flags, flags = split ([],[]) flags in
  let access_flags = if access_flags = [] then [ F_OK ] else access_flags in
  try
    access filename access_flags;
    flags = [] ||
    begin
      let st = (if List.mem Flnk flags then lstat else stat) filename in
      let rec test = function
        | Fdir -> st.st_kind = S_DIR
        | Freg -> st.st_kind = S_REG
        | Flnk -> st.st_kind = S_LNK
        | Fnonempty -> st.st_size > 0
        | Fnewer string -> st.st_mtime > (stat string).st_mtime
        | Folder string -> st.st_mtime < (stat string).st_mtime
        | Fequal string ->
            let st' = stat string in
            st.st_ino = st'.st_ino && st.st_dev = st'.st_dev
        | _ -> assert false
      in
      List.for_all test flags
    end;
  with Unix.Unix_error (err, _, _) when  List.mem err access_ok_errors ->
    false
;;

let buffer_size = 4096
let string_from_descr fd =
  let rec readfd accu =
    let str = String.create buffer_size in
    match restart_on_EINTR (read fd str 0) buffer_size with
    | 0 -> String.concat ""  accu
    | n ->
        let str = if n < buffer_size then String.sub str 0 n else str in
        readfd (str :: accu) in
  readfd []
;;

let descr_from_string str fd =
  let rec writefd offset left =
    if left > 0 then
      let n = restart_on_EINTR (single_write fd str offset) left in
      writefd (offset + n) (left - n) in
  writefd 0 (String.length str)
;;

let perm = 0o640;;


(** [string_of_files] returns the contents of the given file as a string.
   @raise Unix.Unix_error if an error occurs.
*)
let string_of_file file =
  let fd = openfile file [ O_RDONLY ] 0 in
  try_finalize string_from_descr fd close fd
;;

(** [file_of_string ~contents ~file] creates the given file with the given string [str] as contents.
   @raise Unix.Unix_error if an error occurs.
*)
let file_of_string ~contents ~file =
  let fd = openfile file [ O_WRONLY; O_CREAT; O_TRUNC ] perm in
  try_finalize (descr_from_string contents) fd close fd
;;

(** [input_lines channel] return the list of lines from the given channel.
   The function is tail-recursive.
*)
let input_lines chan =
  let rec all lines =
    (* intermediate result to be tail rec *)
    match try Some (input_line chan) with End_of_file -> None with
      Some l -> all (l::lines)
    | None -> List.rev lines in
  all []

(** [unlink_f file] removes the given [file] if it exists.
   If the files is a [.ml] file (resp. a [.mli] file), then
   it also removes the [.cmo], [.cmx], [.o] and [.cmi] files
   (resp. the [.cmi] file) if they exist.
   @raise Unix.Unix_error if an error occurs.
*)
let unlink_f file =
  if !debug then ()
  else
    let files =
      if Filename.check_suffix file ".mli" then
	[file; (Filename.chop_extension file)^".cmi"]
      else
	if Filename.check_suffix file ".ml" then
	  let base = Filename.chop_extension file in
	  [file; base^".cmi" ; base^".cmo" ; base^".cmx"; base^".o" ]
	else
	  [file]
    in
    let f file =
      try unlink file with Unix_error (ENOENT, _, _) -> ()
    in
    List.iter f files



(** {2:exec Handling processes} *)

exception Exec_failure;;
let execvp_to_list cmd args =
  let desc_read, desc_write = pipe () in
  match fork() with
    0 ->
      let exec () =
        close desc_read;
        dup2 desc_write stdout;
        close desc_write;
        execvp cmd (Array.append [| cmd |] args) in
      handle_unix_error exec ()
  | pid ->
      close desc_write;
      let chan = in_channel_of_descr desc_read in
      let after () =
        close_in chan;
        match restart_on_EINTR (waitpid []) pid with
          _, WEXITED 0 -> ()
        | _, _ -> raise Exec_failure in
      try_finalize input_lines chan after ();;

type redirection =
  | In_from_file of string        (** < file *)
  | Out_to_file of string         (** > file *)
  | Err_to_file of string         (** 2> file *)
  | Out_append_to_file of string  (** >> file *)
  | Err_to_out                    (** 2>&1 *)
  | In_from_string of string      (** <<END *)
  | Err_null                      (** 2>/dev/null  *)
  | Out_null                      (** >/dev/null *)
  | Silent                        (** >/dev/null 2>&1 *)
;;

let execvp_redirect redirections cmd args  =
  let perm = 0o640 in
  let temp_file =
    if List.exists (function In_from_string _ -> true | _ -> false)
        redirections
    then Some (create_temp_file ~ext: ".in" ())
    else None in
  let rec make_redirect rd =
    match rd with
      In_from_file file ->
        let desc_file = openfile file [O_RDONLY] perm in
        try_finalize (dup2 desc_file) stdin close desc_file
    | Out_to_file file ->
        let desc_file = openfile file [O_WRONLY;O_CREAT;O_TRUNC] perm in
        try_finalize (dup2 desc_file) stdout close desc_file
    | Err_to_file file  ->
        let desc_file = openfile file [O_WRONLY;O_CREAT;O_TRUNC] perm in
        try_finalize (dup2 desc_file) stderr close desc_file
    | Out_append_to_file file ->
        let desc_file = openfile file [O_WRONLY;O_APPEND;O_CREAT] perm in
        try_finalize (dup2 desc_file) stdout close desc_file
    | Err_to_out -> dup2 stdout stderr
    | In_from_string s ->
        begin match temp_file with
          Some tmp ->
            file_of_string ~file: tmp ~contents: s;
            make_redirect (In_from_file tmp);
        | None -> assert false
        end
    | Out_null ->
        make_redirect (Out_to_file "/dev/null")
    | Err_null ->
        make_redirect (Err_to_file "/dev/null")
    | Silent ->
        make_redirect Out_null;
        make_redirect Err_to_out;
  in
  match fork () with
    0 ->
      let exec () =
        List.iter make_redirect redirections;
        execvp cmd (Array.append [|cmd|] args);
      in
      handle_unix_error exec ();
  | pid ->
      let res = snd (waitpid [] pid) in
      begin match temp_file with
        Some tmp -> unlink_f tmp
      | _ -> ()
      end;
      res
;;


let execvp cmd arg =
  match fork() with
  | 0 -> Unix.execvp cmd (Array.concat [ [| cmd |]; arg ])
  | p -> snd(waitpid [] p)

(** [exec_and_get_first_line com args] tries to execute the given command with
   the given arguments, and read the first line printed by the commande on its
   standard output. If any error occurs or the program doesn't print anything on stdout,
   the function returns [""].*)
let exec_and_get_first_line com args =
  match
    handle_unix_error
      (fun () -> execvp_to_list com args) ()
  with
    [] -> ""
  | h :: _ -> h

(** [exec_status_ok st] returns [true] if the given return status is [Unix.WEXITED 0]. *)
let exec_status_ok = function
    Unix.WEXITED 0 -> true
  | _ -> false


(** {2 Writing to log file} *)

let string_of_date t =
  let d = Unix.localtime t in
  Printf.sprintf "%02d/%02d/%02d %02d:%02d:%02d"
    (d.Unix.tm_year + 1900) (d.Unix.tm_mon+1) d.Unix.tm_mday
    d.Unix.tm_hour d.Unix.tm_min d.Unix.tm_sec

(** [add_to_log str] writes the given string to the {!log_file},
   with the date and time at the beginning of the line and an ending new line.*)
let add_to_log s =
  let oc = open_out_gen
      [Open_wronly ; Open_append ; Open_creat ; Open_text ]
      0o644 !log_file
  in
  Printf.fprintf oc "%s: %s\n"
    (string_of_date (Unix.time()))
    s;
  close_out oc

(** {2 Handling version numbers } *)

type version = int list

(** [v1 << v2] returns [true] if version [v1] is strictly inferior to version [v2]. *)
let (<<) v1 v2 =
  let rec iter = function
      ([], []) -> false
    | ([], _) -> true
    | (_,[]) -> false
    | (h1::q1, h2::q2) ->
        (h1 < h2) ||
        (h1 = h2 && (iter (q1,q2)))
  in
  iter (v1,v2)

(** The dummy version number = [[0]]. *)
let dummy_version = [0]

(** [version_of_string s] returns a version number from the given string [s].
   [s] must b in the form [X[.Y[.Z[...]]]]. If the string is not correct,
   then the dummy version is returned. *)
let version_of_string s =
  let l = Str.split (Str.regexp_string ".") s in
  try List.map int_of_string l
  with Failure _ -> dummy_version

(** [string_of_version v] returns a string to display the given version.
   For example, [string_of_version [1;2;3] = "1.2.3"]. *)
let string_of_version v =
  String.concat "." (List.map string_of_int v)

(** {2 Handling OCaml configuration} *)

(** Representation of the (locally detected) ocaml configuration. *)
type ocaml_conf =
    {
      ocaml : string ;
      ocamlc : string ;
      ocamlopt : string ;
      ocamldep : string ;
      ocamldoc : string ;
      ocamldoc_opt : string ;
      ocamllex : string ;
      ocamlyacc : string ;
      ocamlmklib : string ;
      ocamlmktop : string ;
      ocamlprof : string ;
      camlp4 : string;
      ocamlfind : string ;
      version_string : string ;
      version : version ;
    }



exception Program_not_found of string

(** [ocaml_prog file] return the first executable called [file] in
   the directories of the PATH environment variable.
   @param err can be used to indicate whether not finding the program
   should raise the [Program_not_found] exception ([err=true], default)
   or rather return an empty string ([err=false]).
   The function prints messages indicating what program is searched, and the result,
   using the {!print} function.
*)
let ocaml_prog ?(err=true) file =
  !print (Printf.sprintf "checking for %s... " file);
  match
    handle_unix_error
      (fun () -> find_in_path (testfile [Fexecutable;Freg]) (get_path()) file)
      ()
  with
    [] ->
      !print "no\n";
      if err then raise (Program_not_found file) else ""
  | s :: _ ->
      !print (s^"\n"); s
;;

(** [ocaml_libdir conf] uses the [ocamlc] program of the given configuration
   to retrieve the library directory.
   Return [""] if any error occurs.*)
let ocaml_libdir conf =
  match
    handle_unix_error
      (fun () -> execvp_to_list conf.ocamlc [| "-v" |])
      ()
  with
    [] | [_] -> ""
  | _ :: l :: _ ->
      try
	let p = String.index l ':' in
	String.sub l (p+2) (String.length l - p - 2)
      with
	Invalid_argument _
      |	Not_found ->
	  ""
;;

(** [version_of_ocaml_version_string str] returns a {!type:version} value from
   the string representing an ocaml version (which can contain '+', a date, ...). *)
let version_of_ocaml_version_string s =
  let len = String.length s in
  let b = Buffer.create len in
  let rec iter n =
    if n < len then
      match s.[n] with
	'0'..'9' | '.' -> Buffer.add_char b s.[n]; iter (n+1)
      | _ -> ()
  in
  iter 0;
  version_of_string (Buffer.contents b)

(** [check_version version prog] tries to run [prog -version] and return
   whether the given version string is a suffix of the first line printed.
   This function is used to check that an ocaml program has the correct
   version number.
   @param on_err can be used to indicate another function to call it
   the program is not in the correct version. Default is [!]{!fatal_error}.
*)
let check_version ?(on_err= !fatal_error) version prog =
  match prog with
    "" -> ()
  | _ ->
      match handle_unix_error
	  (fun () -> execvp_to_list prog [| "-version" |]) ()
      with
	[] ->
	  let s = Printf.sprintf "could not get version of %s" prog in
	  on_err s
      | s :: _ ->
	  if not (is_suffix ~suf: version s) then
	    let s = Printf.sprintf "%s is not version %s" prog version in
	    on_err s

(** [check_conf_version conf] verifies that each program of the given configuration
   is of the correct version.
   If a program is not in the correct version, [!]{!fatal_error} is called.
*)
let check_conf_versions conf =
  List.iter (check_version conf.version_string)
    [ conf.ocamlopt ; conf.ocamldep ; conf.ocamldoc ;
      conf.ocamllex ; conf.ocamlyacc ; conf.ocamlmklib ;
      conf.ocamlmktop ;
      (* not yet conf.ocamlprof ; *)
    ]

(** [check_for_opt_prog version prog] checks whether the [prog.opt] program
   is in the required version, and if so returns this program name; else
   it returns the given program name. *)
let check_for_opt_prog version prog =
  match prog with
    "" -> ""
  | _ ->
      let opt = Printf.sprintf "%s.opt" (Filename.basename prog) in
      try
        let opt = ocaml_prog opt in
        check_version
          ~on_err:(fun m -> !print (m^"\n"); raise (Program_not_found ""))
          version opt;
	    !print (Printf.sprintf "we can use %s\n" opt);
	    opt
      with
	    Program_not_found _ -> prog

(** [get_opt_conf conf] returns the same configuration where some program
   names have been replaced by the "opt" version (["..../ocamlc.opt"] instead
   of ["..../ocamlc"] for example).
   To do so, the function verifies if the ".opt" program found for each program
   of the configuration is in the same version of the bytecode program.
   For [ocamldoc], the program in the [ocamldoc] field is not replaced but the [ocamldoc_opt]
   field is set to ["..../ocamldoc.opt"], because both bytecode and native versions
   can be used (for example the bytecode version is required to use custom generators).*)
let get_opt_conf conf =
  let f = check_for_opt_prog conf.version_string in
  { conf with
    ocamlc = f conf.ocamlc ;
    ocamlopt = f conf.ocamlopt ;
    ocamldoc_opt = f conf.ocamldoc ;
    ocamllex = f conf.ocamllex ;
    ocamldep = f conf.ocamldep ;
  }

(** [ocaml_conf ()] detects and returns the Objective Caml configuration found from the PATH.
   The function checks that the programs found are in the same version (see {!check_conf_versions}),
   and refer to the ".opt" programs when they are available (see {!get_opt_conf}).
   @param withopt can be used to raise a {!Program_not_found} exception if the native code compiler is not found;
   default is [false].
   @param ocamlfind can be used to end up with a {!Program_not_found} exception if the [ocamlfind] tool is not found;
   default is [false].
   @raise Program_not found if a required program cannot be found.
*)
let ocaml_conf ?(withopt=false) ?(ocamlfind=false) () =
  let ocamlc = ocaml_prog "ocamlc" in
  let version_string = exec_and_get_first_line  ocamlc [| "-version" |] in
  let version = version_of_ocaml_version_string version_string in
  !print (Printf.sprintf "found OCaml version %s (%s)\n" (string_of_version version) version_string);
  let conf = {
    ocamlc = ocamlc ;
    version_string = version_string ;
    version = version ;
    ocaml = ocaml_prog "ocaml" ;
    ocamlopt = ocaml_prog ~err: withopt "ocamlopt" ;
    ocamldoc = ocaml_prog "ocamldoc" ;
    ocamldoc_opt = "" ;
    ocamlyacc = ocaml_prog "ocamlyacc" ;
    ocamllex = ocaml_prog "ocamllex" ;
    ocamldep = ocaml_prog "ocamldep" ;
    ocamlmklib = ocaml_prog "ocamlmklib" ;
    ocamlmktop = ocaml_prog "ocamlmktop" ;
    ocamlprof = ocaml_prog "ocamlprof" ;
    camlp4 = ocaml_prog "camlp4" ;
    ocamlfind = ocaml_prog ~err: ocamlfind "ocamlfind" ;
  } in
  check_conf_versions conf;
  get_opt_conf conf

(** [print_conf conf] prints the given configuration using [!]{!print}. *)
let print_conf c =
  let sp = Printf.sprintf in
  !print (sp "Objective-Caml version %s (%s)\n" (string_of_version c.version) c.version_string);
  !print (sp "interpreter:                %s\n" c.ocaml);
  !print (sp "bytecode compiler:          %s\n" c.ocamlc);
  !print (sp "native code compiler:       %s\n" c.ocamlopt);
  !print (sp "documentation generator(s): %s%s\n" c.ocamldoc
	    (if c.ocamldoc_opt <> "" && c.ocamldoc_opt <> c.ocamldoc then
	      sp ", %s" c.ocamldoc_opt else ""
	    )
	 );
  !print (sp "lexer generator:            %s\n" c.ocamllex);
  !print (sp "parser generator:           %s\n" c.ocamlyacc);
  !print (sp "dependencies generator:     %s\n" c.ocamldep);
  !print (sp "library builder:            %s\n" c.ocamlmklib);
  !print (sp "toplevel builder:           %s\n" c.ocamlmktop);
  !print (sp "profiler:                   %s\n" c.ocamlprof);
  !print (sp "camlp4:                     %s\n" c.camlp4);
  (match c.ocamlfind with "" -> () | s ->
    !print (sp "ocamlfind:                  %s\n" s))


(** {2:compiling Testing compilation and link} *)

type compilation_mode = [ `Byte | `Opt ]

let string_of_mode = function
    `Byte -> "byte"
  | `Opt -> "opt"

let ocamlc_of_mode conf = function
    `Byte -> conf.ocamlc
  | `Opt -> conf.ocamlopt

let string_of_com_args com args =
  Printf.sprintf "%s %s"
    com
    (String.concat " "
       (List.map Filename.quote (Array.to_list args)))

let can_compile mode conf ?(includes=[]) ?(options=[]) file =
  let ocamlc = ocamlc_of_mode conf mode in
  let args = Array.of_list
      (
       "-c" ::
       (List.flatten (List.map (fun s -> [ "-I" ; s]) (remove_empty_strings includes))) @
       options @
       [file]
      )
  in
  add_to_log (string_of_com_args ocamlc args);
  exec_status_ok (execvp_redirect [Out_append_to_file !log_file;Err_to_out] ocamlc args)

let can_compile_prog ?mes mode conf ?includes ?options prog =
  (match mes with None -> () | Some s -> !print s);
  let res =
    handle_unix_error (fun () ->
      let file = create_temp_file ~contents: prog () in
      try_finalize (can_compile mode conf ?includes ?options) file unlink_f file) ()
  in
  (match mes with None -> () | Some _ -> !print ((!string_of_bool res)^"\n"));
  res
;;

let ocaml_defined ?mes mode conf ?includes ?options v =
  handle_unix_error (fun () ->
    let prog = Printf.sprintf "let _ = %s;;\n" v in
    can_compile_prog ?mes mode conf ?includes ?options prog) ()
;;

let ocaml_value_has_type ?mes mode conf ?includes ?options v t =
  handle_unix_error (fun () ->
    let prog = Printf.sprintf "let (_ : %s) = %s;;\n" t v in
    can_compile_prog ?mes mode conf ?includes prog) ()
;;

let can_link ?mes mode conf ?out ?(includes=[]) ?(libs=[]) ?(options=[]) files =
  (match mes with None -> () | Some s -> !print s);
  let ocamlc = ocamlc_of_mode conf mode in
  let libs = match mode with
    `Byte -> libs
  | `Opt -> List.map byte_ext_to_opt libs
  in
  let files = match mode with
    `Byte -> files
  | `Opt -> List.map byte_ext_to_opt files
  in
  let outfile = match out with
    None -> create_temp_file ~ext: "out" ()
  | Some f -> f
  in
  let args = Array.of_list
      (
       "-linkall" ::
       "-o" :: outfile ::
       (List.flatten (List.map (fun s -> ["-I"; s]) (remove_empty_strings includes))) @
       libs @
       options @
       files
      )
  in
  add_to_log (string_of_com_args ocamlc args);
  let success =
    exec_status_ok (execvp_redirect [Out_append_to_file !log_file; Err_to_out] ocamlc args)
  in
  (
   match out with
     None -> unlink_f outfile
   | Some _ when not success -> unlink_f outfile
   | _ -> ()
  );
  (match mes with None -> () | Some _ -> !print ((!string_of_bool success)^"\n"));
  success

let try_run com = Sys.command com = 0

(** {2:ocamlfind OCamlfind queries} *)

(** [ocamlfind_query conf package] returns the first line printed by "ocamlfind query package".
   If an error occurs (the package does not exist, ocamlfind cannot be executed, ...),
   the function returns [None].
*)
let ocamlfind_query conf package =
  match conf.ocamlfind with
    "" -> None
  | _ ->
      try
	match exec_and_get_first_line conf.ocamlfind [| "query"; package |] with
	  "" -> None
	| s -> Some s
      with
	_ -> None

let ocamlfind_query_version conf package =
  match conf.ocamlfind with
    "" -> None
  | _ ->
      try
        match exec_and_get_first_line conf.ocamlfind
          [| "query" ; "-format" ; "%v" ; package |]
        with
          "" -> None
        | s -> Some s
      with
        _ -> None
;;

let check_ocamlfind_package ?min_version ?max_version ?(fail=true) ?not_found conf name =
  !print (Printf.sprintf "checking for %s... " name);
  let not_found =
    match not_found with
      None ->
        begin
        function
          | `Package_not_installed pkg ->
              let msg = Printf.sprintf "Package %s not found\n" pkg in
              if fail then
                !fatal_error msg
              else
                !print msg
          | `Package_bad_version version ->
              let msg =
                (Printf.sprintf "Package %s found with version %s, but wanted %s%s%s\n"
                 name version
                 (match min_version with
                    None -> ""
                  | Some v -> Printf.sprintf ">= %s" (string_of_version v)
                 )
                 (match min_version, max_version with
                    Some _, Some _ -> " and "
                  | _ -> ""
                 )
                 (match max_version with
                    None -> ""
                  | Some v -> Printf.sprintf "<= %s" (string_of_version v)
                 )
                )
              in
              if fail then
                !fatal_error msg
              else
                !print msg
        end
    | Some f -> f
  in
   match ocamlfind_query conf name with
      None -> not_found (`Package_not_installed name); false
    | Some s ->
        match min_version, max_version with
          None, None -> !print "ok\n"; true
        | _ ->
            match ocamlfind_query_version conf name with
              None -> not_found (`Package_bad_version "<no version>"); false
            | Some s ->
                let version = version_of_string s in
                let min = match min_version with None -> [] | Some v -> v in
                let max = match max_version with None -> [max_int] | Some v -> v in
                if version < min || version > max then
                  (
                   not_found (`Package_bad_version s);
                   false
                  )
                else
                  ( !print "ok\n" ; true )
;;

(** {2:substs Handling substitutions specification} *)

let substs = Hashtbl.create 37
let add_subst = Hashtbl.replace substs
let subst_val var =
  try Hashtbl.find substs var
  with Not_found -> ""
let get_substs_list () =
  Hashtbl.fold (fun var v acc -> (var,v)::acc) substs []

let output_substs oc =
  List.iter
    (fun (var, v) -> Printf.fprintf oc "%s=\"%s\"\n" var v)
    (get_substs_list ())

let output_substs_to_file file =
  let contents = String.concat "\n"
      (List.map (fun (var,v) -> Printf.sprintf "%s=\"%s\"" var v) (get_substs_list()))
  in
  file_of_string ~file ~contents

let add_conf_variables c =
   let l =
   [ "OCAML", c.ocaml ;
     "OCAMLC", c.ocamlc ;
     "OCAMLOPT", c.ocamlopt ;
     "OCAMLDEP", c.ocamldep ;
     "OCAMLDOC", c.ocamldoc ;
     "OCAMLDOC_OPT", c.ocamldoc_opt ;
     "OCAMLLEX", c.ocamllex ;
     "OCAMLYACC", c.ocamlyacc ;
     "OCAMLMKLIB", c.ocamlmklib ;
     "OCAMLMKTOP", c.ocamlmktop ;
     "OCAMLPROF", c.ocamlprof ;
     "CAMLP4", c.camlp4 ;
     "OCAMLFIND", c.ocamlfind ;
     "OCAMLBIN", Filename.dirname c.ocamlc;
     "OCAMLLIB", ocaml_libdir c ;
     "OCAMLVERSION", string_of_version c.version ;
   ]
   in
   List.iter (fun (var,v) -> add_subst var v) l


(*/c==m=[OCaml_conf]=0.8=t==*)

let ocaml_required = [3;12;0]
let conf = ocaml_conf ();;
print_conf conf;;

let _ =
  if conf.version << ocaml_required then
    let msg = Printf.sprintf "Error: OCaml %s required, but found %s.\n"
      (string_of_version ocaml_required) (string_of_version conf.version)
    in
    !print msg; exit 1

let _ = !print "\n### checking required tools and libraries ###\n"


let _ = check_ocamlfind_package conf ~min_version: [1;1] "xmlm";;
let _ = check_ocamlfind_package conf ~min_version: [0;9] "xtmpl";;
let _ = check_ocamlfind_package conf ~min_version: [3;6] "netstring";;
let _ = check_ocamlfind_package conf ~min_version: [1;2] "config-file";;
let _ = check_ocamlfind_package conf ~min_version: [0;2] "higlo";;

let _ =
  let js_of_ocaml =
    try ocaml_prog "js_of_ocaml"
    with Program_not_found _ ->
        prerr_endline "Cannot find js_of_ocaml";
        exit 1
  in
  add_subst "JS_OF_OCAML" js_of_ocaml
;;


let _ = !print "\n###\n"

let _ = add_conf_variables conf

let _ =
  if Array.length Sys.argv < 2 then
    !fatal_error "No output file given for substitutions!"
  else
    output_substs_to_file Sys.argv.(1)
