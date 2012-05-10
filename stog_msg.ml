(** *)

let print_verbose = ref print_endline;;
let verbose_level = ref 0;;

let verbose ?info ?(level=1) msg =
  if level <= !verbose_level then
    begin
      let msg = Printf.sprintf "Warning: %s%s"
        (match info with None -> "" | Some s -> Printf.sprintf "[%s]" s)
        msg
      in
      !print_verbose msg
    end
;;
let set_print_verbose = (:=) print_verbose;;
let set_verbose_level = (:=) verbose_level;;
let incr_verbose_level () = incr verbose_level;;
let verbose_level () = !verbose_level ;;

let warnings = ref 0;;
let print_warning = ref prerr_endline;;

let warning ?info msg =
  let msg = Printf.sprintf "Warning: %s%s"
    (match info with None -> "" | Some s -> Printf.sprintf "[%s]" s)
    msg
  in
  incr warnings;
  !print_warning msg
;;

let set_print_warning = (:=) print_warning;;
let warnings () = !warnings;;

let errors = ref 0;;
let print_error = ref prerr_endline;;

let error ?info ?fatal msg =
  let msg = Printf.sprintf "Error: %s%s"
    (match info with None -> "" | Some s -> Printf.sprintf "[%s]" s)
    msg
  in
  incr errors;
  !print_error msg;
  match fatal with  None -> () | Some n -> exit n
;;

let set_print_error = (:=) print_error;;
let errors () = !errors;;

