(** *)

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

let error ?info msg =
  let msg = Printf.sprintf "Error: %s%s"
    (match info with None -> "" | Some s -> Printf.sprintf "[%s]" s)
    msg
  in
  incr errors;
  !print_error msg
;;

let set_print_error = (:=) print_error;;
let errors () = !errors;;

