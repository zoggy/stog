(** *)

let register_lang = Stog_intl.register_lang;;

let register_fun name f =
  Stog_html.plugin_funs := (name, f) :: !Stog_html.plugin_funs ;;

let stog () =
  match !Stog_html.current_stog with
    None -> failwith "Current stog not initialized"
  | Some x -> x
;;

let set_print_verbose = Stog_msg.set_print_verbose;;
let verbose = Stog_msg.verbose;;

let set_print_warning = Stog_msg.set_print_warning;;
let warning = Stog_msg.warning;;

let set_print_error = Stog_msg.set_print_error;;
let error = Stog_msg.error;;
