(** *)

let register_lang = Stog_intl.register_lang;;

let register_fun name f =
  Stog_html.plugin_funs := (name, f) :: !Stog_html.plugin_funs ;;

let stog () =
  match !Stog_html.current_stog with
    None -> failwith "Current stog not initialized"
  | Some x -> x
;;
