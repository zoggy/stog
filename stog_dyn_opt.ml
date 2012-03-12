(** Dynamic load of code for native version. *)

let f files =
  try
    Dynlink.allow_unsafe_modules true ;
    List.iter Dynlink.loadfile files
    with Dynlink.Error e ->
      failwith (Dynlink.error_message e)
;;

let () = Stog_dyn.load_files := f;;
