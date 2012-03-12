(** Dynamic load of code for bytecode version. Toplevellib being
  loaded too, there is a conflict if we use Dynlink, because
  toplevellib also contains some parts of Dynlink. *)

let load_file file =
  if not (Topdirs.load_file Format.str_formatter file) then
    (
     let msg = Format.flush_str_formatter () in
     failwith msg
    )
;;

let f files = List.iter load_file files;;

let () = Stog_dyn.load_files := f;;
