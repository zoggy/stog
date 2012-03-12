(** *)

let (load_files : (string list -> unit) ref) =
  ref (fun _ -> prerr_endline "Stog_dyn.load_files not initialized"; exit 1);;

