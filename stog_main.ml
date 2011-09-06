
let options = [];;

let usage = Printf.sprintf
  "Usage: %s [options] directory\nwhere options are:"
  Sys.argv.(0)
;;

let main () =
  let remain = ref [] in
  Arg.parse options (fun s -> remain := s :: !remain) usage ;
  match List.rev !remain with
    [] -> failwith usage
  | dir :: _ ->
      let stog = Stog_io.read_stog dir in
      ignore(stog)
;;

Stog_misc.safe_main main;;