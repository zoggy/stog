
let output_dir = ref "stog-output";;

let base_url = ref None ;;
let tmpl_dir = ref None ;;

let set_stog_options stog =
  let stog =
    match !base_url with
      None -> stog
    | Some s -> { stog with Stog_types.stog_base_url = s }
  in
  let stog =
    match !tmpl_dir with
      None -> stog
    | Some s -> { stog with Stog_types.stog_tmpl_dir = s }
  in
  stog
;;

let options = [
    "-d", Arg.Set_string output_dir,
    "<dir> set output directory instead of "^ !output_dir ;

    "--base-url", Arg.String (fun s -> base_url := Some s),
    " <s> use <s> as base url instead of the one specified in the input stog" ;

    "--tmpl", Arg.String (fun s -> tmpl_dir := Some s),
    " <dir> use <dir> as template directory instead tmpl of stog dir";
  ];;

let usage = Printf.sprintf
  "Usage: %s [options] directory\nwhere options are:"
  Sys.argv.(0)
;;

let main () =
  let remain = ref [] in
  Arg.parse options (fun s -> remain := s :: !remain) usage ;
  match List.rev !remain with
    [] -> failwith usage
  | dirs ->
      let stogs = List.map Stog_io.read_stog dirs in
      prerr_endline "directories read";
      let stog = Stog_types.merge_stogs stogs in
      prerr_endline "directories merged";
      let stog = Stog_info.remove_not_published stog in
      prerr_endline "removed not published articles";
      let stog = Stog_info.compute stog in
      prerr_endline "graph computed";
      let stog = set_stog_options stog in
      Stog_html.generate !output_dir stog
;;

Stog_misc.safe_main main;;