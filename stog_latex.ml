(** *)

let gensym = let cpt = ref 0 in fun () -> incr cpt; !cpt;;

let make_svg outdir latex_code =
  let tex = Filename.temp_file "stog" ".tex" in
  let code = Printf.sprintf
"\\documentclass[12pt]{article}
\\pagestyle{empty}
\\usepackage[utf8x]{inputenc}
\\begin{document}
%s
\\end{document}
"
  latex_code
  in
  let base = Filename.chop_extension tex in
  let dvi = base^".dvi" in
  let svg = Filename.concat outdir
    (Filename.basename (Printf.sprintf "_latex%d.svg" (gensym())))
  in
  Stog_misc.file_of_string ~file: tex code;
  let command = Printf.sprintf
(*    "latex -output-directory=/tmp -interaction=batchmode %s ; dvips -o %s %s && convert -units PixelsPerInch -density 100 -trim %s %s"*)
    "latex -output-directory=/tmp -interaction=batchmode %s ; dvisvgm -M 1.5 -e --no-fonts %s -s > %s"
    (Filename.quote tex) (Filename.quote dvi) (Filename.quote svg)
  in
  match Sys.command command with
    0 ->
      List.iter (fun f -> try Sys.remove f with _ -> ())
        [ tex ; dvi ];
      svg
  | n ->
    failwith (Printf.sprintf "Command failed: %s" command)
;;

let fun_latex outdir stog env args subs =
  let code =
    match subs with
      [ Xtmpl.D code ] -> code
    | _ -> failwith (Printf.sprintf "Invalid code: %s"
         (String.concat "" (List.map Xtmpl.string_of_xml subs)))
  in
  let svg = Filename.basename (make_svg outdir code) in
  let url = Printf.sprintf "%s/%s" stog.Stog_types.stog_base_url svg in
  [ Xtmpl.T ("img", ["class", "latex" ; "src", url ; "alt", code ; "title", code], []) ]
;;