(** *)

let to_utf8 s =
  try
    Glib.Convert.convert
      ~to_codeset: "UTF-8" ~from_codeset: "ISO-8859-1" s
  with
    _ -> s

let of_utf8 ?coding s =
  try
    Glib.Convert.convert
      ~from_codeset: "UTF-8" ~to_codeset: "ISO-8859-1" s
  with _ ->
      s
