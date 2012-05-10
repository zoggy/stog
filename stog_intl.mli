(** Inernationalization *)

type lang_abbrev = string

(** Such a structure must be defined for each language to support. *)
type lang_data = {
  days : string array;
  months : string array;
  string_of_date : Stog_types.date -> string;
}

val tm_of_date : Stog_types.date -> Unix.tm
val french : lang_data
val english : lang_data

val register_lang : lang_abbrev -> lang_data -> unit

(** Use the given language abbreviation (such as "fr") to set the default
     language. The language must have been registered previously (except
     for predefined "en" and "fr" languages) or else the [Failure] exception
     if raised. *)
val set_default_lang : lang_abbrev -> unit

val data_of_lang : lang_abbrev option -> lang_data

val get_month : lang_abbrev option -> int -> string
val string_of_date : lang_abbrev option -> Stog_types.date -> string
val short_string_of_date : Stog_types.date -> string
