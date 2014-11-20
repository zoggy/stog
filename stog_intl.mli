(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012-2014 INRIA All rights reserved.                         *)
(*    Author: Maxence Guesdon, INRIA Saclay                                      *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU General Public License for more details.                               *)
(*                                                                               *)
(*    You should have received a copy of the GNU General Public                  *)
(*    License along with this program; if not, write to the Free Software        *)
(*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA                   *)
(*    02111-1307  USA                                                            *)
(*                                                                               *)
(*    As a special exception, you have permission to link this program           *)
(*    with the OCaml compiler and distribute executables, as long as you         *)
(*    follow the requirements of the GNU GPL in regard to all of the             *)
(*    software in the executable aside from the OCaml compiler.                  *)
(*                                                                               *)
(*    Contact: Maxence.Guesdon@inria.fr                                          *)
(*                                                                               *)
(*********************************************************************************)

(** Inernationalization *)

type lang_abbrev = string

(** Such a structure must be defined for each language to support. *)
type lang_data = {
    days : string array;
    months : string array;
    string_of_date : Stog_types.date -> string;
    string_of_datetime : Stog_types.date -> string;
}

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
val string_of_date_opt : lang_abbrev option -> Stog_types.date option -> string

val string_of_datetime : lang_abbrev option -> Stog_types.date -> string
val string_of_datetime_opt : lang_abbrev option -> Stog_types.date option -> string

val short_string_of_date : Stog_types.date -> string
