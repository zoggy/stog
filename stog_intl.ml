(*********************************************************************************)
(*                Stog                                                           *)
(*                                                                               *)
(*    Copyright (C) 2012 Maxence Guesdon. All rights reserved.                   *)
(*                                                                               *)
(*    This program is free software; you can redistribute it and/or modify       *)
(*    it under the terms of the GNU General Public License as                    *)
(*    published by the Free Software Foundation, version 3 of the License.       *)
(*                                                                               *)
(*    This program is distributed in the hope that it will be useful,            *)
(*    but WITHOUT ANY WARRANTY; without even the implied warranty of             *)
(*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *)
(*    GNU Library General Public License for more details.                       *)
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

open Stog_types

type lang_abbrev = string

type lang_data = {
  days : string array; (* 7 *)
  months : string array; (* 12 *)
  string_of_date : date -> string;
}

let tm_of_date {year; month; day} =
  let tm = { Unix.tm_mday = day ; tm_mon = (month-1) ; tm_year = (year - 1900) ;
             tm_sec = 0 ; tm_min = 0 ; tm_hour = 0 ; tm_wday = 0 ;
             tm_yday = 0 ; tm_isdst = false ; } in
  snd (Unix.mktime tm)

let french =
  let days =
    [| "dimanche" ; "lundi" ; "mardi" ; "mercredi" ;
       "jeudi" ; "vendredi" ; "samedi" |] in
  let months = [|
    "janvier" ; "février" ; "mars" ; "avril" ; "mai" ; "juin" ;
    "juillet" ; "août" ; "septembre" ; "octobre" ; "novembre" ; "décembre" |] in
  let string_of_date ({year;month;day} as date) =
    let tm = tm_of_date date in
    Printf.sprintf "%s %d %s %d"
      days.(tm.Unix.tm_wday) day months.(month-1) year in
  { days; months; string_of_date }

let english =
  let days =
    [| "Sunday" ; "Monday" ; "Tuesday" ; "Wednesday" ;
       "Thursday" ; "Friday" ; "Saturday" |] in
  let months = [|
    "January" ; "February" ; "March" ; "April" ; "May" ; "June" ;
    "July" ; "August" ; "September" ; "October" ; "November" ; "December" |] in
  let string_of_date {year;month;day} =
    Printf.sprintf "%s %d, %d"
      months.(month-1) day year in
  { days; months; string_of_date }

let languages = ref Stog_types.Str_map.empty;;

let register_lang abbrev data =
  languages := Stog_types.Str_map.add abbrev data !languages;;

let () = register_lang "fr" french;;
let () = register_lang "en" english;;

let default_lang = ref english;;

let set_default_lang abbrev =
  try default_lang := Stog_types.Str_map.find abbrev !languages
  with Not_found ->
      failwith (Printf.sprintf "Language %S not registered" abbrev)
;;

let data_of_lang =
  let warned = Hashtbl.create 10 in
  fun lang ->
    match lang with
    | None -> !default_lang
    | Some abbrev ->
        try Stog_types.Str_map.find abbrev !languages
        with Not_found ->
            if not (Hashtbl.mem warned lang) then
              begin
                Printf.eprintf "date_of_lang: unknown lang %S, using default"
                abbrev;
                Hashtbl.add warned lang ();
              end;
            !default_lang

let get_month lang m =
  assert (m >= 1 && m <= 12);
  (data_of_lang lang).months.(m - 1)

let string_of_date lang d =
  (data_of_lang lang).string_of_date d

let string_of_date_opt lang = function
  None -> ""
| Some d -> (data_of_lang lang).string_of_date d
;;

let short_string_of_date { year; month; day } =
  Printf.sprintf "%04d/%02d/%02d" year month day

