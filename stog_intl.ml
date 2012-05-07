open Stog_types

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

let default_lang = ref english;;

let data_of_lang =
  let warned = Hashtbl.create 10 in
  fun lang ->
    match lang with
      | None -> !default_lang
      | Some "fr" -> french
      | Some "en" -> english
      | Some other ->
        if not (Hashtbl.mem warned lang) then begin
          Printf.eprintf "date_of_lang: unknown lang %S, using default"
            other;
          Hashtbl.add warned lang ();
        end;
        !default_lang

let get_month lang m =
  assert (m >= 1 && m <= 12);
  (data_of_lang lang).months.(m - 1)

let string_of_date lang d =
  (data_of_lang lang).string_of_date d

let short_string_of_date { year; month; day } =
  Printf.sprintf "%04d/%02d/%02d" year month day

