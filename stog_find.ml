
(*c==m=[File.Find]=0.1=t==*)

    open Unix

    type filter =
	Maxdepth of int
      | Type of Unix.file_kind
      | Follow
      | Regexp of Str.regexp
      | Atime of interval
      | Predicate of (string -> bool)
    and interval =
	Le of int | Eq of int | Ge of int

    type mode =
      | Ignore
      | Stderr
      | Failure
      | Custom of (Unix.error * string * string -> unit)

    (* To memorize visited inodes *)
    type inode = int * int
    let inode st = st.st_dev, st.st_ino

    (* parameters driving the find *)
    type status =
	{ maxdepth : int;
	  follow : bool;
	  filters : (string -> stats -> bool) list;
	  stat_function : string -> stats;
	    action : string -> unit;
	      handler : (error * string * string -> unit)
	}

    exception Hide of exn
    (* Used to hide user-level errors, so that they are no trap by the library *)
    let hide_exn f x = try f x with exn -> raise (Hide exn)
    let reveal_exn f x = try f x with Hide exn -> raise exn

    let stderr_handler (e, b, c) =
      prerr_endline ("find: " ^ c ^": " ^ (error_message e))
    let ignore_handler _ = ()
    let failure_handler (e,b,c) = raise (Hide (Unix_error (e, b, c)))
    let handler = function
	Stderr -> stderr_handler
      | Ignore -> ignore_handler
      | Failure -> failure_handler
      | Custom h -> hide_exn h

    (* handlers of errors during the call. *)
    let treat_unix_error h f x =
      try f x with  Unix_error (e, b, c) ->  h (e, b, c)

    let default_status =
      { follow = false;
	maxdepth = max_int;
	filters = [];
	stat_function = lstat;
	action = prerr_endline;
	handler = handler Stderr;
      }

    let add_filter status f = { status with filters = f :: status.filters }

    let seconds_in_a_day = 86400.
    exception Find of string
    let rec parse_option status = function
      | Maxdepth n ->
	  { status with maxdepth = n }
      | Type k ->
	  add_filter status
            (fun name stat -> stat.st_kind = k)
      | Follow ->
	  { status with follow = true }
      | Regexp exp ->
	  add_filter status
            (fun name stat ->
              Str.string_match exp name 0 &&
              Str.match_beginning () = 0 &&
              Str.match_end () = String.length name
            )

      | Atime n ->
	  let min, max =
            match n with
            | Eq d when d > 0 ->
		float d *. seconds_in_a_day, float (d-1) *. seconds_in_a_day
            | Le d when d > 0 ->
		min_float, float (d-1) *. seconds_in_a_day
            | Le d when d > 0 ->
		min_float, float (d-1) *. seconds_in_a_day
            | Ge d when d > 0 ->
		float (d) *. seconds_in_a_day, max_float
            | _ -> raise (Find "Ill_formed argument")
	  in
	  let now = time() in
	  add_filter status
            (fun name stat ->
              let time = now -. stat.st_atime in min <= time && time <= max)
      | Predicate f ->
	  add_filter status (fun name stat -> f name)

    let parse_options options =
      List.fold_left parse_option default_status options

    (* fonctions auxilaires *)

    let filter_all filename filestat filters =
      List.for_all (fun f -> f filename filestat) filters

    let iter_dir f d =
      let dir_handle = opendir d in
      try while true do f (readdir dir_handle) done with
	End_of_file -> closedir dir_handle
      | x -> closedir dir_handle; raise x

    (* fonction principale seconde version *)
    let rec find_rec status visited depth filename =
      let find() =
	let filestat =
	  if status.follow then stat filename else lstat filename in
	let id = filestat.st_dev, filestat.st_ino in
	if filter_all filename filestat status.filters then status.action filename;
	if filestat.st_kind = S_DIR && depth < status.maxdepth &&
	  (not status.follow || not (List.mem id visited))
	then
	  let process_child child =
            if (child <> Filename.current_dir_name &&
		child <> Filename.parent_dir_name) then
              let child_name = Filename.concat filename child in
              let visited = if status.follow then id :: visited else visited in
              find_rec status visited (depth+1) child_name
	  in
          (* process_child is recursively protected from errors *)
	  iter_dir process_child filename
      in
      treat_unix_error status.handler find ()

    let find_entry status filename = find_rec status [] 0 filename

    let find mode filenames options action =
      let status =
	{ (parse_options options) with
	  handler = handler mode;
	  action = hide_exn action }
      in
      reveal_exn (List.iter (find_entry status)) filenames

    let find_list mode filenames options =
      let l = ref [] in
      find mode filenames options (fun s -> l := s :: !l);
      List.rev !l


  
(*/c==m=[File.Find]=0.1=t==*)

