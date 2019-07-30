open Core

type file_info = string * Unix.stats
type path = string list

let path_append path x = x :: path

let path_to_string ?base path =
  match base, path with
  | None, [] -> "."
  | Some base, [] -> base
  | None, _ -> String.concat ~sep:"/" (List.rev path)
  | Some base, _ -> base ^/ String.concat ~sep:"/" (List.rev path)
;;

module Options = struct
  type error_handler =
    | Ignore
    | Print
    | Raise
    | Handle_with of (string -> unit)

  type t =
    { min_depth : int
    ; max_depth : int option
    ; follow_links : bool
    ; on_open_errors : error_handler
    ; on_stat_errors : error_handler
    ; filter : (file_info -> bool) option
    ; skip_dir : (file_info -> bool) option
    ; relative_paths : bool
    }

  let default =
    { min_depth = 1
    ; max_depth = None
    ; follow_links = false
    ; on_open_errors = Raise
    ; on_stat_errors = Raise
    ; filter = None
    ; skip_dir = None
    ; relative_paths = false
    }
  ;;

  let ignore_errors = { default with on_open_errors = Ignore; on_stat_errors = Ignore }
end

module O = Options

type t =
  { base : string
  ; options : Options.t
  ;
    already_seen : (int * int, unit) Hashtbl.t
  ; (* device num * inode *)
    mutable to_visit : (path * int) list
  ; (* dir to traverse and the depth it is at *)
    mutable current_dir : path
  ; mutable current_handle : [`Just_created | `Starting | `Handle of Unix.dir_handle]
  ; mutable depth : int
  ; mutable closed : bool
  }

let full_path_name t path = path_to_string ~base:t.base path

let output_path_name t path =
  path_to_string ?base:(if t.options.O.relative_paths then None else Some t.base) path
;;

let rec open_next_dir t =
  match t.to_visit with
  | [] -> None
  | (dir_name, depth) :: rest ->
    (try
       t.to_visit <- rest;
       t.current_handle <- `Handle (Unix.opendir (full_path_name t dir_name));
       t.current_dir <- dir_name;
       t.depth <- depth;
       Some ()
     with
     | e ->
       (match t.options.O.on_open_errors with
        | O.Ignore -> open_next_dir t
        | O.Raise -> raise e
        | O.Handle_with f ->
          f (output_path_name t dir_name);
          open_next_dir t
        | O.Print ->
          Printf.eprintf !"unable to open %s - %{Exn}\n" (output_path_name t dir_name) e;
          open_next_dir t))
;;

let closedir t =
  match t.current_handle with
  | `Just_created | `Starting -> ()
  | `Handle handle ->
    (try Unix.closedir handle with
     | Unix.Unix_error _ -> ())
;;

let close t =
  if not t.closed
  then (
    t.closed <- true;
    closedir t;
    Hashtbl.clear t.already_seen;
    t.to_visit <- [])
;;

(* returns the next file from the conceptual stream and updates the state of t - this
   is the only way that t should ever be updated *)
let rec next t =
  assert (not t.closed);
  let stat path =
    let full_fn = full_path_name t path in
    let output_fn = output_path_name t path in
    try
      let stat = if t.options.O.follow_links then Unix.stat else Unix.lstat in
      Some (output_fn, path, stat full_fn)
    with
    | e ->
      (match t.options.O.on_stat_errors with
       | O.Ignore -> None
       | O.Raise -> raise e
       | O.Handle_with f ->
         f output_fn;
         None
       | O.Print ->
         Printf.eprintf !"unable to stat %s - %{Exn}\n" output_fn e;
         None)
  in
  let is_new ((_output_fn, _path, stats) as info) =
    if stats.Unix.st_kind <> Unix.S_DIR
    then Some info
    else (
      let uid = stats.Unix.st_dev, stats.Unix.st_ino in
      match Hashtbl.find t.already_seen uid with
      | Some () -> None
      | None ->
        Hashtbl.set t.already_seen ~key:uid ~data:();
        Some info)
  in
  let handle_dirs (output_fn, path, stats) =
    let info = output_fn, stats in
    if match t.options.O.skip_dir with
      | None -> false
      | Some f -> f info
    then None
    else (
      (* if this is a directory we need to decide if we will be traversing into it
         later... *)
      let visit () = t.to_visit <- (path, t.depth + 1) :: t.to_visit in
      if stats.Unix.st_kind = Unix.S_DIR
      then (
        match t.options.O.max_depth with
        | None -> visit ()
        | Some max_depth -> if t.depth < max_depth then visit () else ());
      Some info)
  in
  let filter file =
    if t.depth < t.options.O.min_depth
    then None
    else (
      match t.options.O.filter with
      | None -> Some file
      | Some f -> if f file then Some file else None)
  in
  let handle_child path =
    (* each function in this bind returns None if the file should be skipped, and
       Some f i if it thinks it's ok to emit - possibly updating the state or
       transforming f along the way *)
    let ( >>= ) t f = Option.bind t ~f in
    let skip =
      try stat path >>= is_new >>= handle_dirs >>= filter with
      | e ->
        closedir t;
        raise e
    in
    match skip with
    | None -> next t
    | file -> file
  in
  let with_next_dir k =
    match open_next_dir t with
    | None ->
      close t;
      None
    | Some () -> k ()
  in
  match t.current_handle with
  | `Just_created ->
    (match t.options.O.max_depth with
     | Some d
       when d < 0 ->
       close t;
       None
     | None | Some _ ->
       t.current_handle <- `Starting;
       handle_child t.current_dir)
  | `Starting -> with_next_dir (fun () -> next t)
  | `Handle current_handle ->
    let dirent =
      match Unix.readdir_opt current_handle with
      | Some fn -> `Dirent fn
      | None ->
        closedir t;
        `End_of_directory
    in
    (match dirent with
     | `End_of_directory -> with_next_dir (fun () -> next t)
     | `Dirent ("." | "..") -> next t
     | `Dirent basename -> handle_child (path_append t.current_dir basename))
;;

let create ?(options = Options.default) dir =
  { base = dir
  ; options
  ; already_seen = Hashtbl.Poly.create () ~size:11
  ; to_visit = []
  ; current_dir = []
  ; current_handle = `Just_created
  ; depth = 0
  ; closed = false
  }
;;

let iter t ~f =
  let rec loop () =
    match next t with
    | None -> ()
    | Some file ->
      f file;
      loop ()
  in
  loop ()
;;

let fold t ~init ~f =
  let rec loop acc =
    match next t with
    | None -> acc
    | Some file -> loop (f acc file)
  in
  loop init
;;

let to_list t = List.rev (fold t ~init:[] ~f:(fun acc file -> file :: acc))

let find_all ?options dir =
  let t = create ?options dir in
  to_list t
;;
