open! Core_kernel

(* The code here refers to c functions that are not part of the library.  Instead, at link
   time of exe, bc, and toplevels, we include an object file that implements the given
   functions.  That way, we can update the functions to include the latest version info
   without having to recompile this library and everything that depends on it. *)
external generated_build_info : unit -> string = "generated_build_info"
external generated_hg_version : unit -> string = "generated_hg_version"

let parse_generated_hg_version generated_hg_version =
  generated_hg_version
  |> String.chop_suffix_if_exists ~suffix:"\n"
  |> String.split ~on:'\n'
  |> List.map ~f:(fun line ->
    match String.rsplit2 line ~on:' ' with
    | None ->
      (* no version util, or compatibility until jenga produces the other branch *)
      line
    | Some (repo, rev_status) ->
      if String.mem rev_status '_' (* the repo has a space in it *)
      then line
      else
        (* For compability with downstream tools that might rely on this output format,
           and with [Version.parse].*)
        String.concat
          [ repo
          ; "_"
          ; String.prefix rev_status 12
          ; (* The revision can have a one-character '+' suffix. Keep it. *)
            if String.length rev_status mod 2 = 1
            then String.suffix rev_status 1
            else ""
          ])
;;

let version_list = parse_generated_hg_version (generated_hg_version ())

let version = String.concat version_list ~sep:" "
let hg_version = String.concat version_list ~sep:"\n"

let build_info = generated_build_info ()

module Version = struct
  type t =
    { repo : string
    ; version : string
    } [@@deriving sexp_of]

  let parse version =
    match String.rsplit2 version ~on:'_' with
    | None -> error_s [%message "Could not parse version" version]
    | Some (repo, version) -> Ok {repo; version}
end

let arg_spec = [
  ("-version",
   Arg.Unit
     (fun () ->
        print_endline hg_version;
        exit 0),
   " Print the hg revision of this build and exit");
  ("-build_info",
   Arg.Unit
     (fun () ->
        print_endline build_info;
        exit 0),
   " Print build info as sexp and exit");
]

module Application_specific_fields = struct
  type t = Sexp.t String.Map.t [@@deriving sexp]
end

module Time_with_limited_parsing = struct
  type t = Time.t * Sexp.t
  let t_of_sexp sexp =
    let str = string_of_sexp sexp in
    try
      match String.chop_suffix str ~suffix:"Z" with
      | None -> failwith "zone must be Z"
      | Some rest ->
        match String.lsplit2 rest ~on:' ' with
        | None -> failwith "time must contain one space between date and ofday"
        | Some (date, ofday) ->
          let date = Date.t_of_sexp (sexp_of_string date) in
          let ofday = Time.Ofday.t_of_sexp (sexp_of_string ofday) in
          Time.of_date_ofday date ofday ~zone:Time.Zone.utc, sexp
    with
    | Sexplib.Conv.Of_sexp_error (e, _) | e -> raise (Sexplib.Conv.Of_sexp_error (e, sexp))

  let sexp_of_t_ref = ref (fun (_, sexp) -> sexp)
  let sexp_of_t time = !sexp_of_t_ref time
end

type t = {
  username                    : string option [@sexp.option];
  hostname                    : string option [@sexp.option];
  kernel                      : string option [@sexp.option];
  build_time                  : Time_with_limited_parsing.t option [@sexp.option];
  x_library_inlining          : bool;
  portable_int63              : bool;
  dynlinkable_code            : bool;
  ocaml_version               : string;
  executable_path             : string;
  build_system                : string;
  allowed_projections         : string list option [@sexp.option];
  with_fdo                    : (string * Md5.t option) option [@sexp.option];
  application_specific_fields : Application_specific_fields.t option [@sexp.option];
} [@@deriving sexp]

let build_info_as_sexp =
  Exn.handle_uncaught_and_exit (fun () -> Sexp.of_string build_info)
;;

let t = Exn.handle_uncaught_and_exit (fun () -> t_of_sexp build_info_as_sexp)

let { username;
      hostname;
      kernel;
      build_time = build_time_and_sexp;
      x_library_inlining;
      portable_int63;
      dynlinkable_code;
      ocaml_version;
      executable_path;
      build_system;
      allowed_projections;
      with_fdo;
      application_specific_fields;
    } = t
;;

let build_time =
  match build_time_and_sexp with
  | None -> None
  | Some (time, _sexp) -> Some time

let reprint_build_info sexp_of_time =
  Ref.set_temporarily
    Time_with_limited_parsing.sexp_of_t_ref
    (fun (time, _) -> sexp_of_time time)
    ~f:(fun () -> Sexp.to_string (sexp_of_t t))

let compiled_for_speed = x_library_inlining && not dynlinkable_code

module For_tests = struct
  let parse_generated_hg_version = parse_generated_hg_version
end
