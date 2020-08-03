module Stable0 = struct
  open Core.Core_stable

  module Inet_port = struct
    module V1 = struct
      module T = struct
        type t = int [@@deriving compare, equal, hash]

        let of_int_exn x =
          if x > 0 && x < 65536
          then x
          else failwith (Core.sprintf "%d is not a valid port number." x)
        ;;

        let to_int x = x

        include Sexpable.Of_sexpable.V1
            (Int.V1)
            (struct
              type nonrec t = t

              let of_sexpable = of_int_exn
              let to_sexpable = to_int
            end)

        include Binable.Of_binable.V1 [@alert "-legacy"]
            (Int.V1)
            (struct
              type nonrec t = t

              let of_binable = of_int_exn
              let to_binable = to_int
            end)

        include (val Comparator.V1.make ~compare ~sexp_of_t)

        let%expect_test _ =
          print_string [%bin_digest: t];
          [%expect {| 698cfa4093fe5e51523842d37b92aeac |}]
        ;;
      end

      include T
      include Comparable.V1.Make (T)
    end
  end
end

open Core
open Poly
open Unix

external raw_fork_exec
  :  stdin:File_descr.t
  -> stdout:File_descr.t
  -> stderr:File_descr.t
  -> ?working_dir:string
  -> ?setuid:int
  -> ?setgid:int
  -> ?env:string array
  -> string
  -> string array
  -> Pid.t
  = "extended_ml_spawn_bc" "extended_ml_spawn"

let raw_fork_exec ~stdin ~stdout ~stderr ?working_dir ?setuid ?setgid ?env prog argv =
  (* [spawn] is generally preferred: it seems better tested and more actively maintained.
     It also uses [vfork] so it's more efficient. For now we still must fall back to
     [extended_ml_spawn] for the case when [setuid] or [setgid] is requested,
     but we should completely switch to [spawn] when/if it supports that. *)
  match setuid, setgid with
  | None, None ->
    let env = Option.map ~f:(fun env -> Spawn.Env.of_list (Array.to_list env)) env in
    let cwd =
      Option.value_map
        ~default:Spawn.Working_dir.Inherit
        ~f:(fun cwd -> Path cwd)
        working_dir
    in
    let argv = Array.to_list argv in
    Pid.of_int (Spawn.spawn ?env ~cwd ~prog ~argv ~stdin ~stdout ~stderr ())
  | Some _, _ | _, Some _ ->
    raw_fork_exec ~stdin ~stdout ~stderr ?working_dir ?setuid ?setgid ?env prog argv
;;

module Env = struct
  open String.Map

  type t = string String.Map.t

  let empty : t = empty

  let get () =
    Array.fold (Unix.environment ()) ~init:empty ~f:(fun env str ->
      match String.lsplit2 ~on:'=' str with
      | Some (key, data) -> set ~key ~data env
      | None ->
        failwithf "extended_unix.Env.get %S is not in the form of key=value" str ())
  ;;

  let add ~key ~data env =
    if String.mem key '='
    then
      failwithf
        "extended_unix.Env.add:variable to export in the environment %S contains an \
         equal sign"
        key
        ()
    else if String.mem key '\000'
    then
      failwithf
        "extended_unix.Env.add:variable to export in the environment %S contains an \
         null character"
        key
        ()
    else if String.mem data '\000'
    then
      failwithf
        "extended_unix.Env.add:value (%S) to export in the environment for %S contains \
         an null character"
        data
        key
        ()
    else String.Map.set ~key ~data env
  ;;

  let to_string_array env =
    String.Map.to_alist env |> List.map ~f:(fun (k, v) -> k ^ "=" ^ v) |> List.to_array
  ;;
end

let fork_exec
      ?(stdin = Unix.stdin)
      ?(stdout = Unix.stdout)
      ?(stderr = Unix.stderr)
      ?(path_lookup = true)
      ?env
      ?working_dir
      ?setuid
      ?setgid
      prog
      args
  =
  let env =
    Option.map env ~f:(fun e ->
      let init, l =
        match e with
        | `Extend l -> Env.get (), l
        | `Replace l -> Env.empty, l
      in
      List.fold_left l ~init ~f:(fun env (key, data) -> Env.add ~key ~data env)
      |> Env.to_string_array)
  and full_prog =
    if path_lookup
    then (
      match Shell_internal.which prog with
      | Some s -> s
      | None -> failwithf "fork_exec: Process not found %s" prog ())
    else prog
  in
  raw_fork_exec
    ~stdin
    ~stdout
    ~stderr
    ?working_dir
    ?setuid
    ?setgid
    ?env
    full_prog
    (Array.of_list (prog :: args))
;;

external seteuid : int -> unit = "extended_ml_seteuid"
external setreuid : uid:int -> euid:int -> unit = "extended_ml_setreuid"
external htonl : Int32.t -> Int32.t = "extended_ml_htonl"
external ntohl : Int32.t -> Int32.t = "extended_ml_ntohl"

let%test _ = htonl (ntohl 0xdeadbeefl) = 0xdeadbeefl

type statvfs =
  { bsize : int (** file system block size *)
  ; frsize : int (** fragment size *)
  ; blocks : int (** size of fs in frsize units *)
  ; bfree : int (** # free blocks *)
  ; bavail : int (** # free blocks for non-root *)
  ; files : int (** # inodes *)
  ; ffree : int (** # free inodes *)
  ; favail : int (** # free inodes for non-root *)
  ; fsid : int (** file system ID *)
  ; flag : int (** mount flags *)
  ; namemax : int (** maximum filename length *)
  }
[@@deriving sexp, bin_io]

(** get file system statistics *)
external statvfs : string -> statvfs = "statvfs_stub"

(** get load averages *)
external getloadavg : unit -> float * float * float = "getloadavg_stub"

module Extended_passwd = struct
  open Passwd

  let of_passwd_line_exn s =
    match String.split s ~on:':' with
    | [ name; passwd; uid; gid; gecos; dir; shell ] ->
      { name
      ; passwd
      ; uid = Int.of_string uid
      ; gid = Int.of_string gid
      ; gecos
      ; dir
      ; shell
      }
    | _ -> failwithf "of_passwd_line: failed to parse: %s" s ()
  ;;

  let of_passwd_line s = Option.try_with (fun () -> of_passwd_line_exn s)

  let of_passwd_file_exn fn =
    Exn.protectx
      (In_channel.create fn)
      ~f:(fun chan -> List.map (In_channel.input_lines chan) ~f:of_passwd_line_exn)
      ~finally:In_channel.close
  ;;

  let of_passwd_file f = Option.try_with (fun () -> of_passwd_file_exn f)
end

let strptime = Core.Unix.strptime

module Inet_port = struct
  module Stable = Stable0.Inet_port

  module T = struct
    type t = int [@@deriving compare, equal, hash]
    type comparator_witness = Stable.V1.comparator_witness

    let comparator = Stable.V1.comparator
    let sexp_of_t = Stable.V1.sexp_of_t
  end

  include T

  let of_int_exn = Stable.V1.of_int_exn

  let of_int x =
    try Some (of_int_exn x) with
    | _ -> None
  ;;

  let of_string_exn x = Int.of_string x |> of_int_exn

  let of_string x =
    try Some (of_string_exn x) with
    | _ -> None
  ;;

  let to_string x = Int.to_string x
  let to_int x = x
  let arg_type = Command.Spec.Arg_type.create of_string_exn

  include Comparable.Make_plain_using_comparator (T)
end

let%test _ = Inet_port.of_string "88" = Some 88
let%test _ = Inet_port.of_string "2378472398572" = None
let%test _ = Inet_port.of_int 88 = Some 88
let%test _ = Inet_port.of_int 872342 = None

module Mac_address = struct
  (* An efficient internal representation would be something like a 6 byte array,
     but let's use a hex string to get this off the ground. *)
  module T = struct
    type t = string [@@deriving sexp, bin_io, compare, hash]

    let ( = ) = String.( = )
    let equal = ( = )

    let of_string s =
      let addr =
        String.lowercase s
        |> String.filter ~f:(function
          | 'a' .. 'f' | '0' .. '9' -> true
          | _ -> false)
      in
      let length = String.length addr in
      if length <> 12
      then failwithf "MAC address '%s' has the wrong length: %d" s length ();
      addr
    ;;

    let to_string t =
      let rec loop acc = function
        | a :: b :: rest ->
          let x = String.of_char_list [ a; b ] in
          loop (x :: acc) rest
        | [] -> List.rev acc |> String.concat ~sep:":"
        | _ -> assert false
      in
      loop [] (String.to_list t)
    ;;

    let to_string_cisco t =
      let lst = String.to_list t in
      let a = List.take lst 4 |> String.of_char_list
      and b = List.take (List.drop lst 4) 4 |> String.of_char_list
      and c = List.drop lst 8 |> String.of_char_list in
      String.concat ~sep:"." [ a; b; c ]
    ;;

    let t_of_sexp sexp = String.t_of_sexp sexp |> of_string
    let sexp_of_t t = to_string t |> String.sexp_of_t
    let _flag = Command.Spec.Arg_type.create of_string
  end

  include T
  include Hashable.Make (T)
end

let%test _ =
  Mac_address.to_string (Mac_address.of_string "00:1d:09:68:82:0f") = "00:1d:09:68:82:0f"
;;

let%test _ =
  Mac_address.to_string (Mac_address.of_string "00-1d-09-68-82-0f") = "00:1d:09:68:82:0f"
;;

let%test _ =
  Mac_address.to_string (Mac_address.of_string "001d.0968.820f") = "00:1d:09:68:82:0f"
;;

let%test _ =
  Mac_address.to_string_cisco (Mac_address.of_string "00-1d-09-68-82-0f")
  = "001d.0968.820f"
;;

module Quota = struct
  type bytes = Int63.t [@@deriving sexp]
  type inodes = Int63.t [@@deriving sexp]

  let bytes x = x
  let inodes x = x

  type 'units limit =
    { soft : 'units option [@sexp.option]
    ; hard : 'units option [@sexp.option]
    ; grace : Time.t option [@sexp.option]
    }
  [@@deriving sexp]

  type 'units usage = private 'units

  (* None is encoded as zero *)
  type 'units c_limit =
    { c_soft : 'units
    ; c_hard : 'units
    ; c_grace : Time.t
    }

  let zero_bytes = bytes Int63.zero
  let zero_inodes = inodes Int63.zero

  let ml_limit_of_c_limit ~zero { c_soft; c_hard; c_grace } =
    { soft = (if c_soft = zero then None else Some c_soft)
    ; hard = (if c_hard = zero then None else Some c_hard)
    ; grace = (if c_grace = Time.epoch then None else Some c_grace)
    }
  ;;

  let c_limit_of_ml_limit ~zero { soft; hard; grace } =
    { c_soft =
        (match soft with
         | None -> zero
         | Some x -> x)
    ; c_hard =
        (match hard with
         | None -> zero
         | Some x -> x)
    ; c_grace =
        (match grace with
         | None -> Time.epoch
         | Some x -> x)
    }
  ;;

  external quota_query
    :  [ `User | `Group ]
    -> id:int
    -> path:string
    -> bytes c_limit * bytes usage * inodes c_limit * inodes usage
    = "quota_query"

  external quota_modify
    :  [ `User | `Group ]
    -> id:int
    -> path:string
    -> bytes c_limit
    -> inodes c_limit
    -> unit
    = "quota_modify"

  let query user_or_group ~id ~path =
    try
      let blimit, busage, ilimit, iusage = quota_query user_or_group ~id ~path in
      Ok
        ( ml_limit_of_c_limit ~zero:zero_bytes blimit
        , busage
        , ml_limit_of_c_limit ~zero:zero_inodes ilimit
        , iusage )
    with
    | Unix.Unix_error _ as exn -> Or_error.of_exn exn
  ;;

  let set user_or_group ~id ~path byte_limit inode_limit =
    try
      Ok
        (quota_modify
           user_or_group
           ~id
           ~path
           (c_limit_of_ml_limit ~zero:zero_bytes byte_limit)
           (c_limit_of_ml_limit ~zero:zero_inodes inode_limit))
    with
    | Unix.Unix_error _ as exn -> Or_error.of_exn exn
  ;;
end

module Mount_entry = struct
  (* see: man 3 getmntent *)
  type t =
    { fsname : string
    ; directory : string
    ; fstype : string
    ; options : string
    ; dump_freq : int option [@sexp.option]
    ; fsck_pass : int option [@sexp.option]
    }
  [@@deriving sexp, fields]

  let escape_seqs = [ "040", " "; "011", "\t"; "012", "\n"; "134", "\\"; "\\", "\\" ]

  let unescape s =
    let find_and_drop_prefix s (prefix, replacement) =
      Option.map (String.chop_prefix ~prefix s) ~f:(fun s -> replacement, s)
    in
    let rec loop s =
      match String.lsplit2 s ~on:'\\' with
      | None -> [ s ]
      | Some (l, r) ->
        (match List.find_map escape_seqs ~f:(find_and_drop_prefix r) with
         | None -> l :: "\\" :: loop r
         | Some (x, r) -> l :: x :: loop r)
    in
    String.concat (loop s)
  ;;

  let parse_optional_int s =
    match Int.of_string s with
    | 0 -> None
    | n -> Some n
  ;;

  let split_and_normalize line =
    let inside_comment = ref false in
    let whitespace = ' ' in
    String.map line ~f:(fun x ->
      if Char.equal x '#' then inside_comment := true;
      if Char.is_whitespace x || !inside_comment then whitespace else x)
    |> String.split ~on:whitespace
    |> List.filter ~f:(fun x -> not (String.is_empty x))
  ;;

  let parse_line line =
    match split_and_normalize line |> List.map ~f:unescape with
    | [] -> Ok None
    | fsname
      :: directory
      :: fstype :: options :: (([] | [ _ ] | [ _; _ ]) as dump_freq_and_fsck_pass) ->
      let dump_freq, fsck_pass =
        match dump_freq_and_fsck_pass with
        | [] -> None, None
        | [ dump_freq ] -> Some dump_freq, None
        | [ dump_freq; fsck_pass ] -> Some dump_freq, Some fsck_pass
        | _ -> assert false
      in
      Or_error.try_with (fun () ->
        let dump_freq = Option.bind dump_freq ~f:parse_optional_int in
        let fsck_pass = Option.bind fsck_pass ~f:parse_optional_int in
        if String.equal fstype "ignore"
        then None
        else Some { fsname; directory; fstype; options; dump_freq; fsck_pass })
    | _ -> Or_error.error "wrong number of fields" line String.sexp_of_t
  ;;

  let visible_filesystem ts =
    let add_slash_if_needed s = if String.is_suffix s ~suffix:"/" then s else s ^ "/" in
    let overlay map t =
      let remove_prefix = add_slash_if_needed (directory t) in
      let rec loop map =
        match String.Map.closest_key map `Greater_than remove_prefix with
        | None -> map
        | Some (key, _) ->
          if not (String.is_prefix ~prefix:remove_prefix key)
          then map
          else loop (String.Map.remove map key)
      in
      String.Map.set (loop map) ~key:(directory t) ~data:t
    in
    List.fold ts ~init:String.Map.empty ~f:(fun map t ->
      if not (String.is_prefix ~prefix:"/" (directory t)) then map else overlay map t)
  ;;
end

let terminal_width =
  lazy
    ((* When both stdout and stderr are not terminals, tput outputs 80 rather than the
        number of columns, so we can't use [Process.run].  Instead, we use
        [open_process_in] so that stderr is still the terminal.  But, we don't want
        tput's error messages to be sent to stderr and seen by the user, so we first
        run tput with no output to see if it succeeds, and only then do we run it with
        stderr not redirected. *)
      try
        Exn.protectx
          (Core.Unix.open_process_in
             "/usr/bin/tput cols &> /dev/null && /usr/bin/tput cols")
          ~f:(fun in_channel ->
            In_channel.input_line in_channel |> Option.value_exn |> Int.of_string)
          ~finally:In_channel.close
      with
      | _ -> 90)
;;
