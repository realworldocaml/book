#require "base,core.top,async,core_unix,core_unix.sys_unix";;
#require "ppx_jane";;

let () = Printexc.record_backtrace false

let handle_state_changes _ _ = Async.Deferred.return ()

module User_id : sig
  type t
end = struct type t = unit end

module User_name : sig
  type t
end = struct type t = unit end

module Permissions : sig
  type t
  val check : t -> User_id.t -> bool
end = struct
  type t = unit
  let check _ _ = false
end

let create_files dir files =
  let open Core in
  if not (Sys_unix.file_exists_exn dir)
  then (
    Core_unix.mkdir dir;
    List.iter files ~f:(fun (name, data) ->
        Out_channel.write_all (dir ^/ name) ~data))

let () =
  create_files "src"
    [ "server.ml", "open Core\nopen! Async\type t ="
    ; "server.mli", ""
    ; "parser.ml", "open Base\n\ntype t ="
    ; "dune", "(library\n (name foo)"
    ]
