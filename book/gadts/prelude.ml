#require "base,core.top,async";;

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
  if not (Sys.file_exists_exn dir)
  then (
    Unix.mkdir dir;
    List.iter files ~f:(fun (name, data) ->
        Out_channel.write_all (dir ^/ name) ~data))

let () =
  create_files "bar"
    [ "a", "this is a file\nwith multiple lines"
    ; "b", "this is a file with just one line"
    ; "c", "This is also a file\nwith mutliple\n lines\n"
    ]
