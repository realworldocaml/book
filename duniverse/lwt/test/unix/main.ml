(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



let is_fd_open fd_ =
  let fd  = (Obj.magic (int_of_string fd_) : Unix.file_descr) in
  let buf = Bytes.create 42 in
    try
      ignore (Unix.read fd buf 0 42);
      true
    with Unix.Unix_error(Unix.EBADF, _, _) ->
      false

let () =
  try
    assert (not @@ is_fd_open @@ Unix.getenv Test_lwt_unix.assert_fd_closed);
    exit 0
  with Not_found -> ()

let () =
  try
    assert (is_fd_open @@ Unix.getenv Test_lwt_unix.assert_fd_open);
    exit 0
  with Not_found -> ()

let () =
  Test.run "unix" [
    Test_lwt_unix.suite;
    Test_lwt_io.suite;
    Test_lwt_io_non_block.suite;
    Test_lwt_process.suite;
    Test_lwt_engine.suite;
    Test_mcast.suite;
    Test_lwt_fmt.suite;
    Test_lwt_timeout.suite;
    Test_lwt_bytes.suite;
    Test_sleep_and_timeout.suite;
  ]
