(* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. *)



open Lwt.Infix
open Test

let debug = false
let hello = Bytes.unsafe_of_string "Hello, World!"
let mcast_addr = "225.0.0.1"
let mcast_port = 4321

let child join fd =
  (* Lwt_unix.setsockopt fd Lwt_unix.SO_REUSEADDR true; *)
  if join then Lwt_unix.mcast_add_membership fd (Unix.inet_addr_of_string mcast_addr);
  let buf = Bytes.create 50 in
  Lwt_unix.with_timeout 0.1 (fun () -> Lwt_unix.read fd buf 0 (Bytes.length buf)) >>= fun n ->
  if debug then
    Printf.printf "\nReceived multicast message %S\n%!" (Bytes.unsafe_to_string (Bytes.sub buf 0 n));
  if Bytes.sub buf 0 n <> hello then
    Lwt.fail (Failure "unexpected multicast message")
  else
    Lwt.return_unit

let parent set_loop fd =
  Lwt_unix.mcast_set_loop fd set_loop;
  let addr = Lwt_unix.ADDR_INET (Unix.inet_addr_of_string mcast_addr, mcast_port) in
  Lwt_unix.sendto fd hello 0 (Bytes.length hello) [] addr >>= fun _ ->
  if debug then
    Printf.printf "\nSending multicast message %S to %s:%d\n%!" (Bytes.unsafe_to_string hello)
      mcast_addr mcast_port;
  Lwt.return_unit

let test_mcast name join set_loop =
  test name ~only_if:(fun () -> not Sys.win32) begin fun () ->
    let should_timeout = not join || not set_loop in
    let fd1 = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
    let fd2 = Lwt_unix.(socket PF_INET SOCK_DGRAM 0) in
    let t () =
      Lwt.catch
        (fun () ->
           Lwt_unix.(bind
             fd1 (ADDR_INET (Unix.inet_addr_any, mcast_port))) >>= fun () ->
           let t1 = child join fd1 in
           let t2 = parent set_loop fd2 in
           Lwt.join [t1; t2] >>= fun () -> Lwt.return true
        )
        (function
          | Lwt_unix.Timeout ->
            Lwt.return should_timeout
          | e ->
            Printf.eprintf "\ntest_mcast: unexpected failure: %S\n%!"
              (Printexc.to_string e);
            Lwt.return false
        )
    in
    Lwt.finalize t (fun () -> Lwt.join [Lwt_unix.close fd1; Lwt_unix.close fd2])
  end

let suite =
  suite "unix_mcast"
    [
      test_mcast "mcast-join-loop" true true;
      test_mcast "mcast-nojoin-loop" false true;
      test_mcast "mcast-join-noloop" true false;
      test_mcast "mcast-nojoin-noloop" false false;
    ]
