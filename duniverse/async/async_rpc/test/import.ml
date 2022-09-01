open Core
open Async

let () = Backtrace.elide := true
let max_message_size = 1_000_000

let test ~make_transport ~imp1 ~imp2 ~state1 ~state2 ~f () =
  let%bind `Reader r1, `Writer w2 = Unix.pipe (Info.of_string "rpc_test 1") in
  let%bind `Reader r2, `Writer w1 = Unix.pipe (Info.of_string "rpc_test 2") in
  let t1 = make_transport (r1, w1) in
  let t2 = make_transport (r2, w2) in
  let s imp =
    if List.length imp > 0
    then
      Some
        (Rpc.Implementations.create_exn
           ~implementations:imp
           ~on_unknown_rpc:`Close_connection)
    else None
  in
  let s1 = s imp1 in
  let s2 = s imp2 in
  let conn1_ivar = Ivar.create () in
  let f2_done =
    Async_rpc_kernel.Rpc.Connection.with_close
      ?implementations:s2
      t2
      ~dispatch_queries:(fun conn2 ->
        let%bind conn1 = Ivar.read conn1_ivar in
        f conn1 conn2)
      ~connection_state:(fun _ -> state2)
      ~on_handshake_error:`Raise
  in
  Async_rpc_kernel.Rpc.Connection.with_close
    ?implementations:s1
    t1
    ~dispatch_queries:(fun conn1 ->
      Ivar.fill conn1_ivar conn1;
      f2_done)
    ~connection_state:(fun _ -> state1)
    ~on_handshake_error:`Raise
;;

let test1 ~make_transport ~imp ~state ~f =
  test ~make_transport ~imp1:imp ~state1:state ~imp2:[] ~state2:() ~f
;;

module Pipe_count_error = struct
  type t = [ `Argument_must_be_positive ] [@@deriving bin_io]
end

let pipe_count_rpc =
  Rpc.Pipe_rpc.create
    ~name:"pipe_count"
    ~version:0
    ~bin_query:Int.bin_t
    ~bin_response:Int.bin_t
    ~bin_error:Pipe_count_error.bin_t
    ()
;;

let pipe_wait_rpc =
  Rpc.Pipe_rpc.create
    ~name:"pipe_wait"
    ~version:0
    ~bin_query:Unit.bin_t
    ~bin_response:Unit.bin_t
    ~bin_error:Unit.bin_t
    ()
;;

let pipe_count_imp =
  Rpc.Pipe_rpc.implement pipe_count_rpc (fun () n ->
    if n < 0
    then return (Error `Argument_must_be_positive)
    else (
      let pipe_r, pipe_w = Pipe.create () in
      upon
        (Deferred.List.iter (List.init n ~f:Fn.id) ~how:`Sequential ~f:(fun i ->
           Pipe.write pipe_w i))
        (fun () -> Pipe.close pipe_w);
      return (Ok pipe_r)))
;;

let pipe_wait_imp ivar =
  Rpc.Pipe_rpc.implement pipe_wait_rpc (fun () () ->
    let pipe_r, pipe_w = Pipe.create () in
    (Pipe.write pipe_w ()
     >>> fun () ->
     Ivar.read ivar >>> fun () -> Pipe.write pipe_w () >>> fun () -> Pipe.close pipe_w);
    return (Ok pipe_r))
;;
