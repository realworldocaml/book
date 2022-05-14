open! Core
open Async
open Import

(* This test must be in a file by itself because of lazy evaluation of the environment
   variable *)
let%expect_test "default_max_message_size" =
  Unix.putenv ~key:"ASYNC_RPC_MAX_MESSAGE_SIZE" ~data:"1";
  let make_transport_default_size (fd_r, fd_w) : Rpc.Transport.t =
    { reader = Reader.create fd_r |> Rpc.Transport.Reader.of_reader
    ; writer = Writer.create fd_w |> Rpc.Transport.Writer.of_writer
    }
  in
  let%map () =
    match%map
      Monitor.try_with (fun () ->
        test1
          ~make_transport:make_transport_default_size
          ~imp:[ pipe_count_imp ]
          ~state:()
          ~f:(fun _ conn -> Rpc.Pipe_rpc.dispatch_exn pipe_count_rpc conn 1)
          ())
    with
    | Error exn -> print_s ([%sexp_of: Exn.t] exn)
    | _ -> ()
  in
  [%expect
    {|
    (monitor.ml.Error
     ("Message cannot be sent"
      ((reason (Message_too_big ((size 7) (max_message_size 1))))
       (connection
        ((description <created-directly>)
         (writer
          ((t ((file_descr _) (info (writer "rpc_test 1")) (kind Fifo)))
           (max_message_size 1)))))))
     ("<backtrace elided in test>")) |}]
;;
