open! Core
open! Async
open! Import

let%expect_test "implementation sexp is useful" =
  let rpc =
    Rpc.Rpc.create
      ~version:1
      ~name:"rpc"
      ~bin_query:Bin_prot.Type_class.bin_unit
      ~bin_response:Bin_prot.Type_class.bin_int
  in
  let implementation = Rpc.Rpc.implement rpc (fun () () -> Deferred.never ()) in
  print_s [%sexp (implementation : _ Async_rpc_kernel.Rpc.Implementation.t)];
  [%expect
    {|
    ((tag rpc) (version 1) (f rpc)
     (shapes
      ((query 86ba5df747eec837f0b391dd49f33f9e)
       (response 698cfa4093fe5e51523842d37b92aeac)))
     (on_exception ((close_connection_if_no_return_value false))))|}];
  let rpc =
    Rpc.State_rpc.create
      ~version:1
      ~name:"pipe"
      ~bin_query:Bin_prot.Type_class.bin_unit
      ~bin_error:Bin_prot.Type_class.bin_unit
      ~bin_state:Bin_prot.Type_class.bin_int
      ~bin_update:Bin_prot.Type_class.bin_string
      ()
  in
  let implementation = Rpc.State_rpc.implement rpc (fun () () -> assert false) in
  print_s [%sexp (implementation : _ Async_rpc_kernel.Rpc.Implementation.t)];
  [%expect
    {|
    ((tag pipe) (version 1) (f streaming-rpc)
     (shapes
      ((query 86ba5df747eec837f0b391dd49f33f9e)
       (initial-response 698cfa4093fe5e51523842d37b92aeac)
       (update-response d9a8da25d5656b016fb4dbdc2e4197fb)
       (error 86ba5df747eec837f0b391dd49f33f9e)))
     (on_exception ((close_connection_if_no_return_value false))))|}];
  let rpc =
    Rpc.One_way.create ~version:1 ~name:"one-way" ~bin_msg:Bin_prot.Type_class.bin_unit
  in
  let implementation = Rpc.One_way.implement rpc (fun () () -> assert false) in
  print_s [%sexp (implementation : _ Async_rpc_kernel.Rpc.Implementation.t)];
  [%expect
    {|
    ((tag one-way) (version 1) (f one-way)
     (shapes ((msg 86ba5df747eec837f0b391dd49f33f9e)))
     (on_exception ((close_connection_if_no_return_value true))))|}];
  Deferred.unit
;;
