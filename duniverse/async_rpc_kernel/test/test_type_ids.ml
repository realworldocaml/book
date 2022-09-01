open! Core
open Async_rpc.Rpc

(* The test in this module is pretty silly;

   It mainly ensures that we aren't minting a new type-id every time that
   we call the type_id getter-functions.  If the functions were implemented
   incorrectly, [same_witness_exn] would crash.

   Our type-ids also have been given names, which can be helpful for
   debugging, so the test prints out the names that have been given to
   these ids. *)

let test_type_id_similarity t ~f =
  let T = Type_equal.Id.same_witness_exn (f t) (f t) in
  print_endline (Type_equal.Id.name (f t))
;;

let rpc =
  Rpc.create
    ~version:1
    ~name:"my-rpc"
    ~bin_query:Bin_prot.Type_class.bin_unit
    ~bin_response:Bin_prot.Type_class.bin_int
;;

let state_rpc =
  State_rpc.create
    ~version:1
    ~name:"my-state-rpc"
    ~bin_query:Bin_prot.Type_class.bin_unit
    ~bin_error:Bin_prot.Type_class.bin_unit
    ~bin_state:Bin_prot.Type_class.bin_int
    ~bin_update:Bin_prot.Type_class.bin_string
    ()
;;

let pipe_rpc =
  Pipe_rpc.create
    ~version:1
    ~name:"my-pipe-rpc"
    ~bin_query:Bin_prot.Type_class.bin_unit
    ~bin_error:Bin_prot.Type_class.bin_unit
    ~bin_response:Bin_prot.Type_class.bin_int
    ()
;;

let one_way_rpc =
  One_way.create ~version:1 ~name:"my-one-way-rpc" ~bin_msg:Bin_prot.Type_class.bin_unit
;;

let%expect_test _ =
  (* rpc *)
  test_type_id_similarity rpc ~f:Rpc.query_type_id;
  test_type_id_similarity rpc ~f:Rpc.response_type_id;
  [%expect {|
    my-rpc:query
    my-rpc:response |}];
  (* state rpc *)
  test_type_id_similarity state_rpc ~f:State_rpc.query_type_id;
  test_type_id_similarity state_rpc ~f:State_rpc.error_type_id;
  test_type_id_similarity state_rpc ~f:State_rpc.state_type_id;
  test_type_id_similarity state_rpc ~f:State_rpc.update_type_id;
  [%expect
    {|
    my-state-rpc:query
    my-state-rpc:error
    my-state-rpc:state
    my-state-rpc:update |}];
  (* pipe rpc *)
  test_type_id_similarity pipe_rpc ~f:Pipe_rpc.query_type_id;
  test_type_id_similarity pipe_rpc ~f:Pipe_rpc.error_type_id;
  test_type_id_similarity pipe_rpc ~f:Pipe_rpc.response_type_id;
  [%expect {|
    my-pipe-rpc:query
    my-pipe-rpc:error
    my-pipe-rpc:response |}];
  (* one-way rpc *)
  test_type_id_similarity one_way_rpc ~f:One_way.msg_type_id;
  [%expect {| my-one-way-rpc:msg |}]
;;
