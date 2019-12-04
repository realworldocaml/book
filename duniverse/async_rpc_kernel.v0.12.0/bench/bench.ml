open Core
open Async

let max_message_size = 16 lsl 20

let config =
  Rpc.Low_latency_transport.Config.create
    (* Always batch, we don't care about measuring the syscall time here *)
    ~start_batching_after_num_messages:0
    ()

let rpc =
  Rpc.Pipe_rpc.create
    ~name:"test"
    ~version:42
    ~bin_query:[%bin_type_class: unit]
    ~bin_response:[%bin_type_class: int * int]
    ~bin_error:[%bin_type_class: unit]
    ()

let rpc_big =
  Rpc.Pipe_rpc.create
    ~name:"test-big"
    ~version:42
    ~bin_query:[%bin_type_class: unit]
    ~bin_response:[%bin_type_class: int list]
    ~bin_error:[%bin_type_class: unit]
    ()

let make rpc =
  let pipe_reader, pipe_writer = Pipe.create () in
  let impl =
    Rpc.Pipe_rpc.implement_direct rpc
      (fun () () writer ->
         Pipe.write_without_pushback pipe_writer writer;
         return (Ok ()))
  in
  (impl, fun () ->
     Pipe.read pipe_reader >>| function
     | `Eof -> assert false
     | `Ok x -> x)

let implementation, next_direct_writer = make rpc
let implementation_big, next_direct_writer_big = make rpc_big

let implementations =
  Rpc.Implementations.create_exn
    ~implementations:[implementation; implementation_big]
    ~on_unknown_rpc:`Raise

let create fdr fdw =
  let reader = Rpc.Low_latency_transport.Reader.create fdr ~max_message_size ~config in
  let writer = Rpc.Low_latency_transport.Writer.create fdw ~max_message_size ~config in
  Async_rpc_kernel.Rpc.Connection.create
    ~implementations ~connection_state:ignore
    { reader; writer }
  >>| Result.ok_exn

let client, server, server_to_client_fdw =
  Thread_safe.block_on_async_exn (fun () ->
    let%bind (`Reader c2s_fdr, `Writer c2s_fdw) =
      Unix.pipe (Info.of_string "client->server")
    in
    let%bind (`Reader s2c_fdr, `Writer s2c_fdw) =
      Unix.pipe (Info.of_string "server->client")
    in
    let%map client = create s2c_fdr c2s_fdw
    and     server = create c2s_fdr s2c_fdw
    in
    (client, server, s2c_fdw))

let new_direct_writer rpc next_direct_writer =
  Thread_safe.block_on_async_exn (fun () ->
    let%map () =
      match%map Rpc.Pipe_rpc.dispatch_iter rpc client () ~f:(fun _ -> Continue) with
      | Ok _    -> ()
      | Error _ -> assert false
    and direct_writer = next_direct_writer () in
    direct_writer)

let direct_writer = new_direct_writer rpc next_direct_writer

let direct_writers_big =
  Array.init 100 ~f:(fun _ -> new_direct_writer rpc_big next_direct_writer_big)
let group1 = Rpc.Pipe_rpc.Direct_stream_writer.Group.create ()
let group10 = Rpc.Pipe_rpc.Direct_stream_writer.Group.create ()
let group100 = Rpc.Pipe_rpc.Direct_stream_writer.Group.create ()
let groups =
  [ 1   , group1
  ; 10  , group10
  ; 100 , group100
  ]
let () =
  List.iter groups ~f:(fun (n, group) ->
    for i = 0 to n - 1 do
      Rpc.Pipe_rpc.Direct_stream_writer.Group.add_exn group direct_writers_big.(i)
    done)
;;

let () =
  (* Now replace the server fd by /dev/null, so that writes always succeed *)
  let fd = Fd.file_descr_exn server_to_client_fdw in
  let open Core in
  let devnull = Unix.openfile ~mode:[O_WRONLY] "/dev/null" in
  Unix.dup2 ~src:devnull ~dst:fd;
  Unix.close devnull
;;

(* [stop_inlining] makes sure the compiler knows nothing about [data] *)
external stop_inlining : 'a -> 'a = "async_rpc_kernel_bench_identity"
let data = stop_inlining (42, 0x1234_5678)
let bin_writer_int_int = [%bin_writer: int * int]

let buf = Bigstring.create 8192
let%bench "direct write" =
  assert (
    Rpc.Pipe_rpc.Direct_stream_writer.write_without_pushback direct_writer
      data
    = `Ok)
;;

let%bench "direct write expert" =
  let len = bin_writer_int_int.write buf ~pos:0 data in
  assert (
    Rpc.Pipe_rpc.Direct_stream_writer.Expert.write_without_pushback direct_writer
      ~buf ~pos:0 ~len
    = `Ok)
;;

let big_data = stop_inlining (List.init 1000 ~f:(fun x -> x * 1000))
let bin_writer_int_list = [%bin_writer: int list]

let%bench "direct write (big)" =
  assert (
    Rpc.Pipe_rpc.Direct_stream_writer.write_without_pushback direct_writers_big.(0)
      big_data
    = `Ok)
;;


let%bench "direct write expert (big)" =
  let len = bin_writer_int_list.write buf ~pos:0 big_data in
  assert (
    Rpc.Pipe_rpc.Direct_stream_writer.Expert.write_without_pushback direct_writers_big.(0)
      ~buf ~pos:0 ~len
    = `Ok)
;;

let%bench "iter direct write (big)" [@indexed n = [1; 10; 100]] =
  for i = 0 to n - 1 do
    assert (
      Rpc.Pipe_rpc.Direct_stream_writer.write_without_pushback direct_writers_big.(i)
        big_data
      = `Ok)
  done
;;

let%bench_fun "direct write to group (big)" [@indexed n = [1; 10; 100]] =
  let group = List.Assoc.find_exn groups ~equal:Int.equal n in
  fun () ->
    Rpc.Pipe_rpc.Direct_stream_writer.Group.write_without_pushback group big_data
;;
