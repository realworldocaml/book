open Core_kernel
open Async_kernel

open Int.Replace_polymorphic_compare

include struct
  open Transport
  module Header = Header
  module Handler_result = Handler_result
  module Send_result = Send_result
  module Reader = Reader
  module Writer = Writer
end

let rec pipe_read t f =
  Pipe.values_available t
  >>= function
  | `Eof -> f `Eof
  | `Ok ->
    match Pipe.read_now' t with
    | `Nothing_available -> pipe_read t f
    | (`Eof | `Ok _) as res -> f res

module type DATA = sig
  type t [@@deriving sexp_of]
  val length : t -> int
  val add_to_bigbuffer : Bigbuffer.t -> t -> unit
  val to_bigstring : t -> Bigstring.t
  val of_bigstring : Bigstring.t -> t
end

module Pipe_and_buffer = struct
  type 'a t =
    { pipe   : 'a Pipe.Reader.t
    ; buffer : Bigbuffer.t
    } [@@deriving sexp_of]

  let create pipe =
    { pipe
    ; buffer = Bigbuffer.create Header.length
    }
  ;;
end

module Pipe_reader(Data : DATA) = struct
  type t = Data.t Pipe_and_buffer.t [@@deriving sexp_of]

  let close (t:t) = Pipe.close_read t.pipe; Deferred.unit
  let is_closed (t:t) = Pipe.is_closed t.pipe

  let read_forever (t : t) ~on_message ~on_end_of_batch : (_,_) Deferred.Result.t =
    let buffer = t.buffer in
    (* Dequeue until enough data ([need]) is available and try read messages *)
    let rec process_queue ~need queue =
      match Queue.dequeue queue with
      | None ->
        (* Queue is empty - we need to wait for more data *)
        return (Second need)
      | Some data ->
        let buff_len = Bigbuffer.length buffer in
        let data_len = Data.length data in
        if buff_len = 0 && data_len >= need
        then begin
          (* We have enough data and buffer is empty.
             This is the common case where every bin_prot message is sent in its own
             websocket frame *)
          process_data ~pos:0 ~data:(Data.to_bigstring data) ~length:data_len queue
        end
        else begin
          (* Slow path. We have to accumulate [Bigstring.t]s *)
          Data.add_to_bigbuffer buffer data;
          if Bigbuffer.length buffer >= need then begin
            (* There is enough data in the buffer *)
            let data = Bigbuffer.volatile_contents buffer in
            let length = Bigbuffer.length buffer in
            process_data ~pos:0 ~data ~length queue
          end else begin
            (* We need more data - keep processing the queue *)
            process_queue ~need queue
          end
        end
    (* Read as many messages as possible from the current data *)
    and process_data ~pos ~data ~length queue =
      if length >= Header.length then begin
        let payload_len = Header.unsafe_get_payload_length data ~pos in
        let total_len = Header.length + payload_len in
        if length >= total_len then begin
          match on_message data ~pos:(pos + Header.length) ~len:payload_len with
          | Handler_result.Stop x ->
            let pos = pos + total_len in
            let length = length - total_len in
            (* Make sure that all data we've read (and not "consumed") from the pipe
               is kept in the buffer.
               That is the remaining bits in [data] and all contents in [queue].
            *)
            set_buffer ~pos ~length ~data;
            Queue.iter queue ~f:(fun data -> Data.add_to_bigbuffer buffer data);
            return (First x)
          | Continue | Wait _ as continue_or_wait ->
            begin
              match continue_or_wait with
              | Handler_result.Stop _ ->
                (* Impossible: this branch does not contain [Stop _] *)
                assert false
              | Continue -> Deferred.unit
              | Wait d   -> d
            end
            >>= fun () ->
            let pos = pos + total_len in
            let length = length - total_len in
            assert(length >= 0);
            if length > 0
            then
              (* Process the rest of the data.
                 More than one bin_prot message was sent inside the websocket frame *)
              process_data ~pos ~data ~length queue
            else (* length = 0 *)
              (* All data was read - keep processing the queue *)
              set_buffer_and_process_queue ~need:Header.length ~pos ~length:0 ~queue ~data
        end else set_buffer_and_process_queue ~need:total_len ~pos ~length ~queue ~data
      end else set_buffer_and_process_queue ~need:Header.length ~pos ~length ~queue ~data
    and set_buffer ~pos ~length ~data =
      if length = 0
      then Bigbuffer.clear buffer
      else begin
        let data = Bigstring.sub data ~pos ~len:length in
        Bigbuffer.clear buffer;
        Bigbuffer.add_bigstring buffer data
      end;

    and set_buffer_and_process_queue ~need ~pos ~length ~queue ~data =
      set_buffer ~pos ~length ~data;
      process_queue ~need queue
    in

    let rec wait_for_read ~need =
      pipe_read t.pipe (function
        | `Eof      -> return (Error `Eof)
        | `Ok queue ->
          process_queue ~need queue >>= end_of_batch_and_continue)
    and end_of_batch_and_continue next =
      on_end_of_batch ();
      match next with
      | First  result -> return (Ok result)
      | Second need   -> wait_for_read ~need
    in
    let start_processing_existing_data ~data ~length =
      process_data
        ~pos:0
        ~data
        ~length
        (Queue.create ())
      >>= end_of_batch_and_continue
    in
    (* We either start by
       - processing remaining bits in the buffer
       - or waiting for more data in the pipe *)
    let need = Header.length in
    let length = Bigbuffer.length buffer in
    if Int.(<) length need
    then wait_for_read ~need
    else start_processing_existing_data ~length ~data:(Bigbuffer.volatile_contents buffer)

  module For_testing = struct
    type t = Data.t
    let of_bigstring = Data.of_bigstring
  end
end

module Pipe_and_monitor = struct
  type 'a t =
    { pipe    : 'a Pipe.Writer.t
    ; monitor : Monitor.t
    } [@@deriving sexp_of]

  let create pipe =
    { pipe
    ; monitor = Monitor.create ()
    }
  ;;
end

(* We don't perform any buffering here.
   A message is consider to be flushed as soon as it enters the pipe. *)
module Pipe_writer(Data : DATA) = struct

  type t = Data.t Pipe_and_monitor.t [@@deriving sexp_of]

  let close (t:t) = Pipe.close t.pipe; Deferred.unit
  let is_closed (t:t) = Pipe.is_closed t.pipe

  let monitor (t:t) = t.monitor

  (* Because we don't maintain any buffer, there are no pending writes *)
  let bytes_to_write (_ : t) = 0

  let stopped (t:t) = Pipe.closed t.pipe

  (* We consider that a message is flushed as soon as it reaches the underlining
     transport. *)
  let flushed (_ : t) = Deferred.unit

  let ready_to_write = flushed

  let sent_result x : _ Send_result.t = Sent x

  let check_closed (t:t) f =
    if not (Pipe.is_closed t.pipe) then f () else Send_result.Closed

  let send_bin_prot t writer x =
    check_closed t (fun () ->
      let buf = Bin_prot.Utils.bin_dump ~header:true writer x in
      Pipe.write_without_pushback t.pipe (Data.of_bigstring buf);
      sent_result ()
    )

  let send_bin_prot_and_bigstring t (writer : _ Bin_prot.Type_class.writer)
        x ~buf ~pos ~len:payload_size =
    check_closed t (fun () ->
      (* Write the size header manually and concatenate the two *)
      let data_size    = writer.size x in
      let data = Bigstring.create (data_size + Header.length + payload_size) in
      Header.unsafe_set_payload_length data ~pos:0 (data_size + payload_size);
      let dst_pos = writer.write data ~pos:Header.length x in
      Bigstring.blit ~src:buf ~src_pos:pos ~dst:data ~dst_pos ~len:payload_size;
      Pipe.write_without_pushback t.pipe (Data.of_bigstring data);
      sent_result ()
    )

  let send_bin_prot_and_bigstring_non_copying t writer x ~buf ~pos ~len =
    match send_bin_prot_and_bigstring t writer x ~buf ~pos ~len with
    | Sent () -> sent_result Deferred.unit
    | Closed | Message_too_big _ as r -> r
end

module Bigstring_data = struct
  type t = Bigstring.t [@@deriving sexp_of]
  let length = Bigstring.length
  let add_to_bigbuffer = Bigbuffer.add_bigstring
  let to_bigstring x = x
  let of_bigstring x = x
end

module String_data = struct
  type t = String.t [@@deriving sexp_of]
  let length = String.length
  let add_to_bigbuffer = Bigbuffer.add_string
  let to_bigstring x = Bigstring.of_string x
  let of_bigstring x = Bigstring.to_string x
end

module Bigstring_pipe_reader = Pipe_reader(Bigstring_data)
module Bigstring_pipe_writer = Pipe_writer(Bigstring_data)
module String_pipe_reader = Pipe_reader(String_data)
module String_pipe_writer = Pipe_writer(String_data)

module Kind = struct
  type 'a t =
    | String    : string t
    | Bigstring : bigstring t

  let string = String
  let bigstring = Bigstring
end

let make_reader (type a) (x : a Kind.t) (reader : a Pipe.Reader.t) =
  let reader = Pipe_and_buffer.create reader in
  match x with
  | Kind.String    -> Reader.pack (module String_pipe_reader)    reader
  | Kind.Bigstring -> Reader.pack (module Bigstring_pipe_reader) reader

let make_writer (type a) (x : a Kind.t) (writer : a Pipe.Writer.t) =
  let writer = Pipe_and_monitor.create writer in
  match x with
  | Kind.String    -> Writer.pack (module String_pipe_writer) writer
  | Kind.Bigstring -> Writer.pack (module Bigstring_pipe_writer) writer



let create kind reader writer =
  { Transport.
    reader = make_reader kind reader
  ; writer = make_writer kind writer
  }
;;

(* Testing *)

module type Transport_reader = sig
  module For_testing : sig
    type t
    val of_bigstring : Bigstring.t -> t
  end
  include Transport.Reader.S with type t = For_testing.t Pipe_and_buffer.t
end

module Test_reader (Transport_reader : Transport_reader) =
struct
  open Transport_reader.For_testing
  let default_message = Bigstring.of_string "FOOBAR123";;

  let create_message_buf (message : Bigstring.t) : Bigstring.t =
    let message_len = Bigstring.length message in
    let buf = Bigstring.create (Header.length + message_len) in
    Header.unsafe_set_payload_length buf ~pos:0 message_len;
    Bigstring.blit
      ~src:message
      ~dst:buf
      ~src_pos:0
      ~dst_pos:Header.length
      ~len:message_len;
    buf
  ;;

  let create_message () = create_message_buf default_message

  let on_message ?(message = default_message) on_message_count =
    fun buffer ~pos ~len ->
      on_message_count := !on_message_count + 1;
      [%test_result: Bigstring.t] (Bigstring.sub buffer ~pos ~len) ~expect:message;
      Handler_result.Continue
  ;;

  let on_end_of_batch on_end_of_batch_count () = incr on_end_of_batch_count

  let wait () = Async_kernel_scheduler.run_cycles_until_no_jobs_remain ()

  let run_test run verify =
    let reader, writer = Pipe.create () in
    let reader = Pipe_and_buffer.create reader in
    let on_message_count = ref 0 in
    let on_end_of_batch_count = ref 0 in
    let on_message = on_message on_message_count in
    let on_end_of_batch = on_end_of_batch on_end_of_batch_count in
    ignore(Transport_reader.read_forever reader ~on_message ~on_end_of_batch);
    wait ();
    run (fun x ->
      Pipe.write_without_pushback writer (of_bigstring x)
    );
    wait ();
    verify ~on_message_count ~on_end_of_batch_count
  ;;

  let verify
        ~on_message_count ~on_end_of_batch_count
        message_count     batch_count =
    [%test_result: int] (!on_message_count) ~expect:message_count;
    [%test_result: int] (!on_end_of_batch_count) ~expect:batch_count
  ;;

  let write_1_msg_1_chunk write = write (create_message ())

  (* Not enough data, we need to accumulate *)
  let write_1_msg_3_chunk write =
    let buf = create_message_buf (Bigstring.of_string "") in
    let message_raw = default_message in
    Header.unsafe_set_payload_length buf ~pos:0 (Bigstring.length message_raw);
    write buf;
    wait ();
    let msg_len  = Bigstring.length message_raw in
    let split_at = msg_len / 2 in
    let part1    = Bigstring.sub message_raw ~pos:0 ~len:split_at in
    let part2    = Bigstring.sub message_raw ~pos:split_at ~len:(msg_len - split_at) in
    write part1;
    wait ();
    write part2
  ;;

  (* Too much data, we need to loop withing the same batch *)
  let write_2_msg_1_chunk write =
    let one_msg = create_message_buf default_message in
    let two_msg = Bigstring.concat [one_msg;one_msg] in
    write two_msg
  ;;

  let write_2_msg_one_byte_at_a_time_34_chunk write =
    let one_msg = create_message_buf default_message in
    let two_msg = Bigstring.concat [one_msg;one_msg] in
    for i = 0 to Bigstring.length two_msg - 1 do
      write (Bigstring.sub two_msg ~pos:i ~len:1);
      wait ()
    done
  ;;

  let%test_unit _ = run_test write_1_msg_1_chunk (verify 1 1)

  let%test_unit _ =
    let f write =
      write_1_msg_1_chunk write;
      write_1_msg_1_chunk write
    in
    run_test f (verify 2 1)

  let%test_unit _ =
    run_test write_1_msg_3_chunk (verify 1 3)

  let%test_unit _ =
    run_test write_2_msg_1_chunk (verify 2 1)

  let%test_unit _ =
    run_test write_2_msg_one_byte_at_a_time_34_chunk (verify 2 34)

  let%test_unit _ =
    let f write =
      write (create_message ());
      wait ();

      write_1_msg_3_chunk write;
      wait ();

      write_2_msg_1_chunk write;
      wait ();

      write_2_msg_one_byte_at_a_time_34_chunk write;
      wait ();

      write (create_message ());
    in
    run_test f (verify 7 40)


  let%test_unit "can call read_forever multiple times" =
    let reader, writer = Pipe.create () in
    let reader = Pipe_and_buffer.create reader in
    let on_end_of_batch () = () in
    let write x = Pipe.write_without_pushback writer (of_bigstring x) in
    let num = ref 0 in
    let stop_after_on_message =
      fun buffer ~pos ~len ->
        incr num;
        [%test_result: Bigstring.t] (Bigstring.sub buffer ~pos ~len) ~expect:default_message;
        Handler_result.Stop ()
    in
    write_2_msg_1_chunk write;
    Pipe.close writer;
    don't_wait_for (
      Transport_reader.read_forever reader ~on_message:stop_after_on_message ~on_end_of_batch
      >>= fun x ->
      assert (Result.is_ok x);
      Transport_reader.read_forever reader ~on_message:stop_after_on_message ~on_end_of_batch
      >>= fun x ->
      assert (Result.is_ok x);
      Deferred.unit
    );
    wait ();
    [%test_result: Int.t] ~expect:2 !num


end

let%test_module "Test_reader_string"    = (module Test_reader(String_pipe_reader))

let%test_module "Test_reader_bigstring" = (module Test_reader(Bigstring_pipe_reader))

module Bench_reader (Transport_reader : Transport_reader) =
struct
  open Transport_reader.For_testing
  let default_message = Bigstring.of_string "FOOBAR123";;

  let create_message_buf (message : Bigstring.t) : Bigstring.t =
    let message_len = Bigstring.length message in
    let buf = Bigstring.create (Header.length + message_len) in
    Header.unsafe_set_payload_length buf ~pos:0 message_len;
    Bigstring.blit
      ~src:message
      ~dst:buf
      ~src_pos:0
      ~dst_pos:Header.length
      ~len:message_len;
    buf
  ;;

  let create_message () = create_message_buf default_message
  let one_message = create_message ()
  let two_messages = Bigstring.concat [one_message;one_message]

  let one_message_p1, one_message_p2 =
    let msg_len  = Bigstring.length one_message in
    let split_at = msg_len / 2 in
    let part1    = Bigstring.sub one_message ~pos:0 ~len:split_at in
    let part2    = Bigstring.sub one_message ~pos:split_at ~len:(msg_len - split_at) in
    part1, part2

  let on_message _buffer ~pos:_ ~len:_ = Handler_result.Continue
  let on_end_of_batch () = ()

  let wait () = Async_kernel_scheduler.run_cycles_until_no_jobs_remain ()

  let run_test run =
    let reader, writer = Pipe.create () in
    let reader = Pipe_and_buffer.create reader in
    let on_message = on_message in
    let on_end_of_batch = on_end_of_batch in
    ignore(Transport_reader.read_forever reader ~on_message ~on_end_of_batch);
    wait ();
    for _ = 0 to 100 do
      run (fun x ->
        Pipe.write_without_pushback writer (of_bigstring x)
      );
    done;
    wait ()
  ;;

  let write_1_msg_1_chunk write = write one_message

  (* Not enough data, we need to accumulate *)
  let buf = create_message_buf (Bigstring.of_string "")
  let write_1_msg_3_chunk write =
    Header.unsafe_set_payload_length buf ~pos:0 (Bigstring.length one_message);
    write buf;
    write one_message_p1;
    write one_message_p2
  ;;

  (* Too much data, we need to loop withing the same batch *)
  let write_2_msg_1_chunk write = write two_messages
  ;;

  let write_2_msg_one_byte_at_a_time_34_chunk write =
    for i = 0 to Bigstring.length two_messages - 1 do
      write (Bigstring.sub two_messages ~pos:i ~len:1);
    done
  ;;

  let%bench "write_1_msg_1_chunk" = run_test write_1_msg_1_chunk

  let%bench "write_2_msg_1_chunk" =
    let f write =
      write_1_msg_1_chunk write;
      write_1_msg_1_chunk write
    in
    run_test f

  let%bench "write_1_msg_3_chunk" =
    run_test write_1_msg_3_chunk

  let%bench "write_2_msg_1_chunk" =
    run_test write_2_msg_1_chunk

  let%bench "write_2_msg_one_byte" =
    run_test write_2_msg_one_byte_at_a_time_34_chunk

  let%bench "multi" =
    let f write =
      write (create_message ());
      write_1_msg_3_chunk write;
      write_2_msg_1_chunk write;
      write_2_msg_one_byte_at_a_time_34_chunk write;
      write (create_message ());
    in
    run_test f
end

let%bench_module "Test_reader_string"    = (module Bench_reader(String_pipe_reader))

let%bench_module "Test_reader_bigstring" = (module Bench_reader(Bigstring_pipe_reader))
