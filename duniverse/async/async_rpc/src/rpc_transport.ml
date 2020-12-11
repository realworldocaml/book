open Core
open Import
module Async_reader = Reader
module Async_writer = Writer
module Kernel_transport = Rpc_kernel.Transport
module Header = Kernel_transport.Header
module Handler_result = Kernel_transport.Handler_result
module Send_result = Kernel_transport.Send_result

module With_limit : sig
  type 'a t = private
    { t : 'a
    ; max_message_size : int
    }
  [@@deriving sexp_of]

  val create : 'a -> max_message_size:int -> 'a t
  val message_size_ok : _ t -> payload_len:int -> bool
  val check_message_size : _ t -> payload_len:int -> unit
end = struct
  type 'a t =
    { t : 'a
    ; max_message_size : int
    }
  [@@deriving sexp_of]

  let create t ~max_message_size =
    if max_message_size < 0
    then
      failwithf
        "Rpc_transport.With_limit.create got negative max message size: %d"
        max_message_size
        ();
    { t; max_message_size }
  ;;

  let message_size_ok t ~payload_len =
    payload_len >= 0 && payload_len <= t.max_message_size
  ;;

  let check_message_size t ~payload_len =
    if not (message_size_ok t ~payload_len)
    then
      failwiths
        ~here:[%here]
        "Rpc_transport: message too small or too big"
        (`Message_size payload_len, `Max_message_size t.max_message_size)
        [%sexp_of: [ `Message_size of int ] * [ `Max_message_size of int ]]
  ;;
end

module Unix_reader = struct
  open With_limit

  type t = Reader.t With_limit.t [@@deriving sexp_of]

  let create ~reader ~max_message_size = With_limit.create reader ~max_message_size
  let close t = Reader.close t.t
  let is_closed t = Reader.is_closed t.t

  let all_unit_then_return l ret_val =
    match l with
    | [] -> return ret_val (* avoid deferred operations in the common case *)
    | _ -> Deferred.all_unit l >>| fun () -> ret_val
  ;;

  let read_forever t ~on_message ~on_end_of_batch =
    let finish_loop ~consumed ~need ~wait_before_reading =
      on_end_of_batch ();
      all_unit_then_return wait_before_reading (`Consumed (consumed, `Need need))
    in
    let rec loop buf ~pos ~len ~consumed ~wait_before_reading =
      if len < Header.length
      then finish_loop ~consumed ~need:Header.length ~wait_before_reading
      else (
        let payload_len = Header.unsafe_get_payload_length buf ~pos in
        let total_len = Header.length + payload_len in
        With_limit.check_message_size t ~payload_len;
        if len < total_len
        then finish_loop ~consumed ~need:total_len ~wait_before_reading
        else (
          let consumed = consumed + total_len in
          let result : _ Handler_result.t =
            on_message buf ~pos:(pos + Header.length) ~len:payload_len
          in
          match result with
          | Stop x ->
            all_unit_then_return wait_before_reading (`Stop_consumed (x, consumed))
          | Continue ->
            loop
              buf
              ~pos:(pos + total_len)
              ~len:(len - total_len)
              ~consumed
              ~wait_before_reading
          | Wait d ->
            let wait_before_reading =
              if Deferred.is_determined d
              then wait_before_reading
              else d :: wait_before_reading
            in
            loop
              buf
              ~pos:(pos + total_len)
              ~len:(len - total_len)
              ~consumed
              ~wait_before_reading))
    in
    let handle_chunk buf ~pos ~len =
      loop buf ~pos ~len ~consumed:0 ~wait_before_reading:[]
    in
    Reader.read_one_chunk_at_a_time t.t ~handle_chunk
    >>| function
    | `Eof | `Eof_with_unconsumed_data _ -> Error `Eof
    | `Stopped x -> Ok x
  ;;
end

module Unix_writer = struct
  open With_limit

  type t = Writer.t With_limit.t [@@deriving sexp_of]

  let create ~writer ~max_message_size =
    (* Prevent exceptions in the writer when the other side disconnects. Note that "stale
       data in buffer" exceptions are not an issue when the consumer leaves, since
       [Rpc_kernel.Connection] takes care of closing the transport when the consumer
       leaves. *)
    Writer.set_raise_when_consumer_leaves writer false;
    With_limit.create writer ~max_message_size
  ;;

  let close t = Writer.close t.t
  let is_closed t = Writer.is_closed t.t
  let monitor t = Writer.monitor t.t
  let bytes_to_write t = Writer.bytes_to_write t.t
  let stopped t = Deferred.any [ Writer.close_started t.t; Writer.consumer_left t.t ]
  let flushed t = Writer.flushed t.t
  let ready_to_write = flushed

  let bin_write_payload_length buf ~pos x =
    Header.unsafe_set_payload_length buf ~pos x;
    pos + Header.length
  ;;

  let send_bin_prot_internal
        t
        (bin_writer : _ Bin_prot.Type_class.writer)
        x
        ~followup_len
    : _ Send_result.t
    =
    if not (Writer.is_closed t.t)
    then (
      let data_len = bin_writer.size x in
      let payload_len = data_len + followup_len in
      if message_size_ok t ~payload_len
      then (
        Writer.write_bin_prot_no_size_header
          t.t
          ~size:Header.length
          bin_write_payload_length
          payload_len;
        Writer.write_bin_prot_no_size_header t.t ~size:data_len bin_writer.write x;
        Sent ())
      else Message_too_big { size = payload_len; max_message_size = t.max_message_size })
    else Closed
  ;;

  let send_bin_prot t bin_writer x =
    send_bin_prot_internal t bin_writer x ~followup_len:0
  ;;

  let send_bin_prot_and_bigstring t bin_writer x ~buf ~pos ~len : _ Send_result.t =
    match send_bin_prot_internal t bin_writer x ~followup_len:len with
    | Sent () ->
      Writer.write_bigstring t.t buf ~pos ~len;
      Sent ()
    | error -> error
  ;;

  let send_bin_prot_and_bigstring_non_copying t bin_writer x ~buf ~pos ~len
    : _ Send_result.t
    =
    match send_bin_prot_internal t bin_writer x ~followup_len:len with
    | Sent () ->
      Writer.schedule_bigstring t.t buf ~pos ~len;
      Sent (Writer.flushed t.t)
    | (Closed | Message_too_big _) as r -> r
  ;;
end

module Reader = struct
  include Kernel_transport.Reader

  let of_reader ~max_message_size reader =
    pack (module Unix_reader) (Unix_reader.create ~reader ~max_message_size)
  ;;
end

module Writer = struct
  include Kernel_transport.Writer

  let of_writer ~max_message_size writer =
    pack (module Unix_writer) (Unix_writer.create ~writer ~max_message_size)
  ;;
end

type t = Kernel_transport.t =
  { reader : Reader.t
  ; writer : Writer.t
  }
[@@deriving sexp_of]

let close = Kernel_transport.close

let of_reader_writer ~max_message_size reader writer =
  { reader = Reader.of_reader reader ~max_message_size
  ; writer = Writer.of_writer writer ~max_message_size
  }
;;

let of_fd ?buffer_age_limit ?reader_buffer_size ~max_message_size fd =
  of_reader_writer
    ~max_message_size
    (Async_unix.Reader.create ?buf_len:reader_buffer_size fd)
    (Async_unix.Writer.create ?buffer_age_limit fd)
;;
