open Core_kernel
open Async_kernel

module Header = struct
  let length = 8

  let unsafe_get_payload_length buf ~pos =
    Bigstring.unsafe_get_int64_le_exn buf ~pos
  ;;

  let unsafe_set_payload_length buf ~pos payload_len =
    Bigstring.unsafe_set_int64_le buf ~pos payload_len
  ;;
end

module Handler_result = Transport_intf.Handler_result

module Reader = struct
  module type S = Transport_intf.Reader

  type t = T : (module S with type t = 'a) * 'a -> t

  let pack m t = T (m, t)

  (* We put type annotations to be sure the type is not a function type, i.e. to avoid
     creating closures *)
  let sexp_of_t (T ((module M), t)) : Sexp.t          = M.sexp_of_t t
  let close     (T ((module M), t)) : unit Deferred.t = M.close     t
  let is_closed (T ((module M), t)) : bool            = M.is_closed t

  let read_forever (T ((module M), t)) ~on_message ~on_end_of_batch : _ Deferred.t =
    M.read_forever t ~on_message ~on_end_of_batch

  let read_one_message_bin_prot t
        (bin_reader : _ Bin_prot.Type_class.reader) =
    read_forever t
      ~on_message:(fun buf ~pos ~len ->
        let pos_ref = ref pos in
        let x = bin_reader.read buf ~pos_ref in
        if !pos_ref <> pos + len then
          failwithf "message length (%d) did not match expected length (%d)"
            (!pos_ref - pos) len ()
        else
          Stop x)
      ~on_end_of_batch:ignore
  ;;
end

module Send_result = Transport_intf.Send_result

module Writer = struct
  module type S = Transport_intf.Writer

  type 'a writer =
    { impl    : (module S with type t = 'a)
    ; t       : 'a
    (* We cache the result of [stopped] because it is often the [Deferred.any] of several
       other deferreds and we want [can_send] to be simple. *)
    ; stopped : unit Deferred.t
    }

  type t = T : 'a writer -> t

  let pack (type a) (module M : S with type t = a) t =
    T { impl = (module M)
      ; t
      ; stopped = M.stopped t
      }
  ;;

  let sexp_of_t      (T { impl=(module M); t; _ }) : Sexp.t          = M.sexp_of_t      t
  let close          (T { impl=(module M); t; _ }) : unit Deferred.t = M.close          t
  let is_closed      (T { impl=(module M); t; _ }) : bool            = M.is_closed      t
  let monitor        (T { impl=(module M); t; _ }) : Monitor.t       = M.monitor        t
  let bytes_to_write (T { impl=(module M); t; _ }) : int             = M.bytes_to_write t
  let flushed        (T { impl=(module M); t; _ }) : unit Deferred.t = M.flushed        t
  let ready_to_write (T { impl=(module M); t; _ }) : unit Deferred.t = M.ready_to_write t

  let send_bin_prot (T { impl=(module M); t; _ }) bin_writer x : _ Send_result.t =
    M.send_bin_prot t bin_writer x
  let send_bin_prot_and_bigstring (T { impl = (module M); t; _ }) bin_writer x
        ~buf ~pos ~len : _ Send_result.t =
    M.send_bin_prot_and_bigstring t bin_writer x ~buf ~pos ~len
  let send_bin_prot_and_bigstring_non_copying (T { impl = (module M); t; _ }) bin_writer x
        ~buf ~pos ~len : _ Send_result.t =
    M.send_bin_prot_and_bigstring_non_copying t bin_writer x ~buf ~pos ~len

  let stopped  (T { stopped; _ }) = stopped
  let can_send (T { impl = (module M); t; stopped }) =
    not (M.is_closed t || Deferred.is_determined stopped)
  ;;

  let transfer t ?(max_num_values_per_read=1_000) pipe f =
    let consumer =
      Pipe.add_consumer pipe
        ~downstream_flushed:(fun () -> flushed t >>| fun () -> `Ok)
    in
    let end_of_pipe =
      Deferred.create (fun ivar ->
        let rec iter () =
          if can_send t then begin
            match
              Pipe.read_now' pipe ~consumer ~max_queue_length:max_num_values_per_read
            with
            | `Ok q ->
              Queue.iter q ~f;
              Pipe.Consumer.values_sent_downstream consumer;
              ready_to_write t >>> iter
            | `Nothing_available ->
              Pipe.values_available pipe
              >>> fun (`Ok | `Eof)  -> iter ()
            | `Eof -> Ivar.fill ivar ()
          end
        in
        iter ())
    in
    Deferred.any
      [ end_of_pipe
      ; stopped t
      ]
    >>| fun () ->
    Pipe.close_read pipe
  ;;
end

type t =
  { reader : Reader.t
  ; writer : Writer.t
  }
[@@deriving sexp_of]

let close t =
  Writer.close t.writer
  >>= fun () ->
  Reader.close t.reader
;;
