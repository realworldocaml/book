open Core_kernel
open Poly
open Async_kernel
open Util

open Implementation_types.Implementations

module P = Protocol
module Reader = Transport.Reader
module Writer = Transport.Writer

(* The Result monad is also used. *)
let (>>|~) = Result.(>>|)

(* Commute Result and Deferred. *)
let defer_result : 'a 'b. ('a Deferred.t,'b) Result.t -> ('a,'b) Result.t Deferred.t =
  function
  | Error _ as err -> return err
  | Ok d ->
    match Deferred.peek d with
    | None -> d >>| fun x -> Ok x
    | Some d -> return (Ok d)

module Deferred_immediate = struct
  let ( >>= ) d f =
    match Deferred.peek d with
    | None -> d >>= f
    | Some x -> f x

  (* We may not be using this at a particular point in time, but we still want [>>=] to be
     rebound when opening [Deferred_immediate] in case we do start using it later. *)
  let _ = ( >>= )

  let ( >>| ) d f =
    match Deferred.peek d with
    | None -> d >>| f
    | Some x -> return (f x)

  let _ = ( >>| )
end

module Responder = Implementation.Expert.Responder

type 'connection_state on_unknown_rpc =
  [ `Raise
  | `Continue
  | `Close_connection
  | `Call of
      ('connection_state
       -> rpc_tag : string
       -> version : int
       -> [ `Close_connection | `Continue ])
  ]

type 'connection_state on_unknown_rpc_with_expert =
  [ 'connection_state on_unknown_rpc
  | `Expert of
      ('connection_state
       -> rpc_tag : string
       -> version : int
       -> Responder.t
       -> Bigstring.t
       -> pos : int
       -> len : int
       -> unit Deferred.t)
  ]

type 'connection_state t = 'connection_state Implementation_types.Implementations.t =
  { implementations : 'connection_state Implementation.F.t Description.Table.t
  ; on_unknown_rpc  : 'connection_state on_unknown_rpc_with_expert
  }

type 'connection_state implementations = 'connection_state t

let descriptions t = Hashtbl.keys t.implementations

module Instance = struct
  type streaming_response = Instance.streaming_response =
    | Pipe : _ Pipe.Reader.t -> streaming_response
    | Direct :
        (_ Implementation_types.Direct_stream_writer.t [@sexp.opaque]) -> streaming_response
  [@@deriving sexp_of]

  type streaming_responses =
    (P.Query_id.t, streaming_response) Hashtbl.t [@@deriving sexp_of]

  type 'a unpacked = 'a Instance.unpacked =
    { implementations          : ('a implementations [@sexp.opaque])
    ; writer                   : Writer.t
    ; open_streaming_responses : streaming_responses
    ; mutable stopped          : bool
    ; connection_state         : 'a
    ; connection_description   : Info.t
    ; connection_close_started : Info.t Deferred.t
    ; mutable last_dispatched_implementation :
        (Description.t * ('a Implementation.F.t [@sexp.opaque])) option
    (* [packed_self] is here so we can essentially pack an unpacked instance without doing
       any additional allocation. *)
    ; packed_self              : (t [@sexp.opaque])
    } [@@deriving sexp_of]

  and t = Instance.t = T : _ unpacked -> t
  let sexp_of_t (T t) = [%sexp_of: _ unpacked] t

  let send_write_error t id sexp =
    let data : _ P.Message.t =
      Response { id; data = Error (Write_error sexp) }
    in
    match Writer.send_bin_prot t.writer P.Message.bin_writer_nat0_t data with
    | Sent () | Closed -> ()
    | Message_too_big _ as r ->
      raise_s [%sexp
        "Failed to send write error to client",
        { error  = (sexp : Sexp.t)
        ; reason = (r : unit Transport.Send_result.t)
        }
      ]
  ;;

  let handle_send_result t id (result : _ Transport.Send_result.t) =
    match result with
    | Sent () -> ()
    | Closed  -> ()
    | Message_too_big _ as r ->
      send_write_error t id ([%sexp_of: unit Transport.Send_result.t] r)
  ;;

  let write_message t ~id bin_writer x =
    if not t.stopped
    then
      Writer.send_bin_prot t.writer bin_writer x
      |> handle_send_result t id
  ;;

  let write_message_expert t ~id bin_writer x ~buf ~pos ~len =
    if not t.stopped
    then
      Writer.send_bin_prot_and_bigstring t.writer bin_writer x ~buf ~pos ~len
      |> handle_send_result t id
  ;;

  let write_response t id bin_writer_data data =
    let bin_writer =
      P.Message.bin_writer_needs_length (Writer_with_length.of_writer bin_writer_data)
    in
    write_message t ~id bin_writer (Response { id; data })
  ;;

  module Cached_stream_writer : sig
    type instance
    type 'a t = 'a Implementation_types.Cached_stream_writer.t

    val create
      :  id:P.Query_id.t
      -> bin_writer:'a Bin_prot.Type_class.writer
      -> 'a t

    val write        : 'a t -> instance -> P.Query_id.t -> 'a -> unit
    val write_expert : 'a t -> instance -> P.Query_id.t -> buf:Bigstring.t -> pos:int -> len:int -> unit
    val write_string : 'a t -> instance -> P.Query_id.t -> string -> unit
  end with type instance := t = struct
    type 'a t = 'a Implementation_types.Cached_stream_writer.t =
      { header_prefix    : string (* Bin_protted constant prefix of the message *)
      ; (* Length of the user data part. We set this field when sending a message. This
           relies on the fact that the message is serialized immediately (which is the
           only acceptable semantics for the transport layer anyway, as it doesn't know if
           the value is mutable or not).

           [data_len] is passed to bin-prot writers by mutating [data_len] instead of by
           passing an additional argument to avoid some allocation.
        *)
        mutable data_len : Nat0.t
      ; bin_writer       : 'a Bin_prot.Type_class.writer
      }


    type void = Void
    let bin_size_void Void = 0
    let bin_write_void _buf ~pos Void = pos

    type void_message = void P.Message.needs_length
    [@@deriving bin_write]

    type void_stream_response_data = void P.Stream_response_data.needs_length
    [@@deriving bin_write]

    (* This is not re-entrant but Async code always runs on one thread at a time *)
    let buffer = Bigstring.create 32
    let cache_bin_protted (bin_writer : _ Bin_prot.Type_class.writer) x =
      let len = bin_writer.write buffer ~pos:0 x in
      Bigstring.To_string.sub buffer ~pos:0 ~len
    ;;

    let create (type a) ~id ~bin_writer : a t =
      let header_prefix =
        cache_bin_protted bin_writer_void_message (Response { id; data = Ok Void })
      in
      { header_prefix
      ; bin_writer
      ; data_len = Nat0.of_int_exn 0
      }
    ;;

    (* This part of the message header is a constant, make it a literal to make the
       writing code slightly faster. *)
    let stream_response_data_header_len      = 4
    let stream_response_data_header_as_int32 = 0x8a79l

    let%test_unit "stream_response_* constants are correct" =
      let len =
        bin_writer_void_stream_response_data.write buffer ~pos:0
          (`Ok Void : void_stream_response_data)
      in
      assert (len = stream_response_data_header_len);
      assert (Bigstring.unsafe_get_int32_t_le buffer ~pos:0
              = stream_response_data_header_as_int32);
    ;;

    let bin_write_string_no_length buf ~pos str =
      let str_len = String.length str in
      (* Very low-level bin_prot stuff... *)
      Bin_prot.Common.assert_pos pos;
      let next = pos + str_len in
      Bin_prot.Common.check_next buf next;
      Bin_prot.Common.unsafe_blit_string_buf ~src_pos:0 str ~dst_pos:pos buf
        ~len:str_len;
      next
    ;;

    (* The two following functions are used by the 3 variants exposed by this module. They
       serialize a [Response { id; data = Ok (`Ok data_len) }] value, taking care of
       writing the [Nat0.t] length prefix where approriate.

       Bear in mind that there are two levels of length prefixes for stream response data
       message: one for the user data (under the `Ok, before the actual data), and one for
       the response data (under the .data field, before the Ok). *)
    let bin_size_nat0_header { header_prefix; data_len; _ } =
      let stream_response_data_nat0_len =
        stream_response_data_header_len + Nat0.bin_size_t data_len
      in
      let stream_response_data_len =
        stream_response_data_nat0_len + (data_len : Nat0.t :> int)
      in
      String.length header_prefix
      + Nat0.bin_size_t (Nat0.of_int_exn stream_response_data_len)
      + stream_response_data_nat0_len

    let bin_write_nat0_header buf ~pos { header_prefix; data_len; _ } =
      let pos = bin_write_string_no_length buf ~pos header_prefix in
      let stream_response_data_len =
        stream_response_data_header_len
        + Nat0.bin_size_t data_len
        + (data_len : Nat0.t :> int)
      in
      let pos = Nat0.bin_write_t buf ~pos (Nat0.of_int_exn stream_response_data_len) in
      let next = pos + 4 in
      Bin_prot.Common.check_next buf next;
      Bigstring.unsafe_set_int32_t_le buf ~pos stream_response_data_header_as_int32;
      Nat0.bin_write_t buf ~pos:next data_len

    let bin_writer_nat0_header : _ Bin_prot.Type_class.writer =
      { size  = bin_size_nat0_header
      ; write = bin_write_nat0_header
      }

    let bin_size_message (t, _) =
      bin_size_nat0_header t + (t.data_len : Nat0.t :> int)

    let bin_write_message buf ~pos (t, data) =
      let pos = bin_write_nat0_header buf ~pos t in
      t.bin_writer.write buf ~pos data

    let bin_writer_message : _ Bin_prot.Type_class.writer =
      { size  = bin_size_message
      ; write = bin_write_message
      }

    let bin_size_message_as_string (t, _) =
      bin_size_nat0_header t + (t.data_len : Nat0.t :> int)

    let bin_write_message_as_string buf ~pos (t, str) =
      let pos = bin_write_nat0_header buf ~pos t in
      bin_write_string_no_length buf ~pos str

    let bin_writer_message_as_string : _ Bin_prot.Type_class.writer =
      { size  = bin_size_message_as_string
      ; write = bin_write_message_as_string
      }

    (* [write] and [write_string] both allocate 3 words for the tuples. [write_expert]
       does not allocate. *)
    let write t (T instance) id data =
      t.data_len <- Nat0.of_int_exn (t.bin_writer.size data);
      write_message instance ~id bin_writer_message (t, data)

    let write_string t (T instance) id str =
      t.data_len <- Nat0.of_int_exn (String.length str);
      write_message instance ~id bin_writer_message_as_string (t, str)

    let write_expert t (T instance) id ~buf ~pos ~len =
      t.data_len <- Nat0.of_int_exn len;
      write_message_expert instance ~id bin_writer_nat0_header t
        ~buf ~pos ~len
  end

  module Direct_stream_writer = struct
    module T = Implementation_types.Direct_stream_writer

    module State = T.State

    module Id = T.Id

    type 'a t = 'a T.t =
      { id            : Id.t
      ; mutable state : 'a State.t
      ; closed        : unit Ivar.t
      ; instance      : Instance.t
      ; query_id      : P.Query_id.t
      ; stream_writer : 'a Cached_stream_writer.t
      ; groups        : 'a group_entry Bag.t
      }

    and 'a group_entry = 'a T.group_entry =
      { group            : 'a T.Group.t
      ; element_in_group : 'a t Bag.Elt.t
      }

    let is_closed t = Ivar.is_full t.closed
    let closed t = Ivar.read t.closed
    let flushed t =
      let (T instance) = t.instance in
      Transport.Writer.flushed instance.writer
    ;;

    let bin_writer t = t.stream_writer.bin_writer

    let write_eof {instance = T instance; query_id; _} =
      write_response instance query_id
        P.Stream_response_data.bin_writer_nat0_t
        (Ok `Eof)
    ;;

    let write_message {instance; stream_writer; query_id; _} x =
      Cached_stream_writer.write stream_writer instance query_id x
    ;;

    let write_message_string {instance; stream_writer; query_id; _} x =
      Cached_stream_writer.write_string stream_writer instance query_id x
    ;;

    let write_message_expert {instance; stream_writer; query_id; _} ~buf ~pos ~len =
      Cached_stream_writer.write_expert stream_writer instance query_id ~buf ~pos ~len
    ;;

    let close_without_removing_from_instance t =
      if not (Ivar.is_full t.closed) then begin
        Ivar.fill t.closed ();
        let groups = t.groups in
        if not (Bag.is_empty groups) then
          Async_kernel_scheduler.Private.Very_low_priority_work.enqueue
            ~f:(fun () ->
              match Bag.remove_one groups with
              | None -> Finished
              | Some { group; element_in_group } ->
                Bag.remove group.components element_in_group;
                Hashtbl.remove group.components_by_id t.id;
                Not_finished);
        match t.state with
        | Not_started _ -> ()
        | Started -> write_eof t
      end
    ;;

    let close ({instance = T instance; query_id; _} as t) =
      close_without_removing_from_instance t;
      Hashtbl.remove instance.open_streaming_responses query_id
    ;;

    let write_without_pushback t x =
      if Ivar.is_full t.closed then
        `Closed
      else begin
        begin match t.state with
        | Not_started q -> Queue.enqueue q (Normal x)
        | Started -> write_message t x
        end;
        `Ok
      end
    ;;

    let write ({instance = T instance; _} as t) x =
      match write_without_pushback t x with
      | `Closed -> `Closed
      | `Ok -> `Flushed (Writer.flushed instance.writer)
    ;;

    module Expert = struct
      let write_without_pushback t ~buf ~pos ~len =
        if Ivar.is_full t.closed then
          `Closed
        else begin
          begin match t.state with
          | Not_started q ->
            Queue.enqueue q (Expert (Bigstring.To_string.sub buf ~pos ~len))
          | Started -> write_message_expert t ~buf ~pos ~len
          end;
          `Ok
        end
      ;;

      let write ({instance = T instance; _} as t) ~buf ~pos ~len =
        match write_without_pushback t ~buf ~pos ~len with
        | `Closed -> `Closed
        | `Ok -> `Flushed (Writer.flushed instance.writer)
      ;;
    end

    let start t =
      match t.state with
      | Started -> failwith "attempted to start writer which was already started"
      | Not_started q ->
        t.state <- Started;
        Queue.iter q ~f:(function
          | Normal x -> write_message        t x
          | Expert x -> write_message_string t x);
        if Ivar.is_full t.closed then write_eof t;
    ;;
  end

let apply_implementation
      t
      implementation
      ~(query : Nat0.t P.Query.t)
      ~read_buffer
      ~read_buffer_pos_ref
  : _ Transport.Handler_result.t =
  let id = query.id in
  match implementation with
  | Implementation.F.One_way (bin_query_reader, f) ->
    let query_contents =
      bin_read_from_bigstring bin_query_reader read_buffer ~pos_ref:read_buffer_pos_ref
        ~len:query.data
        ~location:"server-side one-way rpc message un-bin-io'ing"
    in
    (match query_contents with
     | Error _ as err ->
       Stop err
     | Ok q ->
       try
         f t.connection_state q;
         Continue
       with exn ->
         Stop
           (Rpc_result.uncaught_exn exn
              ~location:"server-side one-way rpc computation")
    )
  | Implementation.F.One_way_expert f ->
    (try
       let len = (query.data :> int) in
       f t.connection_state read_buffer ~pos:!read_buffer_pos_ref ~len;
       read_buffer_pos_ref := !read_buffer_pos_ref + len;
       Continue
     with exn ->
       Stop
         (Rpc_result.uncaught_exn exn
            ~location:"server-side one-way rpc expert computation")
    )
  | Implementation.F.Rpc (bin_query_reader, bin_response_writer, f, result_mode) ->
    let query_contents =
      bin_read_from_bigstring bin_query_reader read_buffer
        ~pos_ref:read_buffer_pos_ref
        ~len:query.data
        ~location:"server-side rpc query un-bin-io'ing"
    in
    begin match result_mode with
    | Implementation.F.Blocking ->
      let data =
        try query_contents >>|~ f t.connection_state with
        | exn ->
          (* In the [Deferred] branch we use [Monitor.try_with], which includes
             backtraces when it catches an exception. For consistency, we also get
             backtraces here. *)
          let backtrace = Backtrace.Exn.most_recent () in
          let sexp =
            [%sexp
              { location = "server-side blocking rpc computation"
              ; exn = (exn : exn)
              ; backtrace = (backtrace : Backtrace.t)
              }]
          in
          Error (Rpc_error.Uncaught_exn sexp)
      in
      write_response t id bin_response_writer data
    | Implementation.F.Deferred ->
      let data =
        Rpc_result.try_with ~run:`Now
          ~location:"server-side rpc computation" (fun () ->
            defer_result (query_contents >>|~ f t.connection_state))
      in
      (* In the common case that the implementation returns a value immediately, we will
         write the response immediately as well (this is also why the above [try_with]
         has [~run:`Now]).  This can be a big performance win for servers that get many
         queries in a single Async cycle. *)
      ( match Deferred.peek data with
        | None -> data >>> write_response t id bin_response_writer
        | Some data -> write_response t id bin_response_writer data );
    end;
    Continue
  | Implementation.F.Rpc_expert (f, result_mode) ->
    let responder = Implementation.Expert.Responder.create query.id t.writer in
    let d =
      (* We need the [Monitor.try_with] even for the blocking mode as the implementation
         might return [Delayed_reponse], so we don't bother optimizing the blocking
         mode. *)
      Monitor.try_with ~run:`Now (fun () ->
        let len = (query.data :> int) in
        let result =
          f t.connection_state responder read_buffer
            ~pos:!read_buffer_pos_ref ~len
        in
        match result_mode with
        | Implementation.F.Deferred -> result
        | Implementation.F.Blocking -> Deferred.return result
      )
    in
    let handle_exn exn =
      let result =
        Rpc_result.uncaught_exn exn ~location:"server-side rpc expert computation"
      in
      if responder.responded then
        result
      else begin
        write_response t id bin_writer_unit result;
        Ok ()
      end
    in
    let check_responded () =
      if responder.responded
      then Ok ()
      else handle_exn (Failure "Expert implementation did not reply")
    in
    let d =
      let open Deferred_immediate in
      d >>| function
      | Ok result ->
        let d =
          match result with
          | Replied -> Deferred.unit
          | Delayed_response d -> d
        in
        if Deferred.is_determined d then
          check_responded ()
        else begin
          upon d
            (fun () ->
               check_responded ()
               |> Rpc_result.or_error
                    ~rpc_tag:query.tag
                    ~rpc_version:query.version
                    ~connection_description:t.connection_description
                    ~connection_close_started:t.connection_close_started
               |> ok_exn);
          Ok ()
        end
      | Error exn -> handle_exn exn
    in
    ( match Deferred.peek d with
      | None ->
        Wait (d >>| fun r ->
              ok_exn (
                Rpc_result.or_error
                  ~rpc_tag:query.tag
                  ~rpc_version:query.version
                  ~connection_description:t.connection_description
                  ~connection_close_started:t.connection_close_started
                  r))
      | Some result ->
        match result with
        | Ok ()   -> Continue
        | Error _ -> Stop result
    )
  | Implementation.F.Streaming_rpc
      (bin_query_reader, bin_init_writer, bin_update_writer, impl) ->
    let stream_query =
      bin_read_from_bigstring P.Stream_query.bin_reader_nat0_t
        read_buffer ~pos_ref:read_buffer_pos_ref
        ~len:query.data
        ~location:"server-side pipe_rpc stream_query un-bin-io'ing"
        ~add_len:(function `Abort -> 0 | `Query (len : Nat0.t) -> (len :> int))
    in
    begin
      match stream_query with
      | Error _err -> ()
      | Ok `Abort ->
        (* Note that there's some delay between when we receive a pipe RPC query and
           when we put something in [open_streaming_responses] (we wait for
           a user-supplied function to return). During this time, an abort message would
           just be ignored. The dispatcher can't abort the query while this is
           happening, though, since the interface doesn't expose the ID required to
           abort the query until after a response has been returned. *)
        Option.iter (Hashtbl.find t.open_streaming_responses query.id) ~f:(function
          | Pipe pipe -> Pipe.close_read pipe
          | Direct w -> Direct_stream_writer.close w
        )
      | Ok (`Query len) ->
        let data =
          bin_read_from_bigstring bin_query_reader read_buffer
            ~pos_ref:read_buffer_pos_ref ~len
            ~location:"streaming_rpc server-side query un-bin-io'ing"
        in
        let stream_writer =
          Cached_stream_writer.create ~id ~bin_writer:bin_update_writer
        in
        let impl_with_state =
          match impl with
          | Pipe f -> `Pipe f
          | Direct f ->
            let writer : _ Direct_stream_writer.t =
              { id         = Direct_stream_writer.Id.create ()
              ; state      = Not_started (Queue.create ())
              ; closed     = Ivar.create ()
              ; instance   = t.packed_self
              ; query_id   = id
              ; groups     = Bag.create ()
              ; stream_writer
              }
            in
            Hashtbl.set t.open_streaming_responses ~key:query.id ~data:(Direct writer);
            `Direct (f, writer)
        in
        let run_impl impl split_ok handle_ok =
          Rpc_result.try_with (fun () -> defer_result (data >>|~ impl))
            ~location:"server-side pipe_rpc computation"
          >>> function
          | Error err ->
            Hashtbl.remove t.open_streaming_responses id;
            write_response t id bin_init_writer (Error err)
          | Ok (Error err) ->
            Hashtbl.remove t.open_streaming_responses id;
            write_response t id bin_init_writer (Ok err)
          | Ok (Ok ok) ->
            let (initial, rest) = split_ok ok in
            write_response t id bin_init_writer (Ok initial);
            handle_ok rest
        in
        match impl_with_state with
        | `Pipe f ->
          run_impl
            (fun data -> f t.connection_state data)
            Fn.id
            (fun pipe_r ->
               Hashtbl.set t.open_streaming_responses ~key:id ~data:(Pipe pipe_r);
               don't_wait_for
                 (Writer.transfer t.writer pipe_r
                    (Cached_stream_writer.write stream_writer t.packed_self id));
               Pipe.closed pipe_r >>> fun () ->
               Pipe.upstream_flushed pipe_r
               >>> function
               | `Ok | `Reader_closed ->
                 write_response t id P.Stream_response_data.bin_writer_nat0_t (Ok `Eof);
                 Hashtbl.remove t.open_streaming_responses id
            )
        | `Direct (f, writer) ->
          run_impl
            (fun data -> f t.connection_state data writer)
            (fun x -> (x, ()))
            (fun () -> Direct_stream_writer.start writer)
    end;
    Continue
;;

  let flush (T t) =
    assert (not t.stopped);
    let producers_flushed =
      Hashtbl.fold t.open_streaming_responses ~init:[]
        ~f:(fun ~key:_ ~data acc ->
          match data with
          | Direct _ -> acc
          | Pipe pipe -> Deferred.ignore_m (Pipe.upstream_flushed pipe) :: acc)
    in
    Deferred.all_unit producers_flushed
  ;;

  let stop (T t) =
    t.stopped <- true;
    Hashtbl.iter t.open_streaming_responses ~f:(function
      | Direct writer ->
        (* Don't remove the writer from the instance, as that would modify the hashtable
           that we are currently iterating over. *)
        Direct_stream_writer.close_without_removing_from_instance writer
      | Pipe _ -> ()
    );
    Hashtbl.clear t.open_streaming_responses
  ;;

  let handle_unknown_rpc on_unknown_rpc error t query : _ Transport.Handler_result.t =
    match on_unknown_rpc with
    | `Continue         -> Continue
    | `Raise            -> Rpc_error.raise error t.connection_description
    | `Close_connection -> Stop (Ok ())
    | `Call f ->
      match
        f t.connection_state ~rpc_tag:(P.Rpc_tag.to_string query.P.Query.tag)
          ~version:query.version
      with
      | `Close_connection -> Stop (Ok ())
      | `Continue         -> Continue
  ;;

  let handle_query_internal t ~(query : Nat0.t P.Query.t)
        ~read_buffer ~read_buffer_pos_ref =
    let { implementations; on_unknown_rpc } = t.implementations in
    let description : Description.t =
      { name = P.Rpc_tag.to_string query.tag; version = query.version }
    in
    match t.last_dispatched_implementation with
    | Some (last_desc, implementation) when Description.equal last_desc description ->
      apply_implementation t implementation ~query ~read_buffer ~read_buffer_pos_ref
    | None | Some _ ->
      match Hashtbl.find implementations description with
      | Some implementation ->
        t.last_dispatched_implementation <- Some (description, implementation);
        apply_implementation t implementation ~query ~read_buffer ~read_buffer_pos_ref
      | None ->
        match on_unknown_rpc with
        | `Expert impl ->
          let {P.Query.tag; version; id; data = len} = query in
          let d =
            let responder = Responder.create id t.writer in
            impl t.connection_state ~rpc_tag:(P.Rpc_tag.to_string tag) ~version
              responder read_buffer ~pos:!read_buffer_pos_ref
              ~len:(len :> int)
          in
          if Deferred.is_determined d
          then Continue
          else Wait d
        | (`Continue | `Raise | `Close_connection | `Call _) as on_unknown_rpc ->
          let error = Rpc_error.Unimplemented_rpc (query.tag, `Version query.version) in
          write_response t query.id P.Message.bin_writer_nat0_t (Error error);
          handle_unknown_rpc on_unknown_rpc error t query
  ;;

  let handle_query (T t) ~query ~read_buffer ~read_buffer_pos_ref =
    if t.stopped || Writer.is_closed t.writer then
      Transport.Handler_result.Stop (Ok ())
    else
      handle_query_internal t ~query ~read_buffer ~read_buffer_pos_ref
  ;;
end

module Direct_stream_writer = Instance.Direct_stream_writer

let create ~implementations:i's ~on_unknown_rpc =
  (* Make sure the tags are unique. *)
  let implementations = Description.Table.create ~size:10 () in
  let dups = Description.Hash_set.create ~size:10 () in
  List.iter i's ~f:(fun (i : _ Implementation.t) ->
    let description =
      { Description.
        name    = P.Rpc_tag.to_string i.tag
      ; version = i.version
      }
    in
    match Hashtbl.add implementations ~key:description ~data:i.f with
    | `Ok -> ()
    | `Duplicate -> Hash_set.add dups description
  );
  if not (Hash_set.is_empty dups) then
    Error (`Duplicate_implementations (Hash_set.to_list dups))
  else
    Ok {
      implementations;
      on_unknown_rpc = (on_unknown_rpc :> _ on_unknown_rpc_with_expert);
    }

let instantiate t
      ~connection_description ~connection_close_started ~connection_state ~writer =
  let rec unpacked : _ Instance.unpacked =
    { implementations = t
    ; writer
    ; open_streaming_responses = Hashtbl.Poly.create ~size:10 ()
    ; connection_state
    ; connection_description
    ; connection_close_started
    ; stopped = false
    ; last_dispatched_implementation = None
    ; packed_self = Instance.T unpacked
    }
  in
  unpacked.packed_self
;;

exception Duplicate_implementations of Description.t list [@@deriving sexp]

let create_exn ~implementations ~on_unknown_rpc =
  match create ~implementations ~on_unknown_rpc with
  | Ok x -> x
  | Error `Duplicate_implementations dups -> raise (Duplicate_implementations dups)

let null () = create_exn ~implementations:[] ~on_unknown_rpc:`Raise

let add_exn t (implementation : _ Implementation.t) =
  let desc : Description.t =
    { name = P.Rpc_tag.to_string implementation.tag
    ; version = implementation.version
    }
  in
  let implementations = Hashtbl.copy t.implementations in
  match Hashtbl.add implementations ~key:desc ~data:implementation.f with
  | `Duplicate -> raise (Duplicate_implementations [desc])
  | `Ok -> { t with implementations }

let add t implementation =
  Or_error.try_with (fun () -> add_exn t implementation)


let lift {implementations; on_unknown_rpc} ~f =
  let implementations =
    Hashtbl.map implementations ~f:(Implementation.F.lift ~f)
  in
  let on_unknown_rpc =
    match on_unknown_rpc with
    | `Raise | `Continue | `Close_connection as x -> x
    | `Call call -> `Call (fun state -> call (f state))
    | `Expert expert -> `Expert (fun state -> expert (f state))
  in
  { implementations; on_unknown_rpc }

module Expert = struct
  module Responder = Responder

  module Rpc_responder = struct
    type t = Responder.t

    let cannot_send r =
      failwiths ~here:[%here] "Message cannot be sent" r [%sexp_of: _ Transport.Send_result.t]
    ;;

    let mark_responded (t : t) =
      if t.responded then failwiths ~here:[%here] "Already responded" t [%sexp_of: Responder.t];
      t.responded <- true;
    ;;

    let schedule (t : t) buf ~pos ~len =
      mark_responded t;
      let header : Nat0.t P.Message.t =
        Response
          { id = t.query_id
          ; data = Ok (Nat0.of_int_exn len)
          }
      in
      match
        Writer.send_bin_prot_and_bigstring_non_copying t.writer
          P.Message.bin_writer_nat0_t header
          ~buf ~pos ~len
      with
      | Sent d -> `Flushed d
      | Closed -> `Connection_closed
      | Message_too_big _ as r -> cannot_send r

    let handle_send_result : unit Transport.Send_result.t -> unit = function
      | Sent () | Closed -> ()
      | Message_too_big _ as r -> cannot_send r

    let write_bigstring (t : t) buf ~pos ~len =
      mark_responded t;
      let header : Nat0.t P.Message.t =
        Response
          { id = t.query_id
          ; data = Ok (Nat0.of_int_exn len)
          }
      in
      Writer.send_bin_prot_and_bigstring t.writer
        P.Message.bin_writer_nat0_t header
        ~buf ~pos ~len
      |> handle_send_result

    let write_error (t : t) error =
      mark_responded t;
      let data =
        Rpc_result.uncaught_exn ~location:"server-side raw rpc computation"
          (Error.to_exn error)
      in
      Writer.send_bin_prot t.writer P.Message.bin_writer_nat0_t
        (Response {id = t.query_id; data})
      |> handle_send_result

    let write_bin_prot (t : t) bin_writer_a a =
      mark_responded t;
      Writer.send_bin_prot t.writer
        (P.Message.bin_writer_needs_length (Writer_with_length.of_writer bin_writer_a))
        (Response {id = t.query_id; data = Ok a})
      |> handle_send_result
  end

  let create_exn = create_exn
end
