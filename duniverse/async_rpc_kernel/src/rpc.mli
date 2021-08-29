(** A library for building asynchronous RPC-style protocols.

    The approach here is to have a separate representation of the server-side
    implementation of an RPC (An [Implementation.t]) and the interface that it exports
    (either an [Rpc.t], a [State_rpc.t] or a [Pipe_rpc.t], but we'll refer to them
    generically as RPC interfaces).  A server builds the [Implementation.t] out of an RPC
    interface and a function for implementing the RPC, while the client dispatches a
    request using the same RPC interface.

    The [Implementation.t] hides the type of the query and the response, whereas the
    [Rpc.t] is polymorphic in the query and response type.  This allows you to build a
    [Implementations.t] out of a list of [Implementation.t]s.

    Each RPC also comes with a version number.  This is meant to allow support of multiple
    different versions of what is essentially the same RPC.  You can think of it as an
    extension to the name of the RPC, and in fact, each RPC is uniquely identified by its
    (name, version) pair.  RPCs with the same name but different versions should implement
    similar functionality. *)

open! Core_kernel
open! Async_kernel

module Description : sig
  type t =
    { name    : string
    ; version : int
    }
  [@@deriving compare, hash, sexp_of]

  module Stable : sig
    module V1 : sig
      type nonrec t = t [@@deriving compare, sexp, bin_io, hash]
    end
  end
end

(** A ['connection_state Implementation.t] is something that knows how to respond to one
    query, given a ['connection_state].  That is, you can create a ['connection_state
    Implementation.t] by providing a function which takes a query *and* a
    ['connection_state] and provides a response.

    The reason for this is that RPCs often do something like look something up in a master
    structure.  This way, [Implementation.t]s can be created without having the master
    structure in your hands. *)
module Implementation : sig
  type 'connection_state t

  val description : _ t -> Description.t

  (** We may want to use an ['a t] implementation (perhaps provided by someone else) in a
      ['b t] context.  We can do this as long as we can map our state into the state
      expected by the original implementer. *)
  val lift : 'a t -> f:('b -> 'a) -> 'b t
end

(** A ['connection_state Implementations.t] is something that knows how to respond to
    many different queries.  It is conceptually a package of ['connection_state
    Implementation.t]s. *)
module Implementations : sig
  type 'connection_state t = 'connection_state Implementations.t

  (** a server that can handle no queries *)
  val null : unit -> 'connection_state t

  val lift : 'a t -> f:('b -> 'a) -> 'b t

  type 'connection_state on_unknown_rpc =

    [ `Raise
    | `Continue
    | `Close_connection  (** used to be the behavior of [`Ignore] *)
    (** [rpc_tag] and [version] are the name and version of the unknown rpc *)
    | `Call of
        ('connection_state
         -> rpc_tag : string
         -> version : int
         -> [ `Close_connection | `Continue ])
    ]

  (** [create ~implementations ~on_unknown_rpc] creates a server capable of responding to
      the rpcs implemented in the implementation list.  Be careful about setting
      [on_unknown_rpc] to [`Raise] because other programs may mistakenly connect to this
      one causing it to crash. *)
  val create
    :  implementations : 'connection_state Implementation.t list
    -> on_unknown_rpc : 'connection_state on_unknown_rpc
    -> ( 'connection_state t
       , [`Duplicate_implementations of Description.t list]
       ) Result.t

  val create_exn
    :  implementations : 'connection_state Implementation.t list
    -> on_unknown_rpc :
      [ `Raise
      | `Continue
      | `Close_connection  (** used to be the behavior of [`Ignore] *)
      | `Call of
          ('connection_state
           -> rpc_tag : string
           -> version : int
           -> [ `Close_connection | `Continue ])
      ]
    -> 'connection_state t

  val add
    :  'connection_state t
    -> 'connection_state Implementation.t
    -> 'connection_state t Or_error.t

  val add_exn
    :  'connection_state t
    -> 'connection_state Implementation.t
    -> 'connection_state t

  val descriptions : _ t -> Description.t list

  (** Low-level, untyped access to queries.  Regular users should ignore this. *)
  module Expert : sig
    (** See [Rpc.Expert.Responder] for how to use this. *)
    module Responder : sig
      type t
    end

    (** Same as [create_exn], except for the additional [`Expert] variant. *)
    val create_exn
      :  implementations : 'connection_state Implementation.t list
      -> on_unknown_rpc :
        [ `Raise
        | `Continue
        | `Close_connection  (** used to be the behavior of [`Ignore] *)
        | `Call of
            ('connection_state
             -> rpc_tag : string
             -> version : int
             -> [ `Close_connection | `Continue ])
        | `Expert of
            (** The [Deferred.t] the function returns is only used to determine when it is
                safe to overwrite the supplied [Bigstring.t], so it is *not* necessary to
                completely finish handling the query before it is filled in.  In
                particular, if you don't intend to read from the [Bigstring.t] after the
                function returns, you can return [Deferred.unit]. *)
            ('connection_state
             -> rpc_tag : string
             -> version : int
             -> Responder.t
             -> Bigstring.t
             -> pos : int
             -> len : int
             -> unit Deferred.t)
        ]
      -> 'connection_state t
  end
end

module Transport = Transport

module Connection : Connection_intf.S

module Rpc : sig
  type ('query, 'response) t

  val create
    :  name         : string
    -> version      : int
    -> bin_query    : 'query    Bin_prot.Type_class.t
    -> bin_response : 'response Bin_prot.Type_class.t
    -> ('query, 'response) t

  (** the same values as were passed to create. *)
  val name    : (_, _) t -> string
  val version : (_, _) t -> int

  val description : (_, _) t -> Description.t

  val bin_query    : ('query, _)    t -> 'query    Bin_prot.Type_class.t
  val bin_response : (_, 'response) t -> 'response Bin_prot.Type_class.t

  (** If the function that implements the RPC raises, the implementer does not see the
      exception. Instead, it is sent as an error to the caller of the RPC, i.e. the
      process that called [dispatch] or one of its alternatives.*)
  val implement
    :  ('query, 'response) t
    -> ('connection_state
        -> 'query
        -> 'response Deferred.t)
    -> 'connection_state Implementation.t

  (** [implement'] is different from [implement] in that:

      1. ['response] is immediately serialized and scheduled for delivery to the RPC
         dispatcher.

      2. Less allocation happens, as none of the Async-related machinery is necessary.

      [implement] also tries to do 1 when possible, but it is guaranteed to happen with
      [implement']. *)
  val implement'
    :  ('query, 'response) t
    -> ('connection_state
        -> 'query
        -> 'response)
    -> 'connection_state Implementation.t

  (** [dispatch'] exposes [Rpc_result.t] as output. Passing it through
      [rpc_result_to_or_error] gives you the same result as [dispatch] *)
  val dispatch'
    :  ('query, 'response) t
    -> Connection.t
    -> 'query
    -> 'response Rpc_result.t Deferred.t

  val rpc_result_to_or_error
    :  ('query, 'response) t
    -> Connection.t
    -> 'response Rpc_result.t
    -> 'response Or_error.t

  val dispatch
    :  ('query, 'response) t
    -> Connection.t
    -> 'query
    -> 'response Or_error.t Deferred.t

  val dispatch_exn
    :  ('query, 'response) t
    -> Connection.t
    -> 'query
    -> 'response Deferred.t

  module Expert : sig
    module Responder : sig
      type t = Implementations.Expert.Responder.t

      (** As in [Writer], after calling [schedule], you should not overwrite the
          [Bigstring.t] passed in until the responder is flushed. *)
      val schedule
        :  t
        -> Bigstring.t
        -> pos : int
        -> len : int
        -> [`Flushed of unit Deferred.t | `Connection_closed]

      (** On the other hand, these are written immediately. *)
      val write_bigstring : t -> Bigstring.t -> pos:int -> len:int -> unit
      val write_bin_prot  : t -> 'a Bin_prot.Type_class.writer -> 'a -> unit
      val write_error     : t -> Error.t -> unit
    end

    (** This just schedules a write, so the [Bigstring.t] should not be overwritten until
        the flushed [Deferred.t] is determined.

        The return value of [handle_response] has the same meaning as in the function
        argument of [Implementations.Expert.create]. *)
    val schedule_dispatch
      :  Connection.t
      -> rpc_tag : string
      -> version : int
      -> Bigstring.t
      -> pos : int
      -> len : int
      -> handle_response : (Bigstring.t -> pos:int -> len:int -> unit Deferred.t)
      -> handle_error : (Error.t -> unit)
      -> [`Flushed of unit Deferred.t | `Connection_closed]

    val dispatch
      :  Connection.t
      -> rpc_tag : string
      -> version : int
      -> Bigstring.t
      -> pos : int
      -> len : int
      -> handle_response : (Bigstring.t -> pos:int -> len:int -> unit Deferred.t)
      -> handle_error : (Error.t -> unit)
      -> [ `Ok | `Connection_closed ]

    (** Result of callbacks passed to [implement] and [implement'] and
        [implement_for_tag_and_version] and [implement_for_tag_and_version']:

        - [Replied] means that the response has already been sent using one of the
          functions of [Responder]
        - [Delayed_response d] means that the implementation is done using the input
          bigstring, but hasn't send the response yet. When [d] becomes determined
          it is expected that the response has been sent.

        Note: it is NOT OK for an implementation to return:

        {[
          Delayed_response (Responder.schedule responder buf ~pos:... ~len:...)
        ]}

        where [buf] is the same bigstring as the one containing the query. This is because
        it would indicate that [buf] can be overwritten even though it is still being used
        by [Responder.schedule]. *)
    type implementation_result =
      | Replied
      | Delayed_response of unit Deferred.t

    val implement
      :  (_, _) t
      -> ('connection_state
          -> Responder.t
          -> Bigstring.t
          -> pos : int
          -> len : int
          -> implementation_result Deferred.t)
      -> 'connection_state Implementation.t

    val implement'
      :  (_, _) t
      -> ('connection_state
          -> Responder.t
          -> Bigstring.t
          -> pos : int
          -> len : int
          -> implementation_result)
      -> 'connection_state Implementation.t

    val implement_for_tag_and_version
      :  rpc_tag: string
      -> version: int
      -> ('connection_state
          -> Responder.t
          -> Bigstring.t
          -> pos : int
          -> len : int
          -> implementation_result Deferred.t)
      -> 'connection_state Implementation.t

    val implement_for_tag_and_version'
      :  rpc_tag: string
      -> version: int
      -> ('connection_state
          -> Responder.t
          -> Bigstring.t
          -> pos : int
          -> len : int
          -> implementation_result)
      -> 'connection_state Implementation.t
  end
end

module Pipe_close_reason : sig
  type t =
    (** You closed the pipe. *)
    | Closed_locally
    (** The RPC implementer closed the pipe. *)
    | Closed_remotely
    (** An error occurred, e.g. a message could not be deserialized.  If the connection
        closes before either side explicitly closes the pipe, it will also go into this
        case. *)
    | Error of Error.t
    [@@deriving bin_io, compare, sexp]

  module Stable : sig
    module V1 : sig
      type nonrec t = t =
        | Closed_locally
        | Closed_remotely
        | Error of Error.Stable.V2.t
        [@@deriving bin_io, compare, sexp]
    end
  end
end

module Pipe_rpc : sig
  type ('query, 'response, 'error) t

  module Id : sig type t end

  module Metadata : sig
    type t
    val id : t -> Id.t
  end

  val create
    (** If the connection is backed up, the server stops consuming elements from the
        pipe. Servers should pay attention to the pipe's pushback, otherwise they risk
        running out of memory if they fill the pipe much faster than the transport can
        handle, or if the client pushes back as discussed next.

        If [client_pushes_back] is set, the client side of the connection will stop
        reading elements from the underlying file descriptor when the client's pipe has
        a sufficient number of elements enqueued, rather than reading elements eagerly.
        This will eventually cause writes on the server's side to block, indicating to
        the server it should slow down.

        There are some drawbacks to using [client_pushes_back]:

        - RPC multiplexing doesn't work as well.  The client will stop reading *all*
        messages on the connection if any pipe gets saturated, not just ones relating
        to that pipe.

        - A server that doesn't pay attention to pushback on its end will accumulate
        elements on its side of the connection, rather than on the client's side,
        meaning a slow client can make the server run out of memory. *)
    :  ?client_pushes_back : unit
    -> name : string
    -> version : int
    -> bin_query    : 'query    Bin_prot.Type_class.t
    -> bin_response : 'response Bin_prot.Type_class.t
    -> bin_error    : 'error    Bin_prot.Type_class.t
    -> unit
    -> ('query, 'response, 'error) t

  val bin_query    : ('query, _, _) t    -> 'query    Bin_prot.Type_class.t
  val bin_response : (_, 'response, _) t -> 'response Bin_prot.Type_class.t
  val bin_error    : (_, _, 'error) t    -> 'error    Bin_prot.Type_class.t

  (** The pipe returned by the implementation function will be closed automatically when
      either the connection to the client is closed or the client closes their pipe. *)
  val implement
    :  ('query, 'response, 'error) t
    -> ('connection_state
        -> 'query
        -> ('response Pipe.Reader.t, 'error) Result.t Deferred.t)
    -> 'connection_state Implementation.t

  (** A [Direct_stream_writer.t] is a simple object for responding to a [Pipe_rpc]
      query. *)
  module Direct_stream_writer : sig
    type 'a t

    (** [write t x] returns [`Closed] if [t] is closed, or [`Flushed d] if it is open. In
        the open case, [d] is determined when the message has been flushed from the
        underlying [Transport.Writer.t]. *)
    val write : 'a t -> 'a -> [`Flushed of unit Deferred.t | `Closed]
    val write_without_pushback : 'a t -> 'a -> [`Ok | `Closed]
    val close : _ t -> unit
    val closed : _ t -> unit Deferred.t
    val flushed : _ t -> unit Deferred.t
    val is_closed : _ t -> bool

    module Expert : sig
      val write
        :  'a t
        -> buf:Bigstring.t
        -> pos:int
        -> len:int
        -> [`Flushed of unit Deferred.t | `Closed]

      val write_without_pushback
        :  'a t
        -> buf:Bigstring.t
        -> pos:int
        -> len:int
        -> [`Ok | `Closed]
    end

    (** Group of direct writers. Groups are optimized for sending the same message to
        multiple clients at once. *)
    module Group : sig
      type 'a direct_stream_writer
      type 'a t

      (** A group internally holds a buffer to serialize messages only once. This buffer
          will grow automatically to accomodate bigger messages.

          It is safe to share the same buffer between multiple groups. *)
      module Buffer : sig
        type t
        val create : ?initial_size:int (* default 4096 *) -> unit -> t
      end

      val create : ?buffer:Buffer.t -> unit -> _ t

      (** [flushed_or_closed t] is determined when the underlying writer for each member of [t] is
          flushed or closed. *)
      val flushed_or_closed : _ t -> unit Deferred.t
      val flushed : _ t -> unit Deferred.t [@@deprecated "[since 2019-11] renamed as [flushed_or_closed]" ]

      (** Add a direct stream writer to the group. Raises if the writer is closed or
          already part of the group, or if its bin-prot writer is different than an
          existing group member's. When the writer is closed, it is automatically
          removed from the group. *)
      val add_exn : 'a t -> 'a direct_stream_writer -> unit

      (** Remove a writer from a group. Note that writers are automatically removed from
          all groups when they are closed, so you only need to call this if you want to
          remove a writer without closing it. *)
      val remove : 'a t -> 'a direct_stream_writer -> unit

      (** Write a message on all direct writers in the group. Contrary to
          [Direct_stream_writer.write], this cannot return [`Closed] as elements of the
          group are removed immediately when they are closed.

          [write t x] is the same as [write_without_pushback t x; flushed t].
      *)
      val write : 'a t -> 'a -> unit Deferred.t
      val write_without_pushback : 'a t -> 'a -> unit

      val to_list : 'a t -> 'a direct_stream_writer list

      val length : _ t -> int

      module Expert : sig
        val write
          :  'a t
          -> buf:Bigstring.t
          -> pos:int
          -> len:int
          -> unit Deferred.t

        val write_without_pushback
          :  'a t
          -> buf:Bigstring.t
          -> pos:int
          -> len:int
          -> unit
      end
    end with type 'a direct_stream_writer := 'a t
  end

  (** Similar to [implement], but you are given the writer instead of providing a writer
      and the writer is a [Direct_stream_writer.t] instead of a [Pipe.Writer.t].

      The main advantage of this interface is that it consumes far less memory per open
      query.

      Though the implementation function is given a writer immediately, the result of the
      client's call to [dispatch] will not be determined until after the implementation
      function returns. Elements written before the function returns will be queued up to
      be written after the function returns. *)
  val implement_direct
    :  ('query, 'response, 'error) t
    -> ('connection_state
        -> 'query
        -> 'response Direct_stream_writer.t
        -> (unit, 'error) Result.t Deferred.t)
    -> 'connection_state Implementation.t

  (** This has [(..., 'error) Result.t] as its return type to represent the possibility of
      the call itself being somehow erroneous (but understood - the outer [Or_error.t]
      encompasses failures of that nature).  Note that this cannot be done simply by
      making ['response] a result type, since [('response Pipe.Reader.t, 'error) Result.t]
      is distinct from [('response, 'error) Result.t Pipe.Reader.t].

      Closing the pipe has the effect of calling [abort]. *)
  val dispatch
    :  ('query, 'response, 'error) t
    -> Connection.t
    -> 'query
    -> ('response Pipe.Reader.t * Metadata.t, 'error) Result.t Or_error.t Deferred.t

  val dispatch_exn
    :  ('query, 'response, 'error) t
    -> Connection.t
    -> 'query
    -> ('response Pipe.Reader.t * Metadata.t) Deferred.t

  (** The input type of the [f] passed to [dispatch_iter]. *)
  module Pipe_message : sig
    type 'a t =
      | Update of 'a
      | Closed of [`By_remote_side | `Error of Error.t]
  end

  (** The output type of the [f] passed to [dispatch_iter]. This is analagous to a simple
      [unit Deferred.t], with [Continue] being like [Deferred.unit], but it is made
      explicit when no waiting should occur. *)
  module Pipe_response : sig
    type t =
      | Continue
      | Wait of unit Deferred.t
  end

  (** Calling [dispatch_iter t conn query ~f] is similar to calling [dispatch t conn
      query] and then iterating over the result pipe with [f]. The main advantage it
      offers is that its memory usage is much lower, making it more suitable for
      situations where many queries are open at once.

      [f] may be fed any number of [Update _] messages, followed by a single [Closed _]
      message.

      [f] can cause the connection to stop reading messages off of its underlying
      [Reader.t] by returning [Wait _]. This is the same as what happens when a client
      stops reading from the pipe returned by [dispatch] when the [Pipe_rpc.t] has
      [client_pushes_back] set.

      When successful, [dispatch_iter] returns an [Id.t] after the subscription is
      started. This may be fed to [abort] with the same [Pipe_rpc.t] and [Connection.t] as
      the call to [dispatch_iter] to cancel the subscription, which will close the pipe on
      the implementation side. Calling it with a different [Pipe_rpc.t] or [Connection.t]
      has undefined behavior. *)
  val dispatch_iter
    :  ('query, 'response, 'error) t
    -> Connection.t
    -> 'query
    -> f:('response Pipe_message.t -> Pipe_response.t)
    -> (Id.t, 'error) Result.t Or_error.t Deferred.t

  (** [abort rpc connection id] given an RPC and the id returned as part of a call to
      dispatch, abort requests that the other side of the connection stop sending
      updates.

      If you are using [dispatch] rather than [dispatch_iter], you are encouraged to close
      the pipe you receive rather than calling [abort] -- both of these have the same
      effect. *)
  val abort : (_, _, _) t -> Connection.t -> Id.t -> unit

  (** [close_reason metadata] will be determined sometime after the pipe associated with
      [metadata] is closed. Its value will indicate what caused the pipe to be closed. *)
  val close_reason : Metadata.t -> Pipe_close_reason.t Deferred.t

  val client_pushes_back : (_, _, _) t -> bool
  val name               : (_, _, _) t -> string
  val version            : (_, _, _) t -> int

  val description : (_, _, _) t -> Description.t

end

(** A state rpc is an easy way for two processes to synchronize a data structure by
    sending updates over the wire.  It's basically a pipe rpc that sends/receives an
    initial state of the data structure, and then updates, and applies the updates under
    the covers. *)
module State_rpc : sig
  type ('query, 'state, 'update, 'error) t

  module Id : sig type t end

  module Metadata : sig
    type t
    val id : t -> Id.t
  end

  val create
    :  ?client_pushes_back : unit
    -> name : string
    -> version : int
    -> bin_query  : 'query  Bin_prot.Type_class.t
    -> bin_state  : 'state  Bin_prot.Type_class.t
    -> bin_update : 'update Bin_prot.Type_class.t
    -> bin_error  : 'error  Bin_prot.Type_class.t
    -> unit
    -> ('query, 'state, 'update, 'error) t

  val bin_query  : ('query, _, _, _)  t -> 'query  Bin_prot.Type_class.t
  val bin_state  : (_, 'state, _, _)  t -> 'state  Bin_prot.Type_class.t
  val bin_update : (_, _, 'update, _) t -> 'update Bin_prot.Type_class.t
  val bin_error  : (_, _, _, 'error)  t -> 'error  Bin_prot.Type_class.t

  val implement
    :  ('query, 'state, 'update, 'error) t
    -> ('connection_state
        -> 'query
        -> (('state * 'update Pipe.Reader.t), 'error) Result.t Deferred.t)
    -> 'connection_state Implementation.t

  val dispatch
    :  ('query, 'state, 'update, 'error) t
    -> Connection.t
    -> 'query
    -> ( 'state * 'update Pipe.Reader.t * Metadata.t
       , 'error
       ) Result.t Or_error.t Deferred.t

  val abort : (_, _, _, _) t -> Connection.t -> Id.t -> unit

  val close_reason : Metadata.t -> Pipe_close_reason.t Deferred.t

  val client_pushes_back : (_, _, _, _) t -> bool
  val name    : (_, _, _, _) t -> string
  val version : (_, _, _, _) t -> int

  val description : (_, _, _, _) t -> Description.t
end

(** An RPC that has no response.  Error handling is trickier here than it is for RPCs with
    responses, as there is no reasonable place to put an error if something goes wrong.
    Because of this, in the event of an error such as dispatching to an unimplemented RPC,
    the connection will be shut down.  Similarly, if the implementation raises an
    exception, the connection will be shut down. *)
module One_way : sig
  type 'msg t

  val create
    :  name     : string
    -> version  : int
    -> bin_msg  : 'msg Bin_prot.Type_class.t
    -> 'msg t

  val name        : _ t -> string
  val version     : _ t -> int
  val description : _ t -> Description.t
  val bin_msg     : 'msg t -> 'msg Bin_prot.Type_class.t

  val implement
    :  'msg t
    -> ('connection_state -> 'msg -> unit)
    -> 'connection_state Implementation.t

  (** [dispatch'] exposes [Rpc_result.t] as output. Passing it through
      [rpc_result_to_or_error] gives you the same result as [dispatch] *)
  val dispatch'
    :  'msg t
    -> Connection.t
    -> 'msg
    -> unit Rpc_result.t

  val rpc_result_to_or_error
    :  'msg t
    -> Connection.t
    -> unit Rpc_result.t
    -> unit Or_error.t

  val dispatch
    :  'msg t
    -> Connection.t
    -> 'msg
    -> unit Or_error.t

  val dispatch_exn
    :  'msg t
    -> Connection.t
    -> 'msg
    -> unit

  module Expert : sig
    val implement
      :  _ t
      -> ('connection_state
          -> Bigstring.t
          -> pos : int
          -> len : int
          -> unit)
      -> 'connection_state Implementation.t

    val dispatch
      :  _ t
      -> Connection.t
      -> Bigstring.t
      -> pos : int
      -> len : int
      -> [`Ok | `Connection_closed]

    (** Like [dispatch], but does not copy data out of the buffer, so it must not change
        until the returned [unit Deferred.t] is determined. *)
    val schedule_dispatch
      :  _ t
      -> Connection.t
      -> Bigstring.t
      -> pos : int
      -> len : int
      -> [`Flushed of unit Deferred.t | `Connection_closed]
  end
end

module Any : sig
  type t =
    | Rpc     : ('q, 'r) Rpc.t -> t
    | Pipe    : ('q, 'r, 'e) Pipe_rpc.t -> t
    | State   : ('q, 's, 'u, 'e) State_rpc.t -> t
    | One_way : 'm One_way.t -> t

  val description : t -> Description.t
end

module Stable : sig

  module Rpc : sig
    type ('query, 'response) t = ('query, 'response) Rpc.t

    val create
      :  name         : string
      -> version      : int
      -> bin_query    : 'query    Bin_prot.Type_class.t
      -> bin_response : 'response Bin_prot.Type_class.t
      -> ('query, 'response) t

    val description : (_, _) t -> Description.t

    val bin_query    : ('query, _)    t -> 'query    Bin_prot.Type_class.t
    val bin_response : (_, 'response) t -> 'response Bin_prot.Type_class.t
  end

  module Pipe_rpc : sig
    type ('query, 'response, 'error) t = ('query, 'response, 'error) Pipe_rpc.t

    val create
      :  ?client_pushes_back : unit
      -> name : string
      -> version : int
      -> bin_query    : 'query    Bin_prot.Type_class.t
      -> bin_response : 'response Bin_prot.Type_class.t
      -> bin_error    : 'error    Bin_prot.Type_class.t
      -> unit
      -> ('query, 'response, 'error) t

    val description : (_, _, _) t -> Description.t

    val bin_query    : ('query, _, _) t    -> 'query    Bin_prot.Type_class.t
    val bin_response : (_, 'response, _) t -> 'response Bin_prot.Type_class.t
    val bin_error    : (_, _, 'error) t    -> 'error    Bin_prot.Type_class.t
  end

  module State_rpc : sig
    type ('query, 'state, 'update, 'error) t =
       ('query, 'state, 'update, 'error) State_rpc.t

    val create
      :  ?client_pushes_back : unit
      -> name : string
      -> version : int
      -> bin_query  : 'query  Bin_prot.Type_class.t
      -> bin_state  : 'state  Bin_prot.Type_class.t
      -> bin_update : 'update Bin_prot.Type_class.t
      -> bin_error  : 'error  Bin_prot.Type_class.t
      -> unit
      -> ('query, 'state, 'update, 'error) t

    val description : (_, _, _, _) t -> Description.t

    val bin_query  : ('query, _, _, _)  t -> 'query  Bin_prot.Type_class.t
    val bin_state  : (_, 'state, _, _)  t -> 'state  Bin_prot.Type_class.t
    val bin_update : (_, _, 'update, _) t -> 'update Bin_prot.Type_class.t
    val bin_error  : (_, _, _, 'error)  t -> 'error  Bin_prot.Type_class.t
  end

  module One_way : sig
    type 'msg t = 'msg One_way.t

    val create
      :  name     : string
      -> version  : int
      -> bin_msg  : 'msg Bin_prot.Type_class.t
      -> 'msg t

    val description : _ t -> Description.t

    val bin_msg     : 'msg t -> 'msg Bin_prot.Type_class.t
  end
  module Description = Description.Stable
  module Pipe_close_reason = Pipe_close_reason.Stable
end
