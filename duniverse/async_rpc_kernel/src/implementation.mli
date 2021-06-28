(** Internal to [Async_rpc_kernel].  See [Rpc.Implementation]. *)

open! Core_kernel
open! Async_kernel
open Protocol

open Implementation_types.Implementation

module Expert : sig
  module Responder : sig
    type t = Expert.Responder.t =
      { query_id          : Query_id.t
      ; writer            : Transport.Writer.t
      ; mutable responded : bool
      } [@@deriving sexp_of]

    val create : Query_id.t -> Transport.Writer.t -> t
  end

  type implementation_result = Expert.implementation_result =
    | Replied
    | Delayed_response of unit Deferred.t
end

module F : sig
  type ('a, 'b) result_mode = ('a, 'b) F.result_mode =
    | Blocking : ('a, 'a           ) result_mode
    | Deferred : ('a, 'a Deferred.t) result_mode

  type ('connection_state, 'query, 'init, 'update) streaming_impl =
    ('connection_state, 'query, 'init, 'update) F.streaming_impl =
    | Pipe of
        ('connection_state
         -> 'query
         -> ('init * 'update Pipe.Reader.t, 'init) Result.t Deferred.t
        )
    | Direct of
        ('connection_state
         -> 'query
         -> 'update Implementation_types.Direct_stream_writer.t
         -> ('init, 'init) Result.t Deferred.t
        )

  type 'connection_state t = 'connection_state F.t =
    | One_way
      : 'msg Bin_prot.Type_class.reader
        * ('connection_state -> 'msg -> unit )
      -> 'connection_state t
    | One_way_expert
      : ('connection_state -> Bigstring.t -> pos : int -> len : int -> unit)
      -> 'connection_state t
    | Rpc
      : 'query Bin_prot.Type_class.reader
        * 'response Bin_prot.Type_class.writer
        * ('connection_state -> 'query -> 'result)
        * ('response, 'result) result_mode
      -> 'connection_state t
    | Rpc_expert
      : ('connection_state
         -> Expert.Responder.t
         -> Bigstring.t
         -> pos : int
         -> len : int
         -> 'result)
        * (Expert.implementation_result, 'result) result_mode
      -> 'connection_state t
    | Streaming_rpc
      : 'query Bin_prot.Type_class.reader
    (* 'init can be an error or an initial state *)
        * 'init Bin_prot.Type_class.writer
        * 'update Bin_prot.Type_class.writer
        * ('connection_state, 'query, 'init, 'update) streaming_impl
      -> 'connection_state t

  val lift : 'a t -> f:('b -> 'a) -> 'b t
end

type 'connection_state t = 'connection_state Implementation_types.Implementation.t =
  { tag     : Rpc_tag.t
  ; version : int
  ; f       : 'connection_state F.t
  }

val description : _ t -> Description.t

val lift : 'a t -> f:('b -> 'a) -> 'b t
