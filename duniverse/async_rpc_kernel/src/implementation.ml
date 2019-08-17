open Core_kernel
open Async_kernel
open Protocol

open Implementation_types.Implementation

module Expert = struct
  module Responder = struct
    type t = Expert.Responder.t =
      { query_id          : Query_id.t
      ; writer            : Transport.Writer.t
      ; mutable responded : bool
      } [@@deriving sexp_of]

    let create query_id writer =
      { query_id
      ; writer
      ; responded = false
      }
    ;;
  end

  type implementation_result = Expert.implementation_result =
    | Replied
    | Delayed_response of unit Deferred.t
end

module F = struct
  type ('a, 'b) result_mode = ('a, 'b) F.result_mode =
    | Blocking : ('a, 'a) result_mode
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

  let lift t ~f =
    match t with
    | One_way (bin_msg, impl) ->
      One_way (bin_msg, fun state str -> impl (f state) str)
    | One_way_expert impl ->
      One_way_expert (fun state buf ~pos ~len -> impl (f state) buf ~pos ~len)
    | Rpc (bin_query, bin_response, impl, result_mode) ->
      Rpc (bin_query, bin_response, (fun state q -> impl (f state) q), result_mode)
    | Rpc_expert (impl, result_mode) ->
      Rpc_expert ((fun state resp buf ~pos ~len -> impl (f state) resp buf ~pos ~len),
                  result_mode)
    | Streaming_rpc (bin_q, bin_i, bin_u, impl) ->
      let impl =
        match impl with
        | Pipe impl -> Pipe (fun state q -> impl (f state) q)
        | Direct impl -> Direct (fun state q w -> impl (f state) q w)
      in
      Streaming_rpc (bin_q, bin_i, bin_u, impl)
end

type nonrec 'connection_state t = 'connection_state t =
  { tag     : Rpc_tag.t
  ; version : int
  ; f       : 'connection_state F.t
  }

let description t = { Description. name = Rpc_tag.to_string t.tag; version = t.version }

let lift t ~f = { t with f = F.lift ~f t.f }
