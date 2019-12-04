(** Internal to [Async_rpc_kernel].  See [Rpc.Implementations]. *)

open! Core_kernel
open! Async_kernel
open Protocol

type 'a t

val create
  :  implementations : 'connection_state Implementation.t list
  -> on_unknown_rpc :
       [ `Raise
       | `Continue
       | `Close_connection
       | `Call of
           ('connection_state
            -> rpc_tag : string
            -> version : int
            -> [ `Close_connection | `Continue ])
       ]
  -> ( 'connection_state t
     , [ `Duplicate_implementations of Description.t list ]
     ) Result.t

val null : unit -> 'a t

val lift : 'a t -> f:('b -> 'a) -> 'b t

module Direct_stream_writer : sig
  type 'a t = 'a Implementation_types.Direct_stream_writer.t

  module Id = Implementation_types.Direct_stream_writer.Id

  val close : _ t -> unit
  val closed : _ t -> unit Deferred.t
  val is_closed : _ t -> bool
  val write : 'a t -> 'a -> [`Flushed of unit Deferred.t | `Closed]
  val write_without_pushback : 'a t -> 'a -> [`Ok | `Closed]
  val flushed : _ t -> unit Deferred.t
  val bin_writer : 'a t -> 'a Bin_prot.Type_class.writer

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
end

module Instance : sig
  type t [@@deriving sexp_of]

  val handle_query
    :  t
    -> query : Nat0.t Query.t
    -> read_buffer : Bigstring.t
    -> read_buffer_pos_ref : int ref
    -> unit Rpc_result.t Transport.Handler_result.t

  (* Flushes all open streaming responses *)
  val flush : t -> unit Deferred.t

  (* Stop the instance: drop all responses to pending requests and make all further call
     to [handle_query] or [flush] to fail. *)
  val stop : t -> unit
end

val instantiate
  :  'a t
  -> connection_description : Info.t
  -> connection_close_started : Info.t Deferred.t
  -> connection_state : 'a
  -> writer : Transport.Writer.t
  -> Instance.t

val create_exn
  :  implementations : 'connection_state Implementation.t list
  -> on_unknown_rpc :
       [ `Raise
       | `Continue
       | `Close_connection
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

module Expert : sig
  module Responder = Implementation.Expert.Responder

  module Rpc_responder : sig
    type t = Responder.t

    val schedule
      :  t -> Bigstring.t -> pos:int -> len:int
      -> [`Connection_closed | `Flushed of unit Deferred.t]

    val write_bigstring : t -> Bigstring.t -> pos:int -> len:int -> unit
    val write_bin_prot  : t -> 'a Bin_prot.Type_class.writer -> 'a -> unit
    val write_error     : t -> Error.t -> unit
  end

  val create_exn
    :  implementations : 'connection_state Implementation.t list
    -> on_unknown_rpc :
         [ `Raise
         | `Continue
         | `Close_connection
         | `Call of
             ('connection_state
              -> rpc_tag : string
              -> version : int
              -> [ `Close_connection | `Continue ])
         | `Expert of
             ('connection_state -> rpc_tag : string -> version : int -> Responder.t
              -> Bigstring.t -> pos : int -> len : int -> unit Deferred.t)
         ]
    -> 'connection_state t
end
