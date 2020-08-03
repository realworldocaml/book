(** Internal to [Async_rpc_kernel].  See [Connection_intf.S]. *)

open! Core_kernel
open! Async_kernel
open Protocol

include Connection_intf.S

(* Internally, we use a couple of extra functions on connections that aren't exposed to
   users. *)

type response_handler
  =  Nat0.t Response.t
  -> read_buffer : Bigstring.t
  -> read_buffer_pos_ref : int ref
  -> [ `keep
     | `wait of unit Deferred.t
     | `remove of unit Rpc_result.t
     | `remove_and_wait of unit Deferred.t
     ]

val dispatch
  :  t
  -> response_handler : response_handler option
  -> bin_writer_query : 'a Bin_prot.Type_class.writer
  -> query            : 'a Query.t
  -> (unit, [`Closed]) Result.t

val dispatch_bigstring
  :  t
  -> tag : Rpc_tag.t
  -> version : int
  -> Bigstring.t
  -> pos : int
  -> len : int
  -> response_handler : response_handler option
  -> (unit, [`Closed]) Result.t

val schedule_dispatch_bigstring
  :  t
  -> tag : Rpc_tag.t
  -> version : int
  -> Bigstring.t
  -> pos : int
  -> len : int
  -> response_handler : response_handler option
  -> (unit Deferred.t, [`Closed]) Result.t

val default_handshake_timeout : Time_ns.Span.t
