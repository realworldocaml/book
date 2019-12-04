(** Async_rpc protocol types, for internal use only *)

(* WARNING: do not change any of these types without good reason *)

open Bin_prot.Std
open Sexplib.Std

module Rpc_tag : Core_kernel.Identifiable = Core_kernel.String

module Query_id = Core_kernel.Unique_id.Int63 ()

module Unused_query_id : sig
  type t [@@deriving bin_io, sexp_of]
  val t : t
end = struct
  type t = Query_id.t [@@deriving bin_io, sexp_of]
  let t = Query_id.create ()
end

module Rpc_error = struct
  type t =
    | Bin_io_exn        of Core_kernel.Sexp.t
    | Connection_closed
    | Write_error       of Core_kernel.Sexp.t
    | Uncaught_exn      of Core_kernel.Sexp.t
    | Unimplemented_rpc of Rpc_tag.t * [`Version of int]
    | Unknown_query_id  of Query_id.t
  [@@deriving bin_io, sexp]
end

module Rpc_result = struct
  type 'a t = ('a, Rpc_error.t) Core_kernel.Result.t [@@deriving bin_io, sexp_of]
end

module Header = Protocol_version_header

module Query = struct
  type 'a needs_length =
    { tag     : Rpc_tag.t
    ; version : int
    ; id      : Query_id.t
    ; data    : 'a
    }
  [@@deriving bin_io, sexp_of]
  type 'a t = 'a needs_length [@@deriving bin_read]
end

module Response = struct
  type 'a needs_length =
    { id   : Query_id.t
    ; data : 'a Rpc_result.t
    }
  [@@deriving bin_io, sexp_of]
  type 'a t = 'a needs_length [@@deriving bin_read]
end

module Stream_query = struct
  type 'a needs_length = [`Query of 'a | `Abort ] [@@deriving bin_io]
  type 'a t = 'a needs_length [@@deriving bin_read]
  type nat0_t = Nat0.t needs_length [@@deriving bin_read, bin_write]
end

module Stream_initial_message = struct
  type ('response, 'error) t =
    { unused_query_id : Unused_query_id.t
    ; initial         : ('response, 'error) Core_kernel.Result.t
    }
  [@@deriving bin_io, sexp_of]
end

module Stream_response_data = struct
  type 'a needs_length = [`Ok of 'a | `Eof] [@@deriving bin_io]
  type 'a t = 'a needs_length [@@deriving bin_read]
  type nat0_t = Nat0.t needs_length [@@deriving bin_read, bin_write]
end

module Message = struct
  type 'a needs_length =
    | Heartbeat
    | Query     of 'a Query.   needs_length
    | Response  of 'a Response.needs_length
  [@@deriving bin_io, sexp_of]
  type 'a t = 'a needs_length [@@deriving bin_read, sexp_of]
  type nat0_t = Nat0.t needs_length [@@deriving bin_read, bin_write]
end
