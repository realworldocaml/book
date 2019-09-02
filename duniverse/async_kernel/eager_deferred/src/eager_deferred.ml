open! Core_kernel
open! Async_kernel
open! Import

include Eager_deferred1  (** @open *)

(** Intended usage is to [open Eager_deferred.Use] to shadow operations from the non-eager
    world and rebind them to their eager counterparts. *)
module Use = struct
  module Deferred = struct
    type 'a t = 'a Deferred.t

    include Eager_deferred1
  end

  include (Eager_deferred1 : Monad.Infix with type 'a t := 'a Deferred.t)
  include Eager_deferred1.Let_syntax

  let upon = Eager_deferred1.upon
  let ( >>> ) = Eager_deferred1.Infix.( >>> )

  let ( >>=? ) x f =
    x
    >>= function
    | Error _ as error -> return error
    | Ok v -> f v
  ;;

  let ( >>|? ) x f =
    x
    >>| function
    | Error _ as error -> error
    | Ok v -> Ok (f v)
  ;;
end
