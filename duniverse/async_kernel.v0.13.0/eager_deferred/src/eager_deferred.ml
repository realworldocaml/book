open! Core_kernel
open! Async_kernel
open! Import

module type S = Eager_deferred_intf.S

include Eager_deferred1

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
