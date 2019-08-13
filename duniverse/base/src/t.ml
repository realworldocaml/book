(** This module defines various abstract interfaces that are convenient when one needs a
    module that matches a bare signature with just a type. This sometimes occurs in
    functor arguments and in interfaces. *)

open! Import

module type T  = sig type t end
module type T1 = sig type 'a t end
module type T2 = sig type ('a, 'b) t end
module type T3 = sig type ('a, 'b, 'c) t end
