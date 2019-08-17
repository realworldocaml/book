(** A stack implemented with a list. *)

open! Import

(** This module has nearly the same interface as [Stack].  If you need O(1) [copy] and
    [to_list], use [Linked_stack].  Otherwise, [Stack] is usually faster. *)

(** @open *)
include Stack.S
