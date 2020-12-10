(** Automatically retrying system calls that may be interrupted with EINTR. *)

open! Core
module Syscall_result = Unix.Syscall_result

(** [syscall f] repeatedly calls [f] until it returns or raises an exception that
    isn't [Unix_error (EINTR, _, _)]. *)
val syscall : (unit -> 'a) -> ('a, exn) Result.t

(** [syscall_result a f] repeatedly calls [f a] until it returns a result that is
    not [Syscall_result.create_error EINTR]. *)
val syscall_result : 'a -> ('a -> 'b Syscall_result.t) -> 'b Syscall_result.t

val syscall_result2
  :  'a
  -> 'b
  -> ('a -> 'b -> 'c Syscall_result.t)
  -> 'c Syscall_result.t
