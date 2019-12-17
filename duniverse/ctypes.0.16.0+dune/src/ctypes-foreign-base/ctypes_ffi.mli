(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module type CLOSURE_PROPERTIES =
sig
  val record : Obj.t -> Obj.t -> int
  (** [record c v] links the lifetimes of [c] and [v], ensuring that [v] is not
      collected while [c] is still live.  The return value is a key
      that can be used to retrieve [v] while [v] is still live. *)

  val retrieve : int -> Obj.t
  (** [retrieve v] retrieves a value using a key returned by [record], or raises
      [Not_found] if [v] is no longer live. *)
end

module Make(Closure_properties : CLOSURE_PROPERTIES) :
sig
  open Ctypes_static
  open Libffi_abi

  (** Dynamic function calls based on libffi *)

  val function_of_pointer : ?name:string -> abi:abi -> check_errno:bool ->
    release_runtime_lock:bool -> ('a -> 'b) fn -> ('a -> 'b) static_funptr ->
    ('a -> 'b)
  (** Build an OCaml function from a type specification and a pointer to a C
      function. *)

  val pointer_of_function : abi:abi -> acquire_runtime_lock:bool ->
    thread_registration:bool ->
    ('a -> 'b) fn -> ('a -> 'b) -> ('a -> 'b) static_funptr
  (** Build an C function from a type specification and an OCaml function.

      The C function pointer returned is callable as long as the OCaml function
      value is live. *)

  type 'a funptr

  val free_funptr : _ funptr -> unit

  val funptr_of_fun : abi:abi -> acquire_runtime_lock:bool ->
    thread_registration:bool ->
    ('a -> 'b) fn -> ('a -> 'b) -> ('a -> 'b) funptr

  val funptr_of_static_funptr : ('a -> 'b) static_funptr -> ('a -> 'b) funptr

  val funptr_to_static_funptr : ('a -> 'b) funptr -> ('a -> 'b) static_funptr

  val report_leaked_funptr : (string -> unit) ref 
end
