(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

module Make(Closure_properties : Ctypes_ffi.CLOSURE_PROPERTIES) =
struct
  open Dl
  open Ctypes

  module Ffi = Ctypes_ffi.Make(Closure_properties)

  exception CallToExpiredClosure = Ctypes_ffi_stubs.CallToExpiredClosure

  let funptr ?(abi=Libffi_abi.default_abi) ?name ?(check_errno=false)
      ?(runtime_lock=false) ?(thread_registration=false) fn =
    let open Ffi in
    let read = function_of_pointer
      ~abi ~check_errno ~release_runtime_lock:runtime_lock ?name fn
    and write = pointer_of_function fn
      ~abi ~acquire_runtime_lock:runtime_lock ~thread_registration in
    Ctypes_static.(view ~read ~write (static_funptr fn))

  let funptr_opt ?abi ?name ?check_errno ?runtime_lock ?thread_registration fn =
    Ctypes_std_views.nullable_funptr_view
      (funptr ?abi ?name ?check_errno ?runtime_lock ?thread_registration fn) fn

  let funptr_of_raw_ptr p = 
    Ctypes.funptr_of_raw_address (Ctypes_ptr.Raw.to_nativeint p)

  let ptr_of_raw_ptr p = 
    Ctypes.ptr_of_raw_address (Ctypes_ptr.Raw.to_nativeint p)

  let foreign_value ?from symbol t =
    from_voidp t (ptr_of_raw_ptr
                    (Ctypes_ptr.Raw.of_nativeint (dlsym ?handle:from ~symbol)))

  let foreign ?(abi=Libffi_abi.default_abi) ?from ?(stub=false)
      ?(check_errno=false) ?(release_runtime_lock=false) symbol typ =
    try
      let coerce = Ctypes_coerce.coerce (static_funptr (void @-> returning void))
        (funptr ~abi ~name:symbol ~check_errno ~runtime_lock:release_runtime_lock typ) in
      coerce (funptr_of_raw_ptr
                (Ctypes_ptr.Raw.of_nativeint
                   (dlsym ?handle:from ~symbol)))
    with
    | exn -> if stub then fun _ -> raise exn else raise exn

  module type Funptr = sig
    type fn
    type t
    val t : t Ctypes.typ
    val t_opt : t option Ctypes.typ
    val free : t -> unit
    val of_fun : fn -> t
    val with_fun : fn -> (t -> 'c) -> 'c
  end

  let dynamic_funptr (type a) (type b) ?(abi=Libffi_abi.default_abi)
        ?(runtime_lock=false) ?(thread_registration=false) fn
      : (module Funptr with type fn = a -> b) =
    (module struct
    type fn = a -> b
    type t = fn Ffi.funptr

    let t =
      let write = Ffi.funptr_to_static_funptr in
      let read = Ffi.funptr_of_static_funptr in
      Ctypes_static.(view ~read ~write (static_funptr fn))

    let t_opt = Ctypes_std_views.nullable_funptr_view t fn
    let free = Ffi.free_funptr
    let of_fun = Ffi.funptr_of_fun ~abi ~acquire_runtime_lock:runtime_lock
      ~thread_registration fn

    let with_fun f do_it =
      let f = of_fun f in
      match do_it f with
      | res -> free f; res
      | exception exn -> free f; raise exn
  end)

  let report_leaked_funptr = Ffi.report_leaked_funptr
end
