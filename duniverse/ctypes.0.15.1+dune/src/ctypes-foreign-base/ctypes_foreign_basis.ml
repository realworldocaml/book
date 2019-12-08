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
end
