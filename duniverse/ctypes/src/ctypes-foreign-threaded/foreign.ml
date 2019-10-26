(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

include Ctypes_foreign_basis.Make(Ctypes_closure_properties.Make(Mutex))

let () = begin
  (* Initialize the Thread library and set up the hook for registering C
     threads with the OCaml runtime *)
  let _ : Thread.t = Thread.self () in
  Ctypes_foreign_threaded_stubs.setup_thread_registration ()
end
