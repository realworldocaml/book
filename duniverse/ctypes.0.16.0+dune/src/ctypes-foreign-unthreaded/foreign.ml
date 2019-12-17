(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

include Ctypes_foreign_basis.Make(Ctypes_closure_properties.Make(Ctypes_gc_mutex))
