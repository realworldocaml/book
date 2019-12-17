open Ctypes

module Callback = (val Foreign.dynamic_funptr (int @-> returning int))

module Stubs (F: Ctypes.FOREIGN) =
struct
  open F

  let call_dynamic_funptr = foreign "call_dynamic_funptr"
    (Callback.t @-> int @-> returning int)
  let save_dynamic_funptr = foreign "save_dynamic_funptr"
    (Callback.t @-> returning void)
  let call_saved_dynamic_funptr = foreign "call_saved_dynamic_funptr"
    (int @-> returning int)
  let call_dynamic_funptr_opt = foreign "call_dynamic_funptr"
    (Callback.t_opt @-> int @-> returning int)
  let save_dynamic_funptr_opt = foreign "save_dynamic_funptr"
    (Callback.t_opt @-> returning void)

  type simple_closure 
  let simple_closure : simple_closure structure typ = structure "simple_closure"
  let simple_closure_f = field simple_closure "f" Callback.t
  let simple_closure_n = field simple_closure "n" int
  let () = seal simple_closure

  let call_dynamic_funptr_struct = foreign "call_dynamic_funptr_struct"
    (simple_closure @-> returning int)
  let call_dynamic_funptr_struct_ptr = foreign "call_dynamic_funptr_struct_ptr"
    (ptr simple_closure @-> returning int)
end
