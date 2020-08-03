open Ppx_compare_lib

type enum = A | B | C

module Compare = struct
  let optim_bool      : bool      compare = polymorphic_compare
  let optim_char      : char      compare = polymorphic_compare
  let optim_float     : float     compare = polymorphic_compare
  let optim_int       : int       compare = polymorphic_compare
  let optim_int32     : int32     compare = polymorphic_compare
  let optim_int64     : int64     compare = polymorphic_compare
  let optim_nativeint : nativeint compare = polymorphic_compare
  let optim_string    : string    compare = polymorphic_compare
  let optim_unit      : unit      compare = polymorphic_compare
  let optim_enum      : enum      compare = polymorphic_compare
end

module Equal = struct
  let optim_bool      : bool      equal = polymorphic_equal
  let optim_char      : char      equal = polymorphic_equal
  let optim_float     : float     equal = polymorphic_equal
  let optim_int       : int       equal = polymorphic_equal
  let optim_int32     : int32     equal = polymorphic_equal
  let optim_nativeint : nativeint equal = polymorphic_equal
  let optim_string    : string    equal = polymorphic_equal
  let optim_unit      : unit      equal = polymorphic_equal
  let optim_enum      : enum      equal = polymorphic_equal

  let optim_int64 =
    if Sys.word_size = 32 then
      (* On 32bits, polymmorphic comparison of int64 values is not specialized *)
      (fun _ _ -> false)
    else
      (polymorphic_equal : int64 equal)
end
