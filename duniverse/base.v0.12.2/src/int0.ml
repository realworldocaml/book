(* [Int0] defines integer functions that are primitives or can be simply
   defined in terms of [Caml]. [Int0] is intended to completely express the
   part of [Caml] that [Base] uses for integers -- no other file in Base other
   than int0.ml should use these functions directly through [Caml]. [Int0] has
   few dependencies, and so is available early in Base's build order.

   All Base files that need to use ints and come before [Base.Int] in build
   order should do:

   {[
     module Int  = Int0
   ]}

   Defining [module Int = Int0] is also necessary because it prevents ocamldep
   from mistakenly causing a file to depend on [Base.Int]. *)

let to_string = Caml.string_of_int
let of_string = Caml.int_of_string
let to_float  = Caml.float_of_int
let of_float  = Caml.int_of_float
let max_value = Caml.max_int
let min_value = Caml.min_int
let succ      = Caml.succ
