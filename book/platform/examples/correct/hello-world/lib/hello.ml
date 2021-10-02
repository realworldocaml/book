(* This file is the implementation of the Hello module.

   The interface (aka exposed API, or signature) of the module is the hello.mli
   counterpart.

   When adding or changing the signature of a function here, the changes must be
   reflected in the module interface as well. *)

(* This defines a function named "greet" that takes an argument "name".

   The infix operator "^" concatenates two strings, so the function will return
   a new string "Hello {name}!" where "{name}" is the value of the name
   argument. *)
let greet name = "Hello " ^ name ^ "!"
