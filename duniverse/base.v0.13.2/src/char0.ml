(* [Char0] defines char functions that are primitives or can be simply defined in terms of
   [Caml.Char].  [Char0] is intended to completely express the part of [Caml.Char] that
   [Base] uses -- no other file in Base other than char0.ml should use [Caml.Char].
   [Char0] has few dependencies, and so is available early in Base's build order.  All
   Base files that need to use chars and come before [Base.Char] in build order should do
   [module Char = Char0].  Defining [module Char = Char0] is also necessary because it
   prevents ocamldep from mistakenly causing a file to depend on [Base.Char]. *)

open! Import0

let failwithf = Printf.failwithf
let escaped = Caml.Char.escaped
let lowercase = Caml.Char.lowercase_ascii
let to_int = Caml.Char.code
let unsafe_of_int = Caml.Char.unsafe_chr
let uppercase = Caml.Char.uppercase_ascii

(* We use our own range test when converting integers to chars rather than
   calling [Caml.Char.chr] because it's simple and it saves us a function call
   and the try-with (exceptions cost, especially in the world with backtraces). *)
let int_is_ok i = 0 <= i && i <= 255
let min_value = unsafe_of_int 0
let max_value = unsafe_of_int 255
let of_int i = if int_is_ok i then Some (unsafe_of_int i) else None

let of_int_exn i =
  if int_is_ok i
  then unsafe_of_int i
  else failwithf "Char.of_int_exn got integer out of range: %d" i ()
;;

let equal (t1 : char) t2 = Poly.equal t1 t2
