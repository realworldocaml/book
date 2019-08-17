# ocaml-integers

The `ocaml-integers` library provides a number of 8-, 16-, 32- and 64-bit signed and unsigned integer types, together with aliases such as `long` and `size_t` whose sizes depend on the host platform.

### Features

* The interfaces follow the pattern of the signatures of the [`Int32`][int32], [`Int64`][int64], and [`Nativeint`][nativeint] modules in the OCaml standard library.

  The behaviour also follows the standard library; for example, conversions such as `of_int` truncate, and operations are "modulo" in general:

   ```ocaml
   # Unsigned.UInt8.(pred zero);;
   - : Unsigned.UInt8.t = <uint8 255>
   ```

* Top-level printers for each type are included

   ```ocaml
   # Unsigned.UInt32.[of_int 103; one; of_string "1000"];; 
   - : Unsigned.UInt32.t list = [<uint32 103>; <uint32 1>; <uint32 1000>]
   ```

* Infix operators are available:

   ```ocaml
   # Unsigned.UInt32.(Infix.(one + one));;
   - : Unsigned.UInt32.t = <uint32 2>
   ```

* Polymorphic operations such as comparison behave correctly: 

   ```ocaml
   # open Unsigned.UInt32
   # zero < one;;
   - : bool = true
   # max_int < zero;;
   - : bool = false
   ```

* Integers 32 bits and above are boxed; integers below 32 bits are unboxed.

   ```ocaml
   # Obj.(tag (repr Unsigned.UInt32.zero));;
   - : int = 255
   # Obj.(tag (repr Unsigned.UInt16.zero));;
   - : int = 1000
   ```

[![Travis build Status](https://travis-ci.org/ocamllabs/ocaml-integers.svg?branch=master)](https://travis-ci.org/ocamllabs/ocaml-integers) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/4l1vout6fl581hgq?svg=true)](https://ci.appveyor.com/project/yallop/ocaml-integers/branch/master) 

[API documentation][doc]

[int32]: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Int32.html
[int64]: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Int64.html
[nativeint]: http://caml.inria.fr/pub/docs/manual-ocaml/libref/Nativeint.html
[doc]: https://ocamllabs.github.io/ocaml-integers/api.docdir/
