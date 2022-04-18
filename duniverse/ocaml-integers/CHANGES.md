v0.7.0 2022-03-23
-----------------

* restore dll support for cygwin64 / newer binutils versions (mingw64) (@fdopen, #40)

v0.6.0 2022-02-05
-----------------

* Add dependency on stdlib-shims (@kit-ty-kate, #38)

v0.5.1 2021-08-12
-----------------

* Restore support for pre-4.05 OCaml versions (@yallop, #37) 

v0.5.0 2021-07-31
-----------------
* Hex printing, and option-based parsing (@raphael-proust, #34)
* Add stdlib-compatible string parsing (@mrmr1993, #32)

v0.4.0 2020-05-01
-----------------
* Expose Signed.S.Infix.(asr) (@dra27, #30)

v0.3.0 2019-02-15
-----------------
* Drop 4.01 support (@yallop, #23)
* Add equal and pp to the output signatures (@emillon, #22)
* Dune port (@rgrinberg, #21)
* Add UInt64.(of|to)_uint32 (@yallop, #17)

v0.2.2 2016-12-19
-----------------
* Fix truncation in UInt64.of_int; remove other uses of `Val_int`/`Int_val`  

v0.2.1 2016-11-14
-----------------
* Register the custom deserializers

v0.2.0 2016-10-04
-----------------
* Expose from_byte_size functions in Unsigned and Signed
* Support for platforms where standard integer types are macros
* Add 'max' and 'min' functions to Unsigned.S.
* Expose private types for UChar, UInt8, UInt16.

0.1.0 2016-09-26
----------------
* Initial public release
