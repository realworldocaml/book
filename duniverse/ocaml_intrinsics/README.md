ocaml_intrinsics - a library of intrinsics for OCaml
====================================================

The ocaml_intrinsics library provides an OCaml interface to operations
that have dedicated hardware instructions on some micro-architectures.
Currently, it provides the following operations:
* count leading zeros of an integer
* count trailing zeros of an integer
* count set bits of an integer
* performance monitoring
* cyclic redundancy check

Some operations require more than one instruction to implement.

On x86_64 architecture, the integer operations are implemented
using `bsf`, `bsr`, and `popcnt` instructions.  Additionally, the
library supports performance monitoring instructions `rdtsc` and
`rdpmc`, as well as `crc32` for the x86_64 architecture only.

The library provides default implementations using C stubs for all
targets.  Ocaml compiler with support for intrinsics intercepts calls
to these C stubs and emits inline assembly instead.

The library and the compiler support was only tested on x86_64 Linux
target.

The goals of the library are to:

    * Expose a portable interface with uniform semantics on all OCaml targets.
      Under the hood, take care of target-specific
      details, including C compiler, operating
      system, and machine architecture support.

    * Reduce the overhead of OCaml-to-C calls in the implemention,
      whenever possible, using attributes such as "noalloc", "unboxed", and
      "untagged". Provides variants of operations, such as tagged
      and untagged integers, and handle corner cases.
      The users can choose the most suitable variant in each context.

    * Make it easier to use dedicated hardware
      instructions, and evaluate their effects on performance.

    * Make it easier to experiment with compiler intrinsics.
      The main difference in generated code
      between this library's C stubs and compiler intrinsics
      is that C stubs cannot be inlined.
      It is not clear whether "inlining" them would noticibly improve
      performance compared to a direct call.
      In particular, inlining might also increase register
      pressure in the caller, especially when new instruction
      require designated registers as operands (such as rdtsc).
      Compiler support for intrinsics
      detects and replaces calls to C stubs annoted with [@@builtin]
      with the corresponding instructions, on targets that support them.

    * Make it easier for the users to transition their code
      from using this library to using compiler intrinsics,
      whenever the corresponding operation become available in the
      upstream OCaml compiler.
