# The Num library for arithmetic on big integers and rationals

## Overview

This library implements arbitrary-precision arithmetic on big integers and on rationals.  

This is a  legacy library.  It used to be part of the core OCaml distribution (in <code>otherlibs/num</code>) but is now distributed separately.  New applications that need arbitrary-precision arithmetic should use the Zarith library (https://github.com/ocaml/Zarith) instead of the Num library, and older applications that already use Num are encouraged to switch to Zarith.  Zarith delivers much better performance than Num and has a nicer API.

## Usage

To use the bignum library from your programs, it is recommended to use ocamlfind:
```
    ocamlfind ocamlc -package num ...
    ocamlfind ocamlopt -package num ...
```
Alternatively, you can do
```
    ocamlc <options> nums.cma <.cmo and .ml files>
    ocamlopt <options> nums.cmxa <.cmx and .ml files>
```
For toplevel use, just issue the commands
```
    #use "topfind";;
    #package "num";;
```
or
```
    #load "nums.cma";;
```

## Documentation

The following modules are documented in their interfaces:

* `Big_int`: operations on arbitrary-precision integers
* `Num`: operations on arbitrary-precision numbers (integers and rationals)
* `Arith_status`: flags that control rational arithmetic

More documentation on the functions provided in this library can be found in _The CAML Numbers Reference Manual_ by Valérie Ménissier-Morain, INRIA technical report 141, july 1992, http://hal.inria.fr/docs/00/07/00/27/PDF/RT-0141.pdf

## Compilation and installation

Prerequisites: OCaml version 4.04 or newer.
```
        make all
        make test
        make install
        make clean
```

## History

This library is derived from Valérie Ménissier-Morain's implementation of rational arithmetic for Caml V3.1.  Xavier Leroy did the Caml Light port.  Victor Manuel Gulias Fernandez did the initial Caml Special Light / OCaml port.  Pierre Weis did most of the maintenance and bug fixing.

Initially, the low-level big integer operations were provided by the BigNum package developed by Bernard Serpette, Jean Vuillemin and Jean-Claude Hervé (INRIA and Digital PRL).  License issues forced us to replace the BigNum package.  The current implementation of low-level big integer operations is due to Xavier Leroy.

