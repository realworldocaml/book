  $ ocamlfind ocamlc -package ppx_compare -package core_kernel -dsource -linkpkg comparelib_test.mli
  open Core_kernel
  type t = {
    foo: string ;
    bar: t }[@@deriving compare]
  include sig [@@@ocaml.warning "-32"] val compare : t -> t -> int end
