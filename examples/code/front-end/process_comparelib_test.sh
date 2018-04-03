  $ ocamlfind ocamlc -package ppx_compare -package core_kernel -dsource -linkpkg comparelib_test.ml
  open Core_kernel
  type t = {
    foo: string ;
    bar: t }[@@deriving compare]
  let _ = fun (_ : t) -> ()
  let rec compare =
    (fun a__001_ ->
       fun b__002_ ->
         if Ppx_compare_lib.phys_equal a__001_ b__002_
         then 0
         else
           (match compare_string a__001_.foo b__002_.foo with
            | 0 -> compare a__001_.bar b__002_.bar
            | n -> n) : t -> t -> int)
  let _ = compare
