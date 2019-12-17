let test_ref_included
  (x : 'a ref)
  =
  (x : 'a Ppx_deriving_runtime.ref)

let test_ref_qualified
  (x : 'a ref)
  =
  (x : 'a Ppx_deriving_runtime.Pervasives.ref[@ocaml.warning "-3"])

let test_backtrace
  (x : Printexc.raw_backtrace)
  =
  (x : Ppx_deriving_runtime.Printexc.raw_backtrace)

let test_hashtbl
  (x : ('a, 'b) Hashtbl.t)
  =
  (x : ('a, 'b) Ppx_deriving_runtime.Hashtbl.t)

let test_result_qualified
  (x : ('a, 'b) Result.result)
  =
  (x : ('a, 'b) Ppx_deriving_runtime.Result.t)

#if OCAML_VERSION >= (4, 06, 0)
let test_result_included
  (x : ('a, 'b) result)
  =
  (x : ('a, 'b) Ppx_deriving_runtime.Result.t)
#endif

#if OCAML_VERSION >= (4, 07, 0)
let test_result_in_stdlib
  (x : ('a, 'b) Stdlib.result)
  =
  (x : ('a, 'b) Ppx_deriving_runtime.Result.t)
#endif

