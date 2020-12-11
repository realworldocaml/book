open! Base
open! Expect_test_helpers_base

let test = function
  | Sexp.Private.Raw_grammar.Ref (_, group) -> print_endline group.origin
  | Inline _                                -> print_cr [%here] (Atom "Unexpected [Inline]")
;;

type t = unit [@@deriving sexp_grammar]

let extension_node = [%sexp_grammar: int]

let%expect_test "toplevel" =
  test t_sexp_grammar;
  [%expect {| test_origin.ml |}];
  test extension_node;
  [%expect {| test_origin.ml |}]
;;

module Foo = struct
  type t = unit [@@deriving sexp_grammar]

  let extension_node = [%sexp_grammar: unit]
end

let%expect_test "[Foo]" =
  test Foo.t_sexp_grammar;
  [%expect {| test_origin.ml.Foo |}];
  test Foo.extension_node;
  [%expect {| test_origin.ml.Foo |}]
;;

module F (M : sig
    type t [@@deriving sexp_grammar]
  end) =
struct
  type t = M.t [@@deriving sexp_grammar]

  let extension_node = [%sexp_grammar: M.t]
end

module F_foo = F (Foo)

(* Because the origin is generated at the type definition, it points to the functor
   definition rather than the functor application. *)
let%expect_test "[F_foo]" =
  test F_foo.t_sexp_grammar;
  [%expect {| test_origin.ml.F |}];
  test F_foo.extension_node;
  [%expect {| test_origin.ml.F |}]
;;

let m__t_sexp_grammar, m__extension_node =
  let module M = struct
    type t = unit [@@deriving sexp_grammar]

    let extension_node = [%sexp_grammar: unit]
  end
  in
  M.t_sexp_grammar, M.extension_node
;;

(* ppx_deriving omits modules defined in expressions from the [path].

   Maybe there's not a good way to bring the module name out of that scope. E.g., how to
   disambiguate multiple evaluations of that expression? *)
let%expect_test "modules in expressions" =
  test m__t_sexp_grammar;
  [%expect {| test_origin.ml |}];
  test m__extension_node;
  [%expect {| test_origin.ml |}]
;;
