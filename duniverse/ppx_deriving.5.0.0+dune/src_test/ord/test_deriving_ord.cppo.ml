open OUnit2

(* Mostly it is sufficient to test that the derived code compiles. *)

let printer = string_of_int

type a1 = int        [@@deriving ord]
type a2 = int32      [@@deriving ord]
type a3 = int64      [@@deriving ord]
type a4 = nativeint  [@@deriving ord]
type a5 = float      [@@deriving ord]
type a6 = bool       [@@deriving ord]
type a7 = char       [@@deriving ord]
type a8 = string     [@@deriving ord]
type a9 = bytes      [@@deriving ord]
type l  = int list   [@@deriving ord]
type a  = int array  [@@deriving ord]
type o  = int option [@@deriving ord]
type y  = int lazy_t [@@deriving ord]

let test_simple ctxt =
  assert_equal ~printer  (1) (compare_a1 1 0);
  assert_equal ~printer  (0) (compare_a1 1 1);
  assert_equal ~printer (-1) (compare_a1 1 2)

type v = Foo | Bar of int * string | Baz of string [@@deriving ord]
let test_variant ctxt =
  assert_equal ~printer (1) (compare_v (Baz "b") (Baz "a"));
  assert_equal ~printer (1) (compare_v (Bar (1, "")) Foo);
  assert_equal ~printer (1) (compare_v (Baz "") (Bar (1, "")));
  assert_equal ~printer (-1) (compare_v Foo (Baz ""))

#if OCAML_VERSION >= (4, 03, 0)
type rv = RFoo | RBar of { x: int; y: string; } [@@deriving ord]
#endif

type pv1 = [ `Foo | `Bar of int * string ] [@@deriving ord]
type pv2 = [ `Baz | pv1 ] [@@deriving ord]

type ty = int * string [@@deriving ord]
let test_complex ctxt =
  assert_equal ~printer (0)  (compare_ty (0, "a") (0, "a"));
  assert_equal ~printer (1)  (compare_ty (1, "a") (0, "a"));
  assert_equal ~printer (-1) (compare_ty (0, "a") (1, "a"));
  assert_equal ~printer (-1) (compare_ty (0, "a") (0, "b"));
  assert_equal ~printer (1)  (compare_ty (0, "b") (0, "a"))


type re = {
  f1 : int;
  f2 : string;
} [@@deriving ord]

module M : sig
  type t = int [@@deriving ord]
end = struct
  type t = int [@@deriving ord]
end

type z = M.t [@@deriving ord]

type file = {
  name : string;
  perm : int     [@compare fun a b -> compare b a];
} [@@deriving ord]
let test_custom ctxt =
  assert_equal ~printer (-1) (compare_file { name = ""; perm = 2 }
                                           { name = ""; perm = 1 });
  assert_equal ~printer (1)  (compare_file { name = ""; perm = 1 }
                                           { name = ""; perm = 2 })

type 'a pt = { v : 'a } [@@deriving ord]

let test_placeholder ctxt =
  assert_equal ~printer 0 ([%ord: _] 1 2)

type mrec_variant =
  | MrecFoo of string
  | MrecBar of int

and mrec_variant_list = mrec_variant list
[@@deriving ord]

let test_mrec ctxt =
  assert_equal ~printer (0)   (compare_mrec_variant_list [MrecFoo "foo"; MrecBar 1;]
                                                         [MrecFoo "foo"; MrecBar 1;]);
  assert_equal ~printer (-1)  (compare_mrec_variant_list [MrecFoo "foo"; MrecBar 1;]
                                                         [MrecFoo "foo"; MrecBar 2;]);
  assert_equal ~printer (1)   (compare_mrec_variant_list [MrecFoo "foo"; MrecBar 2;]
                                                         [MrecFoo "foo"; MrecBar 1;])

type e = Bool of be | Plus of e * e | IfE  of (be, e) if_e
and be = True | False | And of be * be | IfB of (be, be) if_e
and ('cond, 'a) if_e = 'cond * 'a * 'a
  [@@deriving ord]

let test_mrec2 ctxt =
  let ce1 = Bool (IfB (True, False, True)) in
  let ce2 = Bool (IfB (True, False, False)) in
  assert_equal ~printer (0) (compare_e ce1 ce1);
  assert_equal ~printer (-1) (compare_e ce1 ce2);
  assert_equal ~printer (1) (compare_e ce2 ce1)

#if OCAML_VERSION >= (4, 03, 0)
let test_ord_result ctx =
  let compare_res0 = [%ord: (unit, unit) result] in
  assert_equal ~printer 0 (compare_res0 (Ok ()) (Ok ()));
  assert_equal ~printer (-1) (compare_res0 (Ok ()) (Error ()));
  assert_equal ~printer 1 (compare_res0 (Error ()) (Ok ()))
#endif

let test_ord_result_result ctx =
  let compare_res0 = [%ord: (unit, unit) Result.result] in
  let open Result in
  assert_equal ~printer 0 (compare_res0 (Ok ()) (Ok ()));
  assert_equal ~printer (-1) (compare_res0 (Ok ()) (Error ()));
  assert_equal ~printer 1 (compare_res0 (Error ()) (Ok ()))

type r1 = int ref [@@deriving ord]
let test_ref1 ctxt =
  assert_equal ~printer (-1) (compare_r1 (ref 0) (ref 1));
  assert_equal ~printer (0) (compare_r1 (ref 0) (ref 0));
  assert_equal ~printer (1) (compare_r1 (ref 1) (ref 0))

type r2 = int Pervasives.ref
[@@ocaml.warning "-3"]
[@@deriving ord]
let test_ref2 ctxt =
  assert_equal ~printer (-1) (compare_r2 (ref 0) (ref 1));
  assert_equal ~printer (0) (compare_r2 (ref 0) (ref 0));
  assert_equal ~printer (1) (compare_r2 (ref 1) (ref 0))

type es =
  | ESBool of bool
  | ESString of string
and bool =
  | Bfoo of int * ((int -> int) [@compare fun _ _ -> 0])
and string =
  | Sfoo of String.t * ((int -> int) [@compare fun _ _ -> 0])
[@@deriving ord]

let test_std_shadowing ctxt =
  let e1 = ESBool (Bfoo (1, (+) 1)) in
  let e2 = ESString (Sfoo ("lalala", (+) 3)) in
  assert_equal ~printer (-1) (compare_es e1 e2);
  assert_equal ~printer (1) (compare_es e2 e1);
  assert_equal ~printer 0 (compare_es e1 e1);
  assert_equal ~printer 0 (compare_es e2 e2)

type poly_app = float poly_abs
and 'a poly_abs = 'a
[@@deriving ord]

let test_poly_app ctxt =
  assert_equal ~printer 0 (compare_poly_app 1.0 1.0);
  assert_equal ~printer (-1) (compare_poly_app 1.0 2.0)

module List = struct
  type 'a t = [`Cons of 'a | `Nil]
  [@@deriving ord]
end
type 'a std_clash = 'a List.t option
[@@deriving ord]

module Warnings = struct
  module W4 = struct
    [@@@ocaml.warning "@4"]

    type t =
      | A of int
      | B
    [@@deriving ord]
  end
end

type ab = { a : int; b : int } [@@deriving ord]
let test_record_order ctxt =
  assert_equal ~printer (-1) (compare_ab { a = 1; b = 2; } { a = 2; b = 1; });
  assert_equal ~printer (0) (compare_ab { a = 1; b = 2; } { a = 1; b = 2; });
  assert_equal ~printer (1) (compare_ab { a = 2; b = 2; } { a = 1; b = 2; })

let suite = "Test deriving(ord)" >::: [
    "test_simple"        >:: test_simple;
    "test_variant"       >:: test_variant;
    "test_complex"       >:: test_complex;
    "test_custom"        >:: test_custom;
    "test_placeholder"   >:: test_placeholder;
    "test_mrec"          >:: test_mrec;
    "test_mrec2"         >:: test_mrec2;
    "test_record_order"  >:: test_record_order;
    "test_ref1"          >:: test_ref1;
    "test_ref2"          >:: test_ref2;
    "test_std_shadowing" >:: test_std_shadowing;
    "test_poly_app"      >:: test_poly_app;
#if OCAML_VERSION >= (4, 03, 0)
    "test_ord_result"    >:: test_ord_result;
#endif
    "test_ord_result_result" >:: test_ord_result_result;
  ]

let _ = run_test_tt_main suite
