open OUnit2

(* Mostly it is sufficient to test that the derived code compiles. *)

let printer = string_of_bool

type a1 = int        [@@deriving eq]
type a2 = int32      [@@deriving eq]
type a3 = int64      [@@deriving eq]
type a4 = nativeint  [@@deriving eq]
type a5 = float      [@@deriving eq]
type a6 = bool       [@@deriving eq]
type a7 = char       [@@deriving eq]
type a8 = string     [@@deriving eq]
type a9 = bytes      [@@deriving eq]
type r1 = int ref    [@@deriving eq]
type r2 = int Pervasives.ref [@@ocaml.warning "-3"][@@deriving eq]
type l  = int list   [@@deriving eq]
type a  = int array  [@@deriving eq]
type o  = int option [@@deriving eq]
type y  = int lazy_t [@@deriving eq]

let test_simple ctxt =
  assert_equal ~printer true  (equal_a1 1 1);
  assert_equal ~printer false (equal_a1 1 2)

let test_arr ctxt =
  assert_equal ~printer true (equal_a [||] [||]);
  assert_equal ~printer true (equal_a [|1|] [|1|]);
  assert_equal ~printer false (equal_a [||] [|1|]);
  assert_equal ~printer false (equal_a [|2|] [|1|])

let test_ref1 ctxt =
  assert_equal ~printer true (equal_r1 (ref 0) (ref 0))

let test_ref2 ctxt =
  assert_equal ~printer true (equal_r2 (ref 0) (ref 0))

type v = Foo | Bar of int * string | Baz of string [@@deriving eq]

#if OCAML_VERSION >= (4, 03, 0)
type rv = RFoo | RBar of { x: int; y: string; } [@@deriving eq]
#endif

type pv1 = [ `Foo | `Bar of int * string ] [@@deriving eq]
type pv2 = [ `Baz | pv1 ] [@@deriving eq]

type ty = int * string [@@deriving eq]

type re = {
  f1 : int;
  f2 : string;
} [@@deriving eq]

module M : sig
  type t = int [@@deriving eq]
end = struct
  type t = int [@@deriving eq]
end

type z = M.t [@@deriving eq]

type file = {
  name : string;
  perm : int     [@equal (<>)];
} [@@deriving eq]
let test_custom ctxt =
  assert_equal ~printer false (equal_file { name = ""; perm = 1 }
                                          { name = ""; perm = 1 });
  assert_equal ~printer true  (equal_file { name = ""; perm = 1 }
                                          { name = ""; perm = 2 })

type 'a pt = { v : 'a } [@@deriving eq]

let test_placeholder ctxt =
  assert_equal ~printer true ([%eq: _] 1 2)


type mrec_variant =
  | MrecFoo of string
  | MrecBar of int

and mrec_variant_list = mrec_variant list [@@deriving eq]

let test_mrec ctxt =
  assert_equal ~printer true  (equal_mrec_variant_list [MrecFoo "foo"; MrecBar 1]
                                                       [MrecFoo "foo"; MrecBar 1]);
  assert_equal ~printer false (equal_mrec_variant_list [MrecFoo "foo"; MrecBar 1]
                                                       [MrecFoo "bar"; MrecBar 1])

type e = Bool of be | Plus of e * e | IfE  of (be, e) if_e | Unit
and be = True | False | And of be * be | IfB of (be, be) if_e
and ('cond, 'a) if_e = 'cond * 'a * 'a
  [@@deriving eq]

let test_mut_rec ctxt =
  let e1 = IfE (And (False, True), Unit, Plus (Unit, Unit)) in
  let e2 = Plus (Unit, Bool False) in
  assert_equal ~printer true (equal_e e1 e1);
  assert_equal ~printer true (equal_e e2 e2);
  assert_equal ~printer false (equal_e e1 e2);
  assert_equal ~printer false (equal_e e2 e1)

type es =
  | ESBool of (bool [@nobuiltin])
  | ESString of (string [@nobuiltin])
and bool =
  | Bfoo of int * ((int -> int) [@equal fun _ _ -> true])
and string =
  | Sfoo of (String.t [@equal (=)]) * ((int -> int) [@equal fun _ _ -> true])
[@@deriving eq]

let test_std_shadowing ctxt =
  let e1 = ESBool (Bfoo (1, (+) 1)) in
  let e2 = ESString (Sfoo ("lalala", (+) 3)) in
  assert_equal ~printer false (equal_es e1 e2);
  assert_equal ~printer false (equal_es e2 e1);
  assert_equal ~printer true (equal_es e1 e1);
  assert_equal ~printer true (equal_es e2 e2)

type poly_app = float poly_abs
and 'a poly_abs = 'a
[@@deriving eq]

let test_poly_app ctxt =
  assert_equal ~printer true (equal_poly_app 1.0 1.0);
  assert_equal ~printer false (equal_poly_app 1.0 2.0)

module List = struct
  type 'a t = [`Cons of 'a | `Nil]
  [@@deriving eq]
end
type 'a std_clash = 'a List.t option
[@@deriving eq]

#if OCAML_VERSION >= (4, 03, 0)
let test_result ctxt =
  let eq = [%eq: (string, int) result] in
  assert_equal ~printer true (eq (Ok "ttt") (Ok "ttt"));
  assert_equal ~printer false (eq (Ok "123") (Error 123));
  assert_equal ~printer false (eq (Error 123) (Error 0))
#endif

let test_result_result ctxt =
  let open Result in
  let eq = [%eq: (string, int) result] in
  assert_equal ~printer true (eq (Ok "ttt") (Ok "ttt"));
  assert_equal ~printer false (eq (Ok "123") (Error 123));
  assert_equal ~printer false (eq (Error 123) (Error 0))

let suite = "Test deriving(eq)" >::: [
    "test_simple"        >:: test_simple;
    "test_array"         >:: test_arr;
    "test_ref1"          >:: test_ref1;
    "test_ref2"          >:: test_ref2;
    "test_custom"        >:: test_custom;
    "test_placeholder"   >:: test_placeholder;
    "test_mrec"          >:: test_mrec;
    "test_mut_rec"       >:: test_mut_rec;
    "test_std_shadowing" >:: test_std_shadowing;
    "test_poly_app"      >:: test_poly_app;
#if OCAML_VERSION >= (4, 03, 0)
    "test_result"        >:: test_result;
#endif
    "test_result_result" >:: test_result_result;
  ]

let _ = run_test_tt_main suite
