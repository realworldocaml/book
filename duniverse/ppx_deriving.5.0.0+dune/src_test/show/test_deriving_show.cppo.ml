open OUnit2

let printer = fun x -> x

type a1 = int        [@@deriving show]
type a2 = int32      [@@deriving show]
type a3 = int64      [@@deriving show]
type a4 = nativeint  [@@deriving show]
type a5 = float      [@@deriving show]
type a6 = bool       [@@deriving show]
type a7 = char       [@@deriving show]
type a8 = string     [@@deriving show]
type a9 = bytes      [@@deriving show]
type r  = int ref    [@@deriving show]
type r2 = int Pervasives.ref [@@ocaml.warning "-3"] [@@deriving show]
type r3 = int Pervasives.ref ref [@@ocaml.warning "-3"] [@@deriving show]
type l  = int list   [@@deriving show]
type a  = int array  [@@deriving show]
type o  = int option [@@deriving show]
type f  = int -> int [@@deriving show]
type y  = int lazy_t [@@deriving show]
let test_alias ctxt =
  assert_equal ~printer "1"       (show_a1 1);
  assert_equal ~printer "1l"      (show_a2 1l);
  assert_equal ~printer "1L"      (show_a3 1L);
  assert_equal ~printer "1n"      (show_a4 1n);
  assert_equal ~printer "1."      (show_a5 1.);
  assert_equal ~printer "true"    (show_a6 true);
  assert_equal ~printer "'a'"     (show_a7 'a');
  assert_equal ~printer "\"foo\"" (show_a8 "foo");
  assert_equal ~printer "\"foo\"" (show_a9 (Bytes.of_string "foo"));
  assert_equal ~printer "ref (1)" (show_r (ref 1));
  assert_equal ~printer "ref (1)" (show_r2 (ref 1));
  assert_equal ~printer "ref (ref (1))" (show_r3 (ref (ref 1)));
  assert_equal ~printer "[1; 2; 3]" (show_l [1;2;3]);
  assert_equal ~printer "[|1; 2; 3|]" (show_a [|1;2;3|]);
  assert_equal ~printer "(Some 1)" (show_o (Some 1));
  assert_equal ~printer "<fun>"   (show_f (fun x -> x));
  let y = lazy (1 + 1) in
  assert_equal ~printer "<not evaluated>" (show_y y);
  ignore (Lazy.force y);
  assert_equal ~printer "2" (show_y y)

type v = Foo | Bar of int * string | Baz of string [@@deriving show]
let test_variant ctxt =
  assert_equal ~printer "Test_deriving_show.Foo"                (show_v Foo);
  assert_equal ~printer "(Test_deriving_show.Bar (1, \"foo\"))" (show_v (Bar (1, "foo")));
  assert_equal ~printer "(Test_deriving_show.Baz \"foo\")"      (show_v (Baz "foo"))

#if OCAML_VERSION >= (4, 03, 0)
type rv = RFoo | RBar of { x: int; y: string } | RBaz of { z: string } [@@deriving show]
let test_variant_record ctxt =
  assert_equal ~printer "Test_deriving_show.RFoo"
                        (show_rv RFoo);
  assert_equal ~printer "Test_deriving_show.RBar {x = 1; y = \"foo\"}"
                        (show_rv (RBar {x=1; y="foo"}));
  assert_equal ~printer "(Test_deriving_show.RBaz {z = \"foo\"}"
                        (show_rv (RBaz {z="foo"}))
#endif

type vn = Foo of int option [@@deriving show]
let test_variant_nest ctxt =
  assert_equal ~printer "(Test_deriving_show.Foo (Some 1))" (show_vn (Foo (Some 1)))

type pv1 = [ `Foo | `Bar of int * string ] [@@deriving show]
let test_poly ctxt =
  assert_equal ~printer "`Foo"                (show_pv1 `Foo);
  assert_equal ~printer "`Bar ((1, \"foo\"))" (show_pv1 (`Bar (1, "foo")))

type pv2 = [ `Baz | pv1 ] [@@deriving show]
let test_poly_inherit ctxt =
  assert_equal ~printer "`Foo" (show_pv2 `Foo);
  assert_equal ~printer "`Baz" (show_pv2 `Baz)

type ty = int * string [@@deriving show]
let test_tuple ctxt =
  assert_equal ~printer "(1, \"foo\")" (show_ty (1, "foo"))

type re = {
  f1 : int;
  f2 : string;
  f3 : float [@opaque];
} [@@deriving show]
let test_record ctxt =
  assert_equal ~printer "{ Test_deriving_show.f1 = 1; f2 = \"foo\"; f3 = <opaque> }"
                        (show_re { f1 = 1; f2 = "foo"; f3 = 1.0 })

#if OCAML_VERSION >= (4, 03, 0)
type variant = Foo of {
  f1 : int;
  f2 : string;
  f3 : float [@opaque];
} [@@deriving show]
let test_variant_record ctxt =
  assert_equal ~printer
    "Test_deriving_show.Foo {f1 = 1; f2 = \"foo\"; f3 = <opaque>}"
    (show_variant (Foo { f1 = 1; f2 = "foo"; f3 = 1.0 }))
#endif


module M : sig
  type t = A [@@deriving show]
end = struct
  type t = A [@@deriving show]
end

let test_module ctxt =
  assert_equal ~printer "Test_deriving_show.M.A" (M.show M.A)

type z = M.t [@@deriving show]
let test_abstr ctxt =
  assert_equal ~printer "Test_deriving_show.M.A" (show_z M.A)

type file = {
  name : string;
  perm : int     [@printer fun fmt -> Format.fprintf fmt "0o%03o"];
}
[@@deriving show]
let test_custom ctxt =
  assert_equal ~printer "{ Test_deriving_show.name = \"dir\"; perm = 0o755 }"
                        (show_file { name = "dir"; perm = 0o755 })

type 'a pt = { v : 'a } [@@deriving show]
let test_parametric ctxt =
  assert_equal ~printer "{ Test_deriving_show.v = 1 }"
                        (show_pt (fun fmt -> Format.fprintf fmt "%d") { v = 1 })

type 'a btree = Node of 'a btree * 'a * 'a btree | Leaf
[@@deriving show]

module M' = struct
  type t = M.t = A [@@deriving show]
end
let test_alias_path ctxt =
  assert_equal ~printer "M.A" (M'.show M'.A)

let print_hi = fun fmt _ -> Format.fprintf fmt "hi!"
type polypr = (string [@printer print_hi]) btree [@polyprinter pp_btree]
[@@deriving show]
let test_polypr ctxt =
  assert_equal ~printer "(Test_deriving_show.Node (Test_deriving_show.Leaf, hi!,\n\
                        \   Test_deriving_show.Leaf))"
                        (show_polypr (Node (Leaf, "x", Leaf)))

let test_placeholder ctxt =
  assert_equal ~printer "_" ([%show: _] 1)

module rec RecFoo : sig
  type ('a,'b) t = ('b, 'a) RecBar.t [@@deriving show]
end = struct
  type ('a,'b) t = ('b,'a) RecBar.t [@@deriving show]
end
and RecBar : sig
  type ('b, 'a) t = 'b * 'a [@@deriving show]
end = struct
  type ('b,'a) t = 'b * 'a [@@deriving show]
end


type foo = F of int | B of int bar | C of float bar
and 'a bar = { x : 'a ; r : foo }
[@@deriving show]

let test_mrec ctxt =
  let e1 =  B { x = 12; r = F 16 } in
  assert_equal ~printer "(Test_deriving_show.B\n   { Test_deriving_show.x = 12; r = (Test_deriving_show.F 16) })" (show_foo e1)


#if OCAML_VERSION >= (4, 03, 0)
type i_has_result = I_has of (bool, string) result [@@deriving show]

let test_result ctxt =
  assert_equal ~printer "(Ok 100)"
    ([%show: (int, bool) result] (Ok 100));
  assert_equal ~printer "(Test_deriving_show.I_has (Ok true))"
    (show_i_has_result (I_has (Ok true)));
  assert_equal ~printer "(Test_deriving_show.I_has (Error \"err\"))"
    (show_i_has_result (I_has (Error "err")))
#endif

type i_has_result_result = I_has of (bool, string) Result.result [@@deriving show]

let test_result_result ctxt =
  let open Result in
  assert_equal ~printer "(Ok 100)"
    ([%show: (int, bool) result] (Result.Ok 100));
  assert_equal ~printer "(Test_deriving_show.I_has (Ok true))"
    (show_i_has_result_result (I_has (Ok true)));
  assert_equal ~printer "(Test_deriving_show.I_has (Error \"err\"))"
    (show_i_has_result_result (I_has (Error "err")))

type es =
  | ESBool of (bool [@nobuiltin])
  | ESString of (string [@nobuiltin])
and bool =
  | Bfoo of int * (int -> int)
and string =
  | Sfoo of String.t * (int -> int)
[@@deriving show]

let test_std_shadowing ctxt =
  let e1 = ESBool (Bfoo (1, (+) 1)) in
  let e2 = ESString (Sfoo ("lalala", (+) 3)) in
  assert_equal ~printer
    "(Test_deriving_show.ESBool (Test_deriving_show.Bfoo (1, <fun>)))"
    (show_es e1);
  assert_equal ~printer
    "(Test_deriving_show.ESString (Test_deriving_show.Sfoo (\"lalala\", <fun>)))"
    (show_es e2)

type poly_app = float poly_abs
and 'a poly_abs = 'a
[@@deriving show]

let test_poly_app ctxt =
  assert_equal ~printer "1." (show_poly_app 1.0)

module List = struct
  type 'a t = [`Cons of 'a | `Nil]
  [@@deriving show]
end
type 'a std_clash = 'a List.t option
[@@deriving show]

type variant_printer =
  | First [@printer fun fmt _ -> Format.pp_print_string fmt "first"]
  | Second of int [@printer fun fmt i -> fprintf fmt "second: %d" i]
  | Third
  | Fourth of int * int
                [@printer fun fmt (a,b) -> fprintf fmt "fourth: %d %d" a b]
[@@deriving show]

let test_variant_printer ctxt =
  assert_equal ~printer
    "first" (show_variant_printer First);
  assert_equal ~printer
    "second: 42" (show_variant_printer (Second 42));
  assert_equal ~printer
    "Test_deriving_show.Third" (show_variant_printer Third);
  assert_equal ~printer
    "fourth: 8 4" (show_variant_printer (Fourth(8,4)))

type no_full    = NoFull   of int [@@deriving show { with_path = false }]
type with_full  = WithFull of int [@@deriving show { with_path = true  }]
module WithFull = struct
  type t = A of int [@@deriving show ]
end
let test_paths_printer ctxt =
  assert_equal ~printer "(NoFull 1)"   (show_no_full   (NoFull 1));
  assert_equal ~printer "(Test_deriving_show.WithFull 1)" (show_with_full (WithFull 1));
  assert_equal ~printer "(Test_deriving_show.WithFull.A 1)" (WithFull.show (WithFull.A 1));
  ()

let suite = "Test deriving(show)" >::: [
    "test_alias"           >:: test_alias;
    "test_variant"         >:: test_variant;
    "test_variant_nest"    >:: test_variant_nest;
    "test_tuple"           >:: test_tuple;
    "test_poly"            >:: test_poly;
    "test_poly_inherit"    >:: test_poly_inherit;
    "test_record"          >:: test_record;
#if OCAML_VERSION >= (4, 03, 0)
    "test_variant_record"  >:: test_variant_record;
#endif
    "test_abstr"           >:: test_abstr;
    "test_custom"          >:: test_custom;
    "test_parametric"      >:: test_parametric;
    "test_alias_path"      >:: test_alias_path;
    "test_polypr"          >:: test_polypr;
    "test_placeholder"     >:: test_placeholder;
    "test_mrec"            >:: test_mrec;
    "test_std_shadowing"   >:: test_std_shadowing;
    "test_poly_app"        >:: test_poly_app;
    "test_variant_printer" >:: test_variant_printer;
    "test_paths"           >:: test_paths_printer;
#if OCAML_VERSION >= (4, 03, 0)
    "test_result"          >:: test_result;
#endif
    "test_result_result"   >:: test_result_result;
  ]

let _ = run_test_tt_main suite
