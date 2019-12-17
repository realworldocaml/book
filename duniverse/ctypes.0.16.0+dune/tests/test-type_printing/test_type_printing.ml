(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes


module Struct_stubs = Types.Stubs(Generated_struct_bindings)


let strip_whitespace = Str.(global_replace (regexp "[\n ]+") "")

let equal_ignoring_whitespace l r =
  strip_whitespace l = strip_whitespace r

let assert_printed_as ?name format expected typ =
 assert_equal
   ~cmp:equal_ignoring_whitespace 
   ~printer:(fun s -> s)
   expected (format ?name typ)

let assert_typ_printed_as ?name e t = assert_printed_as ?name string_of_typ e t
let assert_fn_printed_as ?name e f = assert_printed_as ?name string_of_fn e f


(*
  Test the printing of atomic types: void, arithmetic types and abstract
  types.
*)
let test_atomic_printing _ =
  begin
    assert_typ_printed_as "void"
      void;

    assert_typ_printed_as ~name:"a" "char a"
      char;

    assert_typ_printed_as "signed char"
      schar;

    assert_typ_printed_as ~name:"b" "short b"
      short;

    assert_typ_printed_as "int"
      int;

    assert_typ_printed_as ~name:"c" "long c"
      long;

    assert_typ_printed_as "long long"
      llong;

    assert_typ_printed_as ~name:"d" "intnat d"
      nativeint;

    assert_typ_printed_as "int8_t"
      int8_t;

    assert_typ_printed_as ~name:"e" "int16_t e"
      int16_t;

    assert_typ_printed_as "int32_t"
      int32_t;

    assert_typ_printed_as ~name:"f" "int64_t f"
      int64_t;

    assert_typ_printed_as "unsigned char"
      uchar;

    assert_typ_printed_as "_Bool"
      bool;

    assert_typ_printed_as ~name:"g" "uint8_t g"
      uint8_t;

    assert_typ_printed_as "uint16_t"
      uint16_t;

    assert_typ_printed_as ~name:"h" "uint32_t h"
      uint32_t;

    assert_typ_printed_as "uint64_t"
      uint64_t;

    assert_typ_printed_as ~name:"i" "size_t i"
      size_t;

    assert_typ_printed_as "unsigned short"
      ushort;

    assert_typ_printed_as ~name:"j" "unsigned int j"
      uint;

    assert_typ_printed_as "unsigned long"
      ulong;

    assert_typ_printed_as ~name:"k" "unsigned long long k"
      ullong;

    assert_typ_printed_as "float"
      float;

    assert_typ_printed_as ~name:"l" "double l"
      double;

    let abs_t = abstract ~name:"abs_t" ~size:1 ~alignment:1 in

    assert_typ_printed_as "abs_t"
      abs_t;
  end


(*
  Test the printing of pointers to object and function types.
*)
let test_pointer_printing _ =
  begin
    (* Pointers to atomic types *)
    assert_typ_printed_as ~name:"a" "void *a"
      (ptr void);

    assert_typ_printed_as "unsigned long long **"
      (ptr (ptr ullong));

    assert_typ_printed_as ~name:"b" "char *****b"
      (ptr (ptr (ptr (ptr (ptr char)))));

    let abs_t = abstract ~name:"abs_t" ~size:1 ~alignment:1 in

    assert_typ_printed_as "abs_t *"
      (ptr abs_t);

    (* Pointers to incomplete structs and unions *)
    let s_incomplete = structure "s_incomplete" in 
    let u_incomplete = union "u_incomplete" in 

    assert_typ_printed_as ~name:"c" "struct s_incomplete *c"
      (ptr s_incomplete);

    assert_typ_printed_as "union u_incomplete **"
      (ptr (ptr u_incomplete));

    (* Pointers to complete structs and unions *)
    let s_complete = structure "s_complete" in
    let _ = field s_complete "i" int in
    seal s_complete;

    let u_complete = union "u_complete" in 
    let _ = field u_complete "i" int in
    seal u_complete;

    assert_typ_printed_as ~name:"d" "struct s_complete *d"
      (ptr s_complete);

    assert_typ_printed_as "union u_complete **"
      (ptr (ptr u_complete));

    (* Pointers to arrays *)
    assert_typ_printed_as ~name:"e" "int (*e)[4]"
      (ptr (array 4 int));

    assert_typ_printed_as "struct s_complete (*)[3]"
      (ptr (array 3 s_complete));

    assert_typ_printed_as ~name:"f" "union u_complete (*f)[3][4][5]"
      (ptr (array 3 (array 4 (array 5 u_complete))));

    (* Pointers to functions *)
    assert_typ_printed_as "void (*)(void)"
      (Foreign.funptr (void @-> returning void));

    assert_typ_printed_as ~name:"g" "float (*g)(int, long)"
      (Foreign.funptr (int @-> long @-> returning float));

    assert_typ_printed_as "void (*)(int (*)[4])"
      (Foreign.funptr (ptr (array 4 int) @-> returning void));

    assert_typ_printed_as ~name:"h" "int32_t (*(*h)(void ))(int)"
      (Foreign.funptr (void @-> returning (Foreign.funptr (int @-> returning int32_t))));

    assert_typ_printed_as
      "unsigned long (*(*)(int, void (*)(float, float)))(long)"
      (Foreign.funptr (int @->
               Foreign.funptr (float @-> float @-> returning void) @->
               returning (Foreign.funptr (long @-> returning ulong))));

    (* Pointers to pointers to functions *)
    assert_typ_printed_as ~name:"i" "double (**i)(int)"
      (ptr (Foreign.funptr (int @-> returning double)));

    assert_typ_printed_as "double (**)(int)"
      (ptr (Foreign.funptr (int @-> returning double)));

    assert_typ_printed_as ~name:"j" "void (*(*(*(**j)(int))(void))[8])(long, long)"
      (ptr
         (Foreign.funptr
            (int @->
             returning (Foreign.funptr
                          (void @->
                           returning (ptr
                                        (array 8
                                           (Foreign.funptr
                                              (long @->
                                               long @->
                                               returning void)))))))));
  end


(*
  Test the printing of pointers to object and function types.
*)
let test_struct_and_union_printing _ =
  begin
    (* Incomplete structs and unions *) 
    let s_incomplete = structure "s_incomplete" in 
    let u_incomplete = union "u_incomplete" in 
    
    assert_typ_printed_as ~name:"a" "struct s_incomplete a"
      s_incomplete;
    
    assert_typ_printed_as "union u_incomplete"
      u_incomplete;

    (* Structs and unions containing primitives *)
    let s_prims = structure "s_prims" in
    let (-:) ty label = field s_prims label ty in
    let _ = int   -: "i" in
    let _ = ulong -: "l" in
    let _ = float -: "z" in
    seal s_prims;

    assert_typ_printed_as ~name:"b"
                          "struct s_prims {
                              int i;
                              unsigned long l;
                              float z;
                           } b"
      s_prims;

    let u_prims = union "u_prims" in
    let (-:) ty label = field u_prims label ty in
    let _ = int32_t -: "i32" in
    let _ = int64_t -: "i64" in
    let _ = double  -: "d" in
    seal u_prims;

    assert_typ_printed_as "union u_prims {
                              int32_t i32;
                              int64_t i64;
                              double d;
                           }"
      u_prims;

    (* Structs and unions containing pointers to themselves *)
    let selfish = structure "selfish" in
    let (-:) ty label = field selfish label ty in
    let _ = ptr selfish       -: "s" in
    let _ = ptr int           -: "i" in
    let _ = ptr (ptr selfish) -: "p" in
    seal selfish;

    assert_typ_printed_as ~name:"c"
                          "struct selfish {
                              struct selfish *s;
                              int *i;
                              struct selfish **p;
                           } c"
      selfish;

    let u_selfish = union "u_selfish" in
    let (-:) ty label = field u_selfish label ty in
    let _ = ptr u_selfish       -: "self" in
    let _ = ptr (union "other") -: "other" in
    seal u_selfish;

    assert_typ_printed_as "union u_selfish {
                              union u_selfish *self;
                              union other *other;
                           }"
      u_selfish;

    (* Structs and unions containing arrays and pointers to functions *)
    let mixture = structure "mixture" in
    let (-:) ty label = field mixture label ty in
    let _ = array 10 (array 12 (ptr mixture))       -: "parr" in
    let _ = Foreign.funptr (ptr mixture @-> returning void) -: "fn" in
    let _ = int                                     -: "i" in
    seal mixture;
      
    assert_typ_printed_as ~name:"d" 
                          "struct mixture {
                              struct mixture *parr[10][12];
                              void (*fn)(struct mixture *);
                              int i;
                           } d"
      mixture;

    let u_mixture = union "u_mixture" in
    let (-:) ty label = field u_mixture label ty in
    let _ = float -: "fl" in
    let _ = ptr (array 3
                   (Foreign.funptr
                      (float @-> returning float)))
                  -: "p" in
    seal u_mixture;
      
    assert_typ_printed_as ~name:"e"
                          "union u_mixture {
                              float fl;
                              float (*(*p)[3])(float);
                           } e"
      u_mixture;
      
    (* Structs and unions containing struct and union members *)
    let inner_s = structure "inner_s" in
    let _ = field inner_s "_" int in
    seal inner_s;

    let inner_u = union "inner_u" in
    let _ = field inner_u "_" int in
    seal inner_u;

    let anon_s = structure "" in
    let _ = field anon_s "a" int in
    seal anon_s;

    let anon_u = union "" in
    let _ = field anon_u "b" int in
    seal anon_u;

    let struct_containing_struct = structure "scs" in
    let _ = field struct_containing_struct "inner" inner_s in
    seal struct_containing_struct;

    let union_containing_struct = union "ucs" in
    let _ = field union_containing_struct "uinner" inner_s in
    seal union_containing_struct;

    let struct_containing_union = structure "scu" in
    let _ = field struct_containing_union "scuf" inner_u in
    seal struct_containing_union;

    let struct_containing_anonymous_struct = structure "scas" in
    let _ = field struct_containing_anonymous_struct "scasf" anon_s in
    seal struct_containing_anonymous_struct;

    let struct_containing_anonymous_union = structure "scau" in
    let _ = field struct_containing_anonymous_union "scauf" anon_u in
    seal struct_containing_anonymous_union;

    let union_containing_union = union "ucu" in
    let _ = field union_containing_union "ucuf" inner_u in
    seal union_containing_union;

    assert_typ_printed_as "struct scs {
                              struct inner_s inner;
                           }"
      struct_containing_struct;

    assert_typ_printed_as ~name:"f" 
                          "union ucs {
                              struct inner_s uinner;
                           } f"
      union_containing_struct;

    assert_typ_printed_as "struct scu {
                              union inner_u scuf;
                           }"
      struct_containing_union;

    assert_typ_printed_as ~name:"g" 
                          "union ucu {
                              union inner_u ucuf;
                           } g"
      union_containing_union;

    assert_typ_printed_as "struct scas {
                              struct { int a; } scasf;
                           }"
      struct_containing_anonymous_struct;

    assert_typ_printed_as "struct scau {
                              union { int b; } scauf;
                           }"
      struct_containing_anonymous_union;
  end


(*
  Test the printing of array types.
*)
let test_array_printing _ =
  begin
    assert_typ_printed_as ~name:"a" "int a[10]"
      (array 10 int);

    assert_typ_printed_as "long [1][2][3]"
      (array 1 (array 2 (array 3 long)));

    assert_typ_printed_as ~name:"b" "int (*b[10])(float)"
      (array 10 (Foreign.funptr (float @-> returning int)));

    let s = structure "s" in

    assert_typ_printed_as ~name:"c" "struct s (*(*(*c[1])[2])(int (*)[3]))[4]"
      (array 1
         (ptr (array 2
                 (Foreign.funptr (ptr (array 3 int) @->
                                  returning (ptr (array 4 s)))))));
  end


(*
  Test the printing of OCaml string types.
*)
let test_ocaml_string_printing _ =
  begin
    assert_typ_printed_as ~name:"p" "char *p"
      ocaml_string;

    assert_typ_printed_as "char *"
      ocaml_string;
  end


(*
  Test the printing of bigarray types with signed elements.
*)
let test_bigarray_signed_printing _ =
  begin
    assert_typ_printed_as "int8_t[1][3]"
      (bigarray genarray [|1; 3|] Bigarray.int8_signed);

    assert_typ_printed_as "int16_t[3]"
      (bigarray array1 3 Bigarray.int16_signed);

    assert_typ_printed_as "int32_t[5][6]"
      (bigarray array2 (5, 6) Bigarray.int32);

    assert_typ_printed_as "int64_t[7][8]"
      (bigarray array2 (7, 8) Bigarray.int64);

    assert_typ_printed_as "intnat[9][10]"
      (bigarray array2 (9, 10) Bigarray.int);

    assert_typ_printed_as "intnat[13][14][15]"
      (bigarray array3 (13, 14, 15) Bigarray.nativeint);
  end


(*
  Test the printing of bigarray types with unsigned elements.
*)
let test_bigarray_unsigned_printing _ =
  skip_if true
    "Unsigned bigarray elements currently indistinguishable from signed elements";
  begin
    assert_typ_printed_as "uint8_t[2]"
      (bigarray array1 2 Bigarray.int8_unsigned);

    assert_typ_printed_as "uint16_t[4]"
      (bigarray array1 4 Bigarray.int16_unsigned);
  end


(*
  Test the printing of bigarray types with floating elements.
*)
let test_bigarray_float_printing _ =
  begin
    assert_typ_printed_as "float[10][100]"
      (bigarray genarray [|10; 100|] Bigarray.float32);

    assert_typ_printed_as "double[20][30][40]"
      (bigarray genarray [|20; 30; 40|] Bigarray.float64);

    assert_typ_printed_as "float _Complex[16][17][18]"
      (bigarray array3 (16, 17, 18) Bigarray.complex32);

    assert_typ_printed_as "double _Complex[19][20][21]"
      (bigarray array3 (19, 20, 21) Bigarray.complex64);
  end


(*
  Test the printing of function types.
*)
let test_function_printing _ =
  begin
    assert_fn_printed_as ~name:"a" "void a(void)"
      (void @-> returning void);

    assert_fn_printed_as "float(int, char, double)"
      (int @-> char @-> double @-> returning float);

    assert_fn_printed_as ~name:"c" "int (*c(void (*)(void)))(int)"
      (Foreign.funptr (void @-> returning void) @->
       returning (Foreign.funptr (int @-> returning int)));

    let s = structure "s" in
    let _ = field s "_" int in
    seal s;
    
    assert_fn_printed_as "struct s(struct s)"
      (s @-> returning s);
  end


(*
  Test the printing of view types.
*)
let test_view_printing _ =
  begin
    (* By default, views are printed as the underlying type *)

    assert_typ_printed_as ~name:"a" "char *a"
      string;

    let v : unit typ = view ~read:(fun _ -> ()) ~write:(fun () () -> ())
      (Foreign.funptr (void @-> returning void)) in
    
    assert_typ_printed_as "void (*)(void)"
      v;

    (* The format_typ optional argument can be used to provide custom
       printing for views. *)
    let w : unit typ = view (Foreign.funptr (int @-> returning float))
      ~format_typ:(fun k fmt -> Format.fprintf fmt "unit%t" k)
      ~read:(fun _ -> ())
      ~write:(fun () _ -> 0.0) in

    assert_typ_printed_as "unit"
      w;

    assert_fn_printed_as ~name:"g" "unit g(unit)"
      (w @-> returning w)
  end



(*
   Test the printing of enum types
*)
let test_enum_printing _ =
  begin
    assert_typ_printed_as ~name:"f" "enum fruit f"
      Struct_stubs.fruit;

    assert_typ_printed_as "enum fruit"
      Struct_stubs.fruit;

    assert_typ_printed_as ~name:"b" "bears_t b"
      Struct_stubs.bears_t;

    assert_typ_printed_as "bears_t"
      Struct_stubs.bears_t;

    assert_typ_printed_as ~name:"l" "letter_t l"
      Struct_stubs.letter_t;

    assert_typ_printed_as "letter_t"
      Struct_stubs.letter_t;
  end


let suite = "Type printing tests" >:::
  ["printing atomic types"
    >:: test_atomic_printing;

   "printing pointers"
    >:: test_pointer_printing;

   "printing structs and unions"
    >:: test_struct_and_union_printing;

   "printing arrays"
    >:: test_array_printing;

   "printing OCaml string types"
    >:: test_ocaml_string_printing;

   "printing bigarrays with signed elements"
    >:: test_bigarray_signed_printing;

   "printing bigarrays with unsigned elements"
    >:: test_bigarray_unsigned_printing;

   "printing bigarrays with floating elements"
    >:: test_bigarray_float_printing;

   "printing functions"
    >:: test_function_printing;

   "printing views"
    >:: test_view_printing;

   "printing enums"
    >:: test_enum_printing;
  ]


let _ =
  run_test_tt_main suite
