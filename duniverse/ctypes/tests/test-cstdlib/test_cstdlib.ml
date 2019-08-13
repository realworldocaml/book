(*
 * Copyright (c) 2013 Jeremy Yallop.
 *
 * This file is distributed under the terms of the MIT License.
 * See the file LICENSE for details.
 *)

open OUnit2
open Ctypes
open Unsigned
open Foreign


module Common_tests(S : Cstubs.FOREIGN with type 'a result = 'a
                                        and type 'a return = 'a) =
struct
  module M = Functions.Stubs(S)
  open M

  (*
    Call the functions

       int isisalnum(int)
       int isisalpha(int)
       int isiscntrl(int)
       int isisdigit(int)
       int isisgraph(int)
       int isislower(int)
       int isisprint(int)
       int isispunct(int)
       int isisspace(int)
       int isisupper(int)
       int isisxdigit(int)
  *)
  let test_isX_functions _ =
    begin
      assert_bool "" (isalnum 'a');
      assert_bool "" (not (isalnum ' '));

      assert_bool "" (isalpha 'x');
      assert_bool "" (not (isalpha ';'));

      assert_bool "" (iscntrl '\r');
      assert_bool "" (not (iscntrl 'a'));

      assert_bool "" (isdigit '2');
      assert_bool "" (not (isdigit 'a'));

      assert_bool "" (isgraph '?');
      assert_bool "" (not (isgraph ' '));

      assert_bool "" (islower 's');
      assert_bool "" (not (islower 'S'));

      assert_bool "" (isprint ' ');
      assert_bool "" (not (isprint '\b'));

      assert_bool "" (ispunct '.');
      assert_bool "" (not (ispunct 'a'));

      assert_bool "" (isspace '\t');
      assert_bool "" (not (isspace '~'));

      assert_bool "" (isupper 'X');
      assert_bool "" (not (isupper 'x'));

      assert_bool "" (isxdigit 'f');
      assert_bool "" (not (isxdigit 'g'));
    end


  (*
    Call the functions

      char *strchr(const char *str, int c);
      int strcmp(const char *str1, const char *str2);
  *)
  let test_string_functions _ =
    assert_equal "efg" (strchr "abcdefg" (Char.code 'e'))
      ~printer:(fun x -> x);

    (* non-word-aligned pointers do not trigger exceptions *)
    assert_equal "defg" (strchr "abcdefg" (Char.code 'd'));

    assert_bool "strcmp('abc', 'def') < 0"
      (strcmp "abc" "def" < 0);

    assert_bool "strcmp('def', 'abc') > 0"
      (strcmp "def" "abc" > 0);

    assert_bool "strcmp('abc', 'abc') == 0"
      (strcmp "abc" "abc" = 0);

    let p1 = allocate int 10 and p2 = allocate int 20 in
    assert_bool "memcmp(&10, &20) < 0"
      (memcmp (to_voidp p1) (to_voidp p2) (Size_t.of_int (sizeof int)) < 0);

    let p = allocate_n uchar ~count:12 in
    let i = 44 in
    let u = UChar.of_int i in begin
      ignore (memset (to_voidp p) i (Size_t.of_int 12));
      for i = 0 to 11 do
        assert_equal u !@(p +@ i)
      done
    end


  (*
    Call the function

       void qsort(void *base, size_t nmemb, size_t size,
                  int(*compar)(const void *, const void *));
  *)
  let test_qsort _ =
    let sortby (type a) (typ : a typ) (f : a -> a -> int) (l : a list) =
      let open CArray in
      let open Size_t in
      let arr = of_list typ l in
      let len = of_int (length arr) in
      let size = of_int (sizeof typ) in
      let cmp xp yp =
        let x = !@(from_voidp typ xp)
        and y = !@(from_voidp typ yp) in
        f x y
      in
      let () = qsort (to_voidp (start arr)) len size cmp in
      let _ = Ctypes_memory_stubs.use_value cmp in
      to_list arr
      in

      assert_equal
        [5; 4; 3; 2; 1]
        (sortby int (fun x y -> - (compare x y)) [3; 4; 1; 2; 5]);

      assert_equal
        ['o'; 'q'; 'r'; 's'; 't']
        (sortby char compare ['q'; 's'; 'o'; 'r'; 't'])


  (*
    Call the function

       void *bsearch(const void *key, const void *base,
                     size_t nmemb, size_t size,
                     int (*compar)(const void *, const void *));
  *)
  let test_bsearch _ =
    let module M = struct
      (*
        struct mi {
           int nr;
           char *name;
        } months[] = {
           { 1, "jan" }, { 2, "feb" }, { 3, "mar" }, { 4, "apr" },
           { 5, "may" }, { 6, "jun" }, { 7, "jul" }, { 8, "aug" },
           { 9, "sep" }, {10, "oct" }, {11, "nov" }, {12, "dec" }
        };
      *)
      type mi
      let mi = structure "mi"
      let (-:) ty label = field mi label ty
      let mr   = int      -: "mr"
      let name = ptr char -: "name"
      let () = seal (mi : mi structure typ)

    let of_string : string -> char carray =
      fun s ->
        CArray.from_ptr (coerce string (ptr char) s) (String.length s)

    let as_string : char ptr -> string =
      coerce (ptr char) string

    let mkmi n s =
      let m = make mi in
      setf m mr n;
      setf m name (CArray.start s);
      m

    let cmpi m1 m2 =
      let mi1 = from_voidp mi m1 in
      let mi2 = from_voidp mi m2 in
      Stdlib.compare
        (as_string (!@(mi1 |-> name)))
        (as_string (!@(mi2 |-> name)))

    let jan = of_string "jan"
    let feb = of_string "feb"
    let mar = of_string "mar"
    let apr = of_string "apr"
    let may = of_string "may"
    let jun = of_string "jun"
    let jul = of_string "jul"
    let aug = of_string "aug"
    let sep = of_string "sep"
    let oct = of_string "oct"
    let nov = of_string "nov"
    let dec = of_string "dec"

    let months = CArray.of_list mi [
      mkmi 1 jan;
      mkmi 2 feb;
      mkmi 3 mar;
      mkmi 4 apr;
      mkmi 5 may;
      mkmi 6 jun;
      mkmi 7 jul;
      mkmi 8 aug;
      mkmi 9 sep;
      mkmi 10 oct;
      mkmi 11 nov;
      mkmi 12 dec;
    ]

    let () = qsort
      (to_voidp (CArray.start months))
      (Size_t.of_int (CArray.length months))
      (Size_t.of_int (sizeof mi))
      cmpi

    let search : mi structure -> mi structure carray -> mi structure option
      = fun key array ->
        let len = Size_t.of_int (CArray.length array) in
        let size = Size_t.of_int (sizeof mi) in
        let r : unit ptr =
          bsearch
            (to_voidp (addr key))
            (to_voidp (CArray.start array))
            len size cmpi in
        if r = null then None
        else Some (!@(from_voidp mi r))

    let find_month_by_name : char carray -> mi structure option =
      fun s -> search (mkmi 0 s) months

    let () = match find_month_by_name dec with
        Some m -> assert_equal 12 (getf m mr)
      | _ -> assert false

    let () = match find_month_by_name feb with
        Some m -> assert_equal 2 (getf m mr)
      | _ -> assert false

    let () = match find_month_by_name jan with
        Some m -> assert_equal 1 (getf m mr)
      | _ -> assert false

    let () = match find_month_by_name may with
        Some m -> assert_equal 5 (getf m mr)
      | _ -> assert false

    let missing = of_string "missing"
    let () =
      assert_equal None (find_month_by_name missing)

    let empty = of_string ""
    let () =
      assert_equal None (find_month_by_name empty)

    let _ = Ctypes_memory_stubs.use_value
      [jan; feb; mar; apr; may; jun;
       jul; aug; sep; oct; nov; dec]
    end in ()
end

(*
  Call the functions

     div_t div(int numerator, int denominator)

  where div_t is defined as follows:

    typedef struct
      {
        int quot;			/* Quotient.  */
        int rem;			/* Remainder.  */
      } div_t;
*)
let test_div _ =
  let module M = struct
    type div_t
    let div_t : div_t structure typ = structure "div_t"
    let (-:) ty label = field div_t label ty
    let quot = int -: "quot"
    let rem  = int -: "rem"
    let () = seal div_t

    let div = foreign "div" (int @-> int @-> returning div_t)

    let test ~num ~dem ~quotient ~remainder =
      let v = div num dem in
      let () = assert_equal quotient (getf v quot) in
      let () = assert_equal remainder (getf v rem) in
      ()

    let () = test ~num:10 ~dem:2 ~quotient:5 ~remainder:0

    let () = test ~num:11 ~dem:2 ~quotient:5 ~remainder:1
  end in ()


module Foreign_tests = Common_tests(Tests_common.Foreign_binder)
module Stub_tests = Common_tests(Generated_bindings)


let suite = "C standard library tests" >:::
  ["test isX functions (foreign)"
    >:: Foreign_tests.test_isX_functions;

   "test isX functions (stubs)"
    >:: Stub_tests.test_isX_functions;

   "test string function (foreign)"
    >:: Foreign_tests.test_string_functions;

   "test string function (stubs)"
    >:: Stub_tests.test_string_functions;

   "test div function"
    >:: test_div;

   "test qsort function (foreign)"
    >:: Foreign_tests.test_qsort;

   "test qsort function (stubs)"
    >:: Stub_tests.test_qsort;

   "test bsearch function (foreign)"
    >:: Foreign_tests.test_bsearch;

   "test bsearch function (stubs)"
    >:: Stub_tests.test_bsearch;
  ]


let _ =
  run_test_tt_main suite
