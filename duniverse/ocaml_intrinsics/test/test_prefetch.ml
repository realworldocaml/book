open Base
open Stdio
open Bigarray

type bigstring = (char, int8_unsigned_elt, c_layout) Array1.t

module EP = Ocaml_intrinsics.Ext_pointer
module NP = Ocaml_intrinsics.Native_pointer
module P = Ocaml_intrinsics.Prefetch

external alloc_untagged_int_ref : int -> int = "external_untagged_int_ref"
external alloc_unboxed_float_ref : float -> int = "external_unboxed_float_ref"

let create_untagged_int_ref init_val = alloc_untagged_int_ref init_val |> EP.create
let create_unboxed_float_ref init_val = alloc_unboxed_float_ref init_val |> EP.create

external create_untagged_int_ref_as_native_pointer
  :  int
  -> NP.t
  = "external_untagged_int_ref_as_native_pointer"

external create_unboxed_float_ref_as_native_pointer
  :  float
  -> NP.t
  = "external_unboxed_float_ref_as_native_pointer"

external caml_bigstring_get_16 : bigstring -> int -> int = "%caml_bigstring_get16"

(* We want intrinsics library to be eventually included in the compiler,
   and therefore have no other dependencies, but the tests can use Base. *)
module T = struct
  type temporal_locality = P.temporal_locality =
    | None
    | Low
    | Moderate
    | High
  [@@deriving sexp, enumerate]

  type operation = P.operation =
    | Read
    | Write
  [@@deriving sexp, enumerate]
end

include T

let len = 10
let ints = List.init len ~f:(fun _ -> Random.int Int.max_value)
let floats = List.init len ~f:(fun _ -> Random.float Float.max_value)
let int_refs = List.map ints ~f:create_untagged_int_ref
let float_refs = List.map floats ~f:create_unboxed_float_ref

let int_refs_as_native_pointer =
  List.map ints ~f:create_untagged_int_ref_as_native_pointer
;;

let float_refs_as_native_pointer =
  List.map floats ~f:create_unboxed_float_ref_as_native_pointer
;;

let bigstring_of_string s =
  let a = Array1.create char c_layout (String.length s) in
  for i = 0 to String.length s - 1 do
    a.{i} <- s.[i]
  done;
  a
;;

let bigstring = bigstring_of_string (String.make 500 '\x00')
let positions = [ 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 16; 32; 64; 256 ]

let%expect_test "prefetch" =
  List.iter all_of_operation ~f:(fun operation ->
    List.iter all_of_temporal_locality ~f:(fun temporal_locality ->
      (* The data is probably already in cache so the test's effectiveness
         is limited to code generation. *)
      printf
        !"%{sexp:operation} %{sexp:temporal_locality}\n"
        operation
        temporal_locality;
      let test_int n r =
        P.ext_pointer r ~operation ~temporal_locality;
        let n' = EP.load_untagged_int r in
        if not (Int.equal n n') then printf "n=%d n'=%d\n" n n';
        let k = n + 47 in
        EP.store_untagged_int r k;
        let k' = EP.load_untagged_int r in
        if not (Int.equal k k') then printf "n=%d n'=%d\n" k k';
        EP.store_untagged_int r n
      in
      let test_float n r =
        P.ext_pointer r ~operation ~temporal_locality;
        let n' = EP.load_unboxed_float r in
        if not (Float.equal n n') then printf "float ext: n=%f n'=%f\n" n n';
        let k = n *. 49.0 in
        EP.store_unboxed_float r k;
        let k' = EP.load_unboxed_float r in
        if not (Float.equal k k') then printf "n=%f n'=%f\n" k k';
        EP.store_unboxed_float r n
      in
      let test_int_np n r =
        P.native_pointer r ~operation ~temporal_locality;
        let n' = NP.load_untagged_int r in
        if not (Int.equal n n') then printf "int np: n=%d n'=%d\n" n n';
        let k = n + 47 in
        NP.store_untagged_int r k;
        let k' = NP.load_untagged_int r in
        if not (Int.equal k k') then printf "n=%d n'=%d\n" k k';
        NP.store_untagged_int r n
      in
      let test_float_np n r =
        P.native_pointer r ~operation ~temporal_locality;
        let n' = NP.load_unboxed_float r in
        if not (Float.equal n n') then printf "float ext: n=%f n'=%f\n" n n';
        let k = n *. 49.0 in
        NP.store_unboxed_float r k;
        let k' = NP.load_unboxed_float r in
        if not (Float.equal k k') then printf "n=%f n'=%f\n" k k';
        NP.store_unboxed_float r n
      in
      let test_bigstring pos =
        P.bigstring bigstring ~pos ~operation ~temporal_locality;
        let c = caml_bigstring_get_16 bigstring pos in
        if not (Int.equal c 0) then printf "bigstring_get_16 %d=%d\n" pos c
      in
      List.iter2_exn ints int_refs ~f:test_int;
      List.iter2_exn floats float_refs ~f:test_float;
      List.iter2_exn ints int_refs_as_native_pointer ~f:test_int_np;
      List.iter2_exn floats float_refs_as_native_pointer ~f:test_float_np;
      List.iter positions ~f:test_bigstring));
  [%expect
    {|
    Read None
    Read Low
    Read Moderate
    Read High
    Write None
    Write Low
    Write Moderate
    Write High |}]
;;

let%expect_test "pause" =
  P.pause ();
  [%expect {||}]
;;
