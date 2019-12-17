(*
 * Copyright (c) 2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2013 Citrix Systems Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

[@@@warning "-3"]

let to_string { Cstruct.buffer; off; len } =
  Printf.sprintf "buffer length = %d; off=%d; len=%d" (Bigarray.Array1.dim buffer) off len

(* Check we can create and use an empty cstruct *)
let test_empty_cstruct () =
  let x = Cstruct.create 0 in
  Alcotest.(check int) "empty len" 0 (Cstruct.len x);
  let y = Cstruct.to_string x in
  Alcotest.(check string) "empty" "" y

(* Check that we can't create a cstruct with a negative length *)
let test_anti_cstruct () =
  try
    let x = Cstruct.create (-1) in
    failwith (Printf.sprintf "test_anti_cstruct: %s" (to_string x))
  with Invalid_argument _ ->
    ()

(* Check we can shift in the +ve direction *)
let test_positive_shift () =
  let x = Cstruct.create 1 in
  let y = Cstruct.shift x 1 in
  Alcotest.(check int) "positive shift" 0 (Cstruct.len y)

(* Check that negative shifts are forbidden. *)
let test_negative_shift () =
  let x = Cstruct.create 2 in
  let y = Cstruct.sub x 1 1 in
  try
    let z = Cstruct.shift x (-1) in
    failwith (Printf.sprintf "test_negative_shift/outer: %s" (to_string z))
  with Invalid_argument _ ->
    try
      let z = Cstruct.shift y (-1) in
      failwith (Printf.sprintf "test_negative_shift/inner: %s" (to_string z))
    with Invalid_argument _ ->
      ()

(* Check that an attempt to shift beyond the end of the buffer fails *)
let test_bad_positive_shift () =
  let x = Cstruct.create 10 in
  try
    let y = Cstruct.shift x 11 in
    failwith (Printf.sprintf "test_bad_positive_shift: %s" (to_string y))
  with Invalid_argument _ -> ()

(* Check that 'sub' works *)
let test_sub () =
  let x = Cstruct.create 100 in
  let y = Cstruct.sub x 10 80 in
  Alcotest.(check int) "sub 1" 10 y.Cstruct.off;
  Alcotest.(check int) "sub 2" 80 y.Cstruct.len;
  let z = Cstruct.sub y 10 60 in
  Alcotest.(check int) "sub 3" 20 z.Cstruct.off;
  Alcotest.(check int) "sub 4" 60 z.Cstruct.len

let test_negative_sub () =
  let x = Cstruct.create 2 in
  let y = Cstruct.sub x 1 1 in
  try
    let z = Cstruct.sub x (-1) 0 in
    failwith (Printf.sprintf "test_negative_sub/outer: %s" (to_string z))
  with Invalid_argument _ ->
    try
      let z = Cstruct.sub y (-1) 0 in
      failwith (Printf.sprintf "test_negative_sub/inner: %s" (to_string z))
    with Invalid_argument _ ->
      ()

(* Check that 'sub' can't set 'len' too big *)
let test_sub_len_too_big () =
  let x = Cstruct.create 0 in
  try
    let y = Cstruct.sub x 0 1 in
    failwith (Printf.sprintf "test_sub_len_too_big: %s" (to_string y))
  with Invalid_argument _ -> ()

let test_sub_len_too_small () =
  let x = Cstruct.create 0 in
  try
    let y = Cstruct.sub x 0 (-1) in
    failwith (Printf.sprintf "test_sub_len_too_small: %s" (to_string y))
  with Invalid_argument _ -> ()

let test_sub_offset_too_big () =
  let x = Cstruct.create 10 in
  begin
    try
      let y = Cstruct.sub x 11 0 in
      failwith (Printf.sprintf "test_sub_offset_too_big: %s" (to_string y))
    with Invalid_argument _ -> ()
  end;
  let y = Cstruct.sub x 1 9 in
  begin
    try
      let z = Cstruct.sub y 10 0 in
      failwith (Printf.sprintf "test_sub_offset_too_big: %s" (to_string z))
    with Invalid_argument _ -> ()
  end

let test_of_bigarray_negative_params () =
  let ba = Bigarray.(Array1.create char c_layout 1) in
  try
    let x = Cstruct.of_bigarray ~off:(-1) ba in
    failwith (Printf.sprintf "test_of_bigarray_negative_params: negative ~off: %s" (to_string x))
  with Invalid_argument _ ->
    try
      let x = Cstruct.of_bigarray ~len:(-1) ba in
      failwith (Printf.sprintf "test_of_bigarray_negative_params: negative ~len: %s" (to_string x))
    with Invalid_argument _ ->
      ()

let test_of_bigarray_large_offset () =
  let ba = Bigarray.(Array1.create char c_layout 1) in
  let _ = Cstruct.of_bigarray ~off:1 ~len:0 ba
  and _ = Cstruct.of_bigarray ~off:1 ba in
  try
    let x = Cstruct.of_bigarray ~off:2 ~len:0 ba in
    failwith (Printf.sprintf "test_of_bigarray_large_offset: %s" (to_string x))
  with Invalid_argument _ ->
    try
      let x = Cstruct.of_bigarray ~off:2 ba in
      failwith (Printf.sprintf "test_of_bigarray_large_offset: large ~off: %s" (to_string x))
    with Invalid_argument _ ->
      ()

let test_of_bigarray_large_length () =
  let ba = Bigarray.(Array1.create char c_layout 1) in
  try
    let x = Cstruct.of_bigarray ~off:0 ~len:2 ba in
    failwith (Printf.sprintf "test_of_bigarray_large_length: %s" (to_string x))
  with Invalid_argument _ ->
    try
      let x = Cstruct.of_bigarray ~off:1 ~len:1 ba in
      failwith (Printf.sprintf "test_of_bigarray_large_length: %s" (to_string x))
    with Invalid_argument _ ->
      try
        let x = Cstruct.of_bigarray ~off:2 ~len:0 ba in
        failwith (Printf.sprintf "test_of_bigarray_large_length: %s" (to_string x))
      with Invalid_argument _ ->
        try
          let x = Cstruct.of_bigarray ~off:2 ba in
          failwith (Printf.sprintf "test_of_bigarray_large_length: %s" (to_string x))
        with Invalid_argument _ ->
          ()

let test_set_len_too_big () =
  let x = Cstruct.create 0 in
  try
    let[@ocaml.warning "-3"] y = Cstruct.set_len x 1 in
    failwith (Printf.sprintf "test_set_len_too_big: %s" (to_string y))
  with Invalid_argument _ -> ()

let test_set_len_too_small () =
  let x = Cstruct.create 0 in
  try
    let[@ocaml.warning "-3"] y = Cstruct.set_len x (-1) in
    failwith (Printf.sprintf "test_set_len_too_small: %s" (to_string y))
  with Invalid_argument _ -> ()

let test_add_len_too_big () =
  let x = Cstruct.create 0 in
  try
    let[@ocaml.warning "-3"] y = Cstruct.add_len x 1 in
    failwith (Printf.sprintf "test_add_len_too_big: %s" (to_string y))
  with Invalid_argument _ -> ()

let test_add_len_too_small () =
  let x = Cstruct.create 0 in
  try
    let[@ocaml.warning "-3"] y = Cstruct.add_len x (-1) in
    failwith (Printf.sprintf "test_add_len_too_small: %s" (to_string y))
  with Invalid_argument _ -> ()

let test_blit_offset_too_big () =
  let x = Cstruct.create 1 in
  let y = Cstruct.create 1 in
  try
    Cstruct.blit x 2 y 1 1;
    failwith "test_blit_offset_too_big"
  with Invalid_argument _ -> ()

let test_blit_offset_too_small () =
  let x = Cstruct.create 1 in
  let y = Cstruct.create 1 in
  try
    Cstruct.blit x (-1) y 1 1;
    failwith "test_blit_offset_too_small"
  with Invalid_argument _ -> ()

let test_blit_dst_offset_too_big () =
  let x = Cstruct.create 1 in
  let y = Cstruct.create 1 in
  try
    Cstruct.blit x 1 y 2 1;
    failwith "test_blit_dst_offset_too_big"
  with Invalid_argument _ -> ()

let test_blit_dst_offset_too_small () =
  let x = Cstruct.create 1 in
  let y = Cstruct.create 1 in
  try
    Cstruct.blit x 1 y (-1) 1;
    failwith "test_blit_dst_offset_too_small"
  with Invalid_argument _ -> ()

let test_blit_dst_offset_negative () =
  let x = Cstruct.create 1 in
  let y = Cstruct.create 1 in
  try
    Cstruct.blit x 0 y (-1) 1;
    failwith "test_blit_dst_offset_negative"
  with Invalid_argument _ -> ()

let test_blit_len_too_big () =
  let x = Cstruct.create 1 in
  let y = Cstruct.create 2 in
  try
    Cstruct.blit x 0 y 0 2;
    failwith "test_blit_len_too_big"
  with Invalid_argument _ -> ()

let test_blit_len_too_big2 () =
  let x = Cstruct.create 2 in
  let y = Cstruct.create 1 in
  try
    Cstruct.blit x 0 y 0 2;
    failwith "test_blit_len_too_big2"
  with Invalid_argument _ -> ()

let test_blit_len_too_small () =
  let x = Cstruct.create 1 in
  let y = Cstruct.create 1 in
  try
    Cstruct.blit x 0 y 0 (-1);
    failwith "test_blit_len_too_small"
  with Invalid_argument _ -> ()

let test_view_bounds_too_small () =
  let src = Cstruct.create 4 in
  let dst = Cstruct.create 4 in
  let dst_small = Cstruct.sub dst 0 2 in
  try
    Cstruct.blit src 0 dst_small 0 3;
    failwith "test_view_bounds_too_small"
  with
    Invalid_argument _ -> ()

let test_view_bounds_too_small_get_u8 () =
  let x = Cstruct.create 2 in
  let x' = Cstruct.sub x 0 1 in
  try
    let _ = Cstruct.get_uint8 x' 1 in
    failwith "test_view_bounds_too_small_get_u8"
  with
    Invalid_argument _ -> ()

let test_view_bounds_too_small_get_char () =
  let x = Cstruct.create 2 in
  let x' = Cstruct.sub x 0 1 in
  try
    let _ = Cstruct.get_char x' 1 in
    failwith "test_view_bounds_too_small_get_char"
  with
    Invalid_argument _ -> ()

let test_view_bounds_too_small_get_be16 () =
  let x = Cstruct.create 4 in
  let x' = Cstruct.sub x 0 1 in
  try
    let _ = Cstruct.BE.get_uint16 x' 0 in
    failwith "test_view_bounds_too_small_get_be16"
  with
    Invalid_argument _ -> ()

let test_view_bounds_too_small_get_be32 () =
  let x = Cstruct.create 8 in
  let x' = Cstruct.sub x 2 5 in
  try
    let _ = Cstruct.BE.get_uint32 x' 2 in
    failwith "test_view_bounds_too_small_get_be32"
  with
    Invalid_argument _ -> ()

let test_view_bounds_too_small_get_be64 () =
  let x = Cstruct.create 9 in
  let x' = Cstruct.sub x 1 5 in
  try
    let _ = Cstruct.BE.get_uint64 x' 0 in
    failwith "test_view_bounds_too_small_get_be64"
  with
    Invalid_argument _ -> ()

let test_view_bounds_too_small_get_le16 () =
  let x = Cstruct.create 4 in
  let x' = Cstruct.sub x 0 1 in
  try
    let _ = Cstruct.LE.get_uint16 x' 0 in
    failwith "test_view_bounds_too_small_get_le16"
  with
    Invalid_argument _ -> ()

let test_view_bounds_too_small_get_le32 () =
  let x = Cstruct.create 8 in
  let x' = Cstruct.sub x 2 5 in
  try
    let _ = Cstruct.LE.get_uint32 x' 2 in
    failwith "test_view_bounds_too_small_get_le32"
  with
    Invalid_argument _ -> ()

let test_view_bounds_too_small_get_le64 () =
  let x = Cstruct.create 9 in
  let x' = Cstruct.sub x 1 5 in
  try
    let _ = Cstruct.LE.get_uint64 x' 0 in
    failwith "test_view_bounds_too_small_get_le64"
  with
    Invalid_argument _ -> ()

let test_lenv_overflow () =
  if Sys.word_size = 32 then (
    (* free-up some space *)
    Gc.major ();
    let b = Cstruct.create max_int and c = Cstruct.create 3 in
    try
      let _ = Cstruct.lenv [b; b; c] in
      failwith "test_lenv_overflow"
    with
      Invalid_argument _ -> ())

let test_copyv_overflow () =
  if Sys.word_size = 32 then (
    (* free-up some space *)
    Gc.major ();
    let b = Cstruct.create max_int and c = Cstruct.create 3 in
    try
      let _ = Cstruct.copyv [b; b; c] in
      failwith "test_copyv_overflow"
    with
      Invalid_argument _ -> ())

(* Steamroll over a buffer and a contained subview, checking that only the
 * contents of the subview is visible. *)
let test_subview_containment_get_char,
    test_subview_containment_get_8,
    test_subview_containment_get_be16,
    test_subview_containment_get_be32,
    test_subview_containment_get_be64,
    test_subview_containment_get_le16,
    test_subview_containment_get_le32,
    test_subview_containment_get_le64
  =
  let open Cstruct in
  let test get zero () =
    let x = create 24 in
    let x' = sub x 8 8 in
    for i = 0 to len x - 1 do set_uint8 x i 0xff done ;
    for i = 0 to len x' - 1 do set_uint8 x' i 0x00 done ;
    for i = -8 to 8 do
      try
        let v = get x' i in
        if v <> zero then
          failwith "test_subview_containment_get"
      with Invalid_argument _ -> ()
    done
  in
  test get_char '\000',
  test get_uint8 0,
  test BE.get_uint16 0,
  test BE.get_uint32 0l,
  test BE.get_uint64 0L,
  test LE.get_uint16 0,
  test LE.get_uint32 0l,
  test LE.get_uint64 0L

(* Steamroll over a buffer and a contained subview, checking that only the
 * contents of the subview is writable. *)
let test_subview_containment_set_char,
    test_subview_containment_set_8,
    test_subview_containment_set_be16,
    test_subview_containment_set_be32,
    test_subview_containment_set_be64,
    test_subview_containment_set_le16,
    test_subview_containment_set_le32,
    test_subview_containment_set_le64
  =
  let open Cstruct in
  let test set ff () =
    let x = create 24 in
    let x' = sub x 8 8 in
    for i = 0 to len x - 1 do set_uint8 x i 0x00 done ;
    for i = -8 to 8 do
      try set x' i ff with Invalid_argument _ -> ()
    done;
    let acc = ref 0 in
    for i = 0 to len x - 1 do
      acc := !acc + get_uint8 x i
    done ;
    if !acc <> (len x' * 0xff) then
      failwith "test_subview_containment_set"
  in
  test set_char '\255',
  test set_uint8 0xff,
  test BE.set_uint16 0xffff,
  test BE.set_uint32 0xffffffffl,
  test BE.set_uint64 0xffffffffffffffffL,
  test LE.set_uint16 0xffff,
  test LE.set_uint32 0xffffffffl,
  test LE.set_uint64 0xffffffffffffffffL

let regression_244 () =
  let whole = Cstruct.create 44943 in
  let empty = Cstruct.sub whole 0 0 in
  try
    let _big = Cstruct.sub empty 0 204 in
    Alcotest.fail "could get a bigger buffer via sub"
  with Invalid_argument _ -> ()


let suite = [
  "test empty cstruct", `Quick, test_empty_cstruct;
  "test anti cstruct", `Quick, test_anti_cstruct;
  "test positive shift", `Quick, test_positive_shift;
  "test negative shift", `Quick, test_negative_shift;
  "test bad positive shift", `Quick, test_bad_positive_shift;
  "test sub", `Quick, test_sub;
  "test negative sub", `Quick, test_negative_sub;
  "test sub len too big", `Quick, test_sub_len_too_big;
  "test sub len too small", `Quick, test_sub_len_too_small;
  "test sub offset too big", `Quick, test_sub_offset_too_big;
  "test of_bigarray negative params", `Quick, test_of_bigarray_negative_params;
  "test of_bigarray large offset", `Quick, test_of_bigarray_large_offset;
  "test of_bigarray large length", `Quick, test_of_bigarray_large_length;
  "test set len too big", `Quick, test_set_len_too_big;
  "test set len too small", `Quick, test_set_len_too_small;
  "test add len too big", `Quick, test_add_len_too_big;
  "test add len too small", `Quick, test_add_len_too_small;
  "test blit offset too big", `Quick, test_blit_offset_too_big;
  "test blit offset too small", `Quick, test_blit_offset_too_small;
  "test blit dst offset too big", `Quick, test_blit_dst_offset_too_big;
  "test blit dst offset too small", `Quick, test_blit_dst_offset_too_small;
  "test blit dst offset negative", `Quick, test_blit_dst_offset_negative;
  "test blit len too big", `Quick, test_blit_len_too_big;
  "test blit len too big2", `Quick, test_blit_len_too_big2;
  "test blit len too small", `Quick, test_blit_len_too_small;
  "test view bounds too small", `Quick, test_view_bounds_too_small;
  "test_view_bounds_too_small_get_u8" , `Quick, test_view_bounds_too_small_get_u8;
  "test_view_bounds_too_small_get_char" , `Quick, test_view_bounds_too_small_get_char;
  "test_view_bounds_too_small_get_be16" , `Quick, test_view_bounds_too_small_get_be16;
  "test_view_bounds_too_small_get_be32" , `Quick, test_view_bounds_too_small_get_be32;
  "test_view_bounds_too_small_get_be64" , `Quick, test_view_bounds_too_small_get_be64;
  "test_view_bounds_too_small_get_le16" , `Quick, test_view_bounds_too_small_get_le16;
  "test_view_bounds_too_small_get_le32" , `Quick, test_view_bounds_too_small_get_le32;
  "test_view_bounds_too_small_get_le64" , `Quick, test_view_bounds_too_small_get_le64;
  "test_lenv_overflow", `Quick, test_lenv_overflow;
  "test_copyv_overflow", `Quick, test_copyv_overflow;
  "test_subview_containment_get_char", `Quick, test_subview_containment_get_char;
  "test_subview_containment_get_8"   , `Quick, test_subview_containment_get_8;
  "test_subview_containment_get_be16", `Quick, test_subview_containment_get_be16;
  "test_subview_containment_get_be32", `Quick, test_subview_containment_get_be32;
  "test_subview_containment_get_be64", `Quick, test_subview_containment_get_be64;
  "test_subview_containment_get_le16", `Quick, test_subview_containment_get_le16;
  "test_subview_containment_get_le32", `Quick, test_subview_containment_get_le32;
  "test_subview_containment_get_le64", `Quick, test_subview_containment_get_le64;
  "test_subview_containment_set_char", `Quick, test_subview_containment_set_char;
  "test_subview_containment_set_8"   , `Quick, test_subview_containment_set_8;
  "test_subview_containment_set_be16", `Quick, test_subview_containment_set_be16;
  "test_subview_containment_set_be32", `Quick, test_subview_containment_set_be32;
  "test_subview_containment_set_be64", `Quick, test_subview_containment_set_be64;
  "test_subview_containment_set_le16", `Quick, test_subview_containment_set_le16;
  "test_subview_containment_set_le32", `Quick, test_subview_containment_set_le32;
  "test_subview_containment_set_le64", `Quick, test_subview_containment_set_le64;
  "regression 244", `Quick, regression_244;
]
