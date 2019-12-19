open! Core_kernel
open Poly
open! Import
open! Binary_packing

module Make_inline_tests (A : sig
    val num_bytes : int
    val signed : bool

    type t

    val ns : t list
    val of_int64 : int64 -> t
    val to_int64 : t -> int64
    val pack : byte_order:endian -> buf:bytes -> pos:int -> t -> unit
    val unpack : byte_order:endian -> buf:bytes -> pos:int -> t
    val pack_big_endian : buf:bytes -> pos:int -> t -> unit
    val unpack_big_endian : buf:bytes -> pos:int -> t
    val pack_little_endian : buf:bytes -> pos:int -> t -> unit
    val unpack_little_endian : buf:bytes -> pos:int -> t
  end) =
struct
  include A

  let pos = 3
  let buf_size = 13

  let ns_rev =
    List.map ns ~f:(fun t ->
      let t = to_int64 t in
      of_int64
        (List.fold ~init:0L (List.init num_bytes ~f:Fn.id) ~f:(fun acc k ->
           Int64.( lor )
             acc
             (let w =
                Int64.shift_left
                  (Int64.( land ) 0xFFL (Int64.shift_right_logical t (k * 8)))
                  ((num_bytes - 1 - k) * 8)
              in
              if signed && num_bytes < 8
              then (
                let max_val = Int64.shift_left 1L ((num_bytes * 8) - 1) in
                if w >= max_val
                then Int64.( - ) w (Int64.shift_left max_val 1)
                else w)
              else w))))
  ;;

  let padding = '.'

  let test_rest_of_buf buf =
    for k = 0 to buf_size - 1 do
      if k < pos || k > pos + num_bytes then assert (Bytes.get buf k = padding)
    done
  ;;

  let%test _ =
    ns
    = List.map ns ~f:(fun n ->
      let buf = Bytes.make buf_size padding in
      pack ~byte_order:`Little_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack ~byte_order:`Little_endian ~buf ~pos)
  ;;

  let%test _ =
    ns
    = List.map ns ~f:(fun n ->
      let buf = Bytes.make buf_size padding in
      pack ~byte_order:`Big_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack ~byte_order:`Big_endian ~buf ~pos)
  ;;

  let%test _ =
    ns
    = List.map ns ~f:(fun n ->
      let buf = Bytes.make buf_size padding in
      pack_little_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack_little_endian ~buf ~pos)
  ;;

  let%test _ =
    ns
    = List.map ns ~f:(fun n ->
      let buf = Bytes.make buf_size padding in
      pack_big_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack_big_endian ~buf ~pos)
  ;;

  let%test _ =
    ns_rev
    = List.map ns ~f:(fun n ->
      let buf = Bytes.make buf_size padding in
      pack_big_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack_little_endian ~buf ~pos)
  ;;

  let%test _ =
    ns_rev
    = List.map ns ~f:(fun n ->
      let buf = Bytes.make buf_size padding in
      pack_little_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack_big_endian ~buf ~pos)
  ;;

  let%test _ =
    ns
    = List.map ns ~f:(fun n ->
      let buf = Bytes.make buf_size padding in
      pack ~byte_order:`Big_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack_big_endian ~buf ~pos)
  ;;

  let%test _ =
    ns
    = List.map ns ~f:(fun n ->
      let buf = Bytes.make buf_size padding in
      pack ~byte_order:`Little_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack_little_endian ~buf ~pos)
  ;;

  let%test _ =
    ns
    = List.map ns ~f:(fun n ->
      let buf = Bytes.make buf_size padding in
      pack_big_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack ~byte_order:`Big_endian ~buf ~pos)
  ;;

  let%test _ =
    ns
    = List.map ns ~f:(fun n ->
      let buf = Bytes.make buf_size padding in
      pack_little_endian ~buf ~pos n;
      test_rest_of_buf buf;
      unpack ~byte_order:`Little_endian ~buf ~pos)
  ;;
end

let%test_module "inline_unsigned_16" =
  (module Make_inline_tests (struct
       let ns = [ 0x3f20; 0x7f20; 0xef20; 0; 0x7fff; 0x8000; 0xffff ]
       let num_bytes = 2
       let signed = false

       type t = int

       let of_int64 = Int64.to_int_trunc
       let to_int64 = Int64.of_int
       let pack = pack_unsigned_16
       let unpack = unpack_unsigned_16
       let pack_big_endian = pack_unsigned_16_big_endian
       let unpack_big_endian = unpack_unsigned_16_big_endian
       let pack_little_endian = pack_unsigned_16_little_endian
       let unpack_little_endian = unpack_unsigned_16_little_endian
     end))
;;

let%test_module "inline_signed_16" =
  (module Make_inline_tests (struct
       let ns = [ 0x3f20; 0x7f20; -0x7f20; -0x8000; 0; 1; 0x7fff ]
       let num_bytes = 2
       let signed = true

       type t = int

       let of_int64 = Int64.to_int_trunc
       let to_int64 = Int64.of_int
       let pack = pack_signed_16
       let unpack = unpack_signed_16
       let pack_big_endian = pack_signed_16_big_endian
       let unpack_big_endian = unpack_signed_16_big_endian
       let pack_little_endian = pack_signed_16_little_endian
       let unpack_little_endian = unpack_signed_16_little_endian
     end))
;;

let signed_max = Int32.to_int_trunc Int32.max_value
let unsigned_max = Int64.to_int_trunc 0xffff_ffffL

let%test_module ("inline_unsigned_32_int"[@tags "64-bits-only"]) =
  (module Make_inline_tests (struct
       let ns =
         [ 0x3f20_3040
         ; Int64.to_int_exn 0x7f20_3040L
         ; signed_max
         ; signed_max + 1
         ; unsigned_max
         ; 0
         ]
       ;;

       let num_bytes = 4
       let signed = false

       type t = int

       let of_int64 = Int64.to_int_trunc
       let to_int64 = Int64.of_int
       let pack = pack_unsigned_32_int
       let unpack = unpack_unsigned_32_int
       let pack_big_endian = pack_unsigned_32_int_big_endian
       let unpack_big_endian = unpack_unsigned_32_int_big_endian
       let pack_little_endian = pack_unsigned_32_int_little_endian
       let unpack_little_endian = unpack_unsigned_32_int_little_endian
     end))
;;

let%test_module ("inline_signed_32_int"[@tags "64-bits-only"]) =
  (module Make_inline_tests (struct
       let ns =
         [ 0x3f20_3040
         ; Int64.to_int_exn 0x7f20_3040L
         ; Int64.to_int_exn (-0x7f20_3040L)
         ; signed_max
         ; -(signed_max + 1)
         ; 0
         ]
       ;;

       let num_bytes = 4
       let signed = true

       type t = int

       let of_int64 = Int64.to_int_trunc
       let to_int64 = Int64.of_int
       let pack = pack_signed_32_int
       let unpack = unpack_signed_32_int
       let pack_big_endian = pack_signed_32_int_big_endian
       let unpack_big_endian = unpack_signed_32_int_big_endian
       let pack_little_endian = pack_signed_32_int_little_endian
       let unpack_little_endian = unpack_signed_32_int_little_endian
     end))
;;

let%test_unit ("63 bits overflow"[@tags "64-bits-only"]) =
  let buf = Bytes.create 8 in
  let pos = 0 in
  List.iter
    [ pack_signed_64_little_endian, unpack_signed_64_int_little_endian
    ; pack_signed_64_big_endian, unpack_signed_64_int_big_endian
    ]
    ~f:(fun (pack, unpack) ->
      List.iter
        [ Int64.max_value, Some 127
        ; Int64.min_value, Some 128
        ; Int64.(of_int Int.max_value + 1L), Some 64
        ; Int64.(of_int Int.min_value + -1L), Some 191
        ; Int64.(of_int Int.max_value), None
        ; Int64.(of_int Int.min_value), None
        ]
        ~f:(fun (n, opt) ->
          pack ~buf ~pos n;
          try
            ignore (unpack ~buf ~pos : int);
            assert (opt = None)
          with
          | Private.Unpack_signed_64_int_most_significant_byte_too_large n
            when Some n = opt -> ()))
;;

let%test_module "inline_signed_64" =
  (module Make_inline_tests (struct
       let ns =
         [ 0x3f20_3040_5060_7080L
         ; 0x7f20_3040_5060_7080L
         ; -0x7f20_3040_5060_7080L
         ; 0x7fff_ffff_ffff_ffffL
         ; 0x8000_0000_0000_0000L
         ; 0L
         ]
       ;;

       let num_bytes = 8
       let signed = true

       type t = int64

       let of_int64 = Fn.id
       let to_int64 = Fn.id
       let pack = pack_signed_64
       let unpack = unpack_signed_64
       let pack_big_endian = pack_signed_64_big_endian
       let unpack_big_endian = unpack_signed_64_big_endian
       let pack_little_endian = pack_signed_64_little_endian
       let unpack_little_endian = unpack_signed_64_little_endian
     end))
;;

let%test_module ("inline_signed_64_int"[@tags "64-bits-only"]) =
  (module Make_inline_tests (struct
       (* These numbers are written with one endianness and read with the opposite endianness,
          so the smallest byte becomes the biggest byte. Because of this, the range restriction
          that applies to the biggest byte also applies to the smallest byte. *)
       let ns =
         [ "0x3f20_3040_5060_0708"
         ; "0x7f20_3040_5060_0708"
         ; "-0x7f20_3040_5060_0708"
         ; "0x7fff_ffff_ffff_0000"
         ; "0"
         ]
         |> List.map ~f:Int.of_string
       ;;

       let num_bytes = 8
       let signed = true

       type t = int

       let of_int64 = Int64.to_int_trunc
       let to_int64 = Int64.of_int
       let pack = pack_signed_64_int
       let unpack = unpack_signed_64_int
       let pack_big_endian = pack_signed_64_int_big_endian
       let unpack_big_endian = unpack_signed_64_int_big_endian
       let pack_little_endian = pack_signed_64_int_little_endian
       let unpack_little_endian = unpack_signed_64_int_little_endian
     end))
;;

let%test_module "inline_tail_padded_fixed_string" =
  (module struct
    let test_last_nonmatch_plus_one ~buf ~min_pos ~pos ~char ~expect =
      Private.last_nonmatch_plus_one ~buf:(Bytes.of_string buf) ~min_pos ~pos ~char
      = expect
    ;;

    let%test _ =
      test_last_nonmatch_plus_one ~buf:"222121212" ~min_pos:3 ~pos:9 ~char:'2' ~expect:8
    ;;

    let%test _ =
      test_last_nonmatch_plus_one ~buf:"111121212" ~min_pos:3 ~pos:9 ~char:'1' ~expect:9
    ;;

    let%test _ =
      test_last_nonmatch_plus_one ~buf:"222121222" ~min_pos:3 ~pos:9 ~char:'2' ~expect:6
    ;;

    let%test _ =
      test_last_nonmatch_plus_one ~buf:"222222222" ~min_pos:3 ~pos:9 ~char:'2' ~expect:3
    ;;

    let%test _ =
      test_last_nonmatch_plus_one ~buf:"221222222" ~min_pos:3 ~pos:9 ~char:'2' ~expect:3
    ;;

    let%test _ =
      test_last_nonmatch_plus_one ~buf:"222122222" ~min_pos:3 ~pos:9 ~char:'2' ~expect:4
    ;;

    let%test _ =
      test_last_nonmatch_plus_one ~buf:"222122222" ~min_pos:3 ~pos:9 ~char:'1' ~expect:9
    ;;

    let%test _ =
      test_last_nonmatch_plus_one ~buf:"222122222" ~min_pos:3 ~pos:8 ~char:'1' ~expect:8
    ;;

    let%test _ =
      test_last_nonmatch_plus_one ~buf:"222122221" ~min_pos:3 ~pos:8 ~char:'1' ~expect:8
    ;;

    let%test _ =
      test_last_nonmatch_plus_one ~buf:"222122221" ~min_pos:3 ~pos:8 ~char:'2' ~expect:4
    ;;

    let test_unpack_tail_padded_fixed_string ~padding ~buf ~pos ~len ~expect =
      let result =
        unpack_tail_padded_fixed_string ~padding ~buf:(Bytes.of_string buf) ~pos ~len ()
      in
      result = expect
    ;;

    let%test _ =
      test_unpack_tail_padded_fixed_string
        ~padding:'.'
        ~buf:"ab..c."
        ~pos:1
        ~len:5
        ~expect:"b..c"
    ;;

    let%test _ =
      test_unpack_tail_padded_fixed_string
        ~padding:'.'
        ~buf:"ab..c."
        ~pos:1
        ~len:4
        ~expect:"b..c"
    ;;

    let%test _ =
      test_unpack_tail_padded_fixed_string
        ~padding:'.'
        ~buf:"ab..c."
        ~pos:1
        ~len:3
        ~expect:"b"
    ;;

    let%test _ =
      test_unpack_tail_padded_fixed_string
        ~padding:'.'
        ~buf:"ab..c."
        ~pos:1
        ~len:2
        ~expect:"b"
    ;;

    let%test _ =
      test_unpack_tail_padded_fixed_string
        ~padding:'.'
        ~buf:"ab..c."
        ~pos:1
        ~len:1
        ~expect:"b"
    ;;

    let%test _ =
      test_unpack_tail_padded_fixed_string
        ~padding:'.'
        ~buf:"ab..c"
        ~pos:2
        ~len:3
        ~expect:"..c"
    ;;

    let%test _ =
      test_unpack_tail_padded_fixed_string
        ~padding:'.'
        ~buf:"ab..c"
        ~pos:2
        ~len:2
        ~expect:""
    ;;

    let%test _ =
      test_unpack_tail_padded_fixed_string
        ~padding:'.'
        ~buf:"ab..cd"
        ~pos:2
        ~len:3
        ~expect:"..c"
    ;;

    let%test _ =
      test_unpack_tail_padded_fixed_string
        ~padding:'.'
        ~buf:"ab..cd"
        ~pos:2
        ~len:2
        ~expect:""
    ;;

    let%test _ =
      test_unpack_tail_padded_fixed_string
        ~padding:'.'
        ~buf:"ab..c."
        ~pos:2
        ~len:1
        ~expect:""
    ;;

    let%test _ =
      test_unpack_tail_padded_fixed_string
        ~padding:'.'
        ~buf:".....x"
        ~pos:0
        ~len:6
        ~expect:".....x"
    ;;

    let%test _ =
      test_unpack_tail_padded_fixed_string
        ~padding:'.'
        ~buf:".....x"
        ~pos:0
        ~len:5
        ~expect:""
    ;;

    let test_pack_tail_padded_fixed_string ~padding ~pos ~len str ~expect =
      let buf = Bytes.of_string "12345678" in
      pack_tail_padded_fixed_string ~padding ~buf ~pos ~len str;
      Bytes.to_string buf = expect
    ;;

    let%test _ =
      test_pack_tail_padded_fixed_string
        ~expect:"1abcd.78"
        ~padding:'.'
        ~pos:1
        ~len:5
        "abcd"
    ;;

    let%test _ =
      test_pack_tail_padded_fixed_string
        ~expect:"1abcde78"
        ~padding:'.'
        ~pos:1
        ~len:5
        "abcde"
    ;;

    let%test _ =
      test_pack_tail_padded_fixed_string ~expect:"1.....78" ~padding:'.' ~pos:1 ~len:5 ""
    ;;

    let%test _ =
      test_pack_tail_padded_fixed_string
        ~expect:"1.....78"
        ~padding:'.'
        ~pos:1
        ~len:5
        "..."
    ;;
  end)
;;

let test byte_order =
  let buf = Bytes.make 8 'a' in
  let test name to_string p u ns =
    List.iter ns ~f:(fun n ->
      p ~byte_order ~buf ~pos:0 n;
      let n' = u ~byte_order ~buf ~pos:0 in
      if n <> n'
      then
        failwith
          (sprintf "%s = unpack_%s (pack_%s %s)" (to_string n') name name (to_string n)))
  in
  test
    "signed_8"
    string_of_int
    (fun ~byte_order:_ ~buf ~pos i -> pack_signed_8 ~buf ~pos i)
    (fun ~byte_order:_ ~buf ~pos -> unpack_signed_8 ~buf ~pos)
    [ -0x80; -0x7F; -0xF; -1; 0; 1; 0xF; 0x7F ];
  test
    "signed_16"
    string_of_int
    pack_signed_16
    unpack_signed_16
    [ -0x8000; -0x7ABC; -0xFF; -1; 0; 1; 0xFF; 0x7ABC; 0x7FFF ];
  test
    "signed_32"
    Int32.to_string
    pack_signed_32
    unpack_signed_32
    [ -0x80000000l
    ; -0x76543210l
    ; -0xFFl
    ; Int32.minus_one
    ; Int32.zero
    ; Int32.one
    ; 0x76543210l
    ; 0x7FFFFFFFl
    ];
  test
    "signed_64"
    Int64.to_string
    pack_signed_64
    unpack_signed_64
    [ -0x8000_0000_0000_0000L
    ; -0x789A_BCDE_F012_3456L
    ; -0xFFL
    ; Int64.minus_one
    ; Int64.zero
    ; Int64.one
    ; 0x789A_BCDE_F012_3456L
    ; 0x7FFF_FFFF_FFFF_FFFFL
    ]
;;

let test () =
  test `Big_endian;
  test `Little_endian
;;
