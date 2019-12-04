open! Import
open Base_bigstring

module Bigstring_sequence = struct
  type nonrec t = t

  let create ~len = create len
  let get = get
  let set = set
  let length = length
end

module Bytes_sequence = struct
  type t = bytes [@@deriving sexp_of]

  let create ~len = Bytes.create len
  let get = Bytes.get
  let set = Bytes.set
  let length = Bytes.length
end

module Blit_elt = struct
  include Char

  let of_bool b = if b then 'a' else 'b'
end

include Base_for_tests.Test_blit.Test(Blit_elt)(Bigstring_sequence)(Base_bigstring)

include Base_for_tests.Test_blit.Test_distinct
    (Blit_elt)
    (Bytes_sequence)
    (Bigstring_sequence)
    (Base_bigstring.From_bytes)

include Base_for_tests.Test_blit.Test_distinct
    (Blit_elt)
    (Bigstring_sequence)
    (Bytes_sequence)
    (Base_bigstring.To_bytes)

let%test_unit "roundtrip" =
  let string_gen =
    let open Quickcheck.Generator.Let_syntax in
    let%bind length = Quickcheck.Generator.small_non_negative_int in
    Quickcheck.Generator.list_with_length length Quickcheck.Generator.char_print
    >>| String.of_char_list
  in
  Quickcheck.test string_gen ~f:(fun str ->
    let bstr = of_string str in
    [%test_eq: Base_bigstring.t] bstr (of_string (to_string bstr));
    [%test_eq: Base_bigstring.t] bstr (of_bytes (to_bytes bstr));
    [%test_eq: Base_bigstring.t] bstr (t_of_sexp (sexp_of_t bstr));
  )

let%test "bigstring created with create are not mmapped" =
  not (is_mmapped (create 2))
