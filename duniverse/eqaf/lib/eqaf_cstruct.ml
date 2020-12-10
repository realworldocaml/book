let equal a b =
  Eqaf_bigstring.equal (Cstruct.to_bigarray a) (Cstruct.to_bigarray b)

let compare_be_with_len ~len a b =
  Eqaf_bigstring.compare_be_with_len ~len
    (Cstruct.to_bigarray a) (Cstruct.to_bigarray b)

let compare_le_with_len ~len a b =
  Eqaf_bigstring.compare_le_with_len ~len
    (Cstruct.to_bigarray a) (Cstruct.to_bigarray b)

let compare_le a b =
  Eqaf_bigstring.compare_le
    (Cstruct.to_bigarray a) (Cstruct.to_bigarray b)

let compare_be a b =
  Eqaf_bigstring.compare_be
    (Cstruct.to_bigarray a) (Cstruct.to_bigarray b)

let find_uint8 ?off ~f v =
  Eqaf_bigstring.find_uint8
    ?off ~f (Cstruct.to_bigarray v)

let exists_uint8 ?off ~f v =
  Eqaf_bigstring.exists_uint8
    ?off ~f (Cstruct.to_bigarray v)
