type t = Scalar of Cstruct.t

let is_in_range cs =
  let zero = Cstruct.create 32 in
  let n = Hex.to_cstruct Parameters.n in
  Eqaf_cstruct.compare_be_with_len ~len:32 cs zero > 0
  && Eqaf_cstruct.compare_be_with_len ~len:32 n cs > 0

let of_cstruct cs =
  match is_in_range cs with
  | exception Invalid_argument _ -> Error `Invalid_length
  | true -> Ok (Scalar (Cstruct.rev cs))
  | false -> Error `Invalid_range

let bit_at (Scalar s) i =
  let byte_num = i / 8 in
  let bit_num = i mod 8 in
  let byte = Cstruct.get_uint8 s byte_num in
  byte land (1 lsl bit_num) <> 0
