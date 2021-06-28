type t = Cstruct.buffer

let create () = Cstruct.to_bigarray (Cstruct.create 32)

external mul : t -> t -> t -> unit = "fiat_p256_caml_mul" [@@noalloc]

external sub : t -> t -> t -> unit = "fiat_p256_caml_sub" [@@noalloc]

external add : t -> t -> t -> unit = "fiat_p256_caml_add" [@@noalloc]

let r_squared =
  Cstruct.to_bigarray
    (Cstruct.of_hex
       "0300000000000000fffffffffbfffffffefffffffffffffffdffffff04000000")

let to_montgomery x = mul x x r_squared

let copy dst src = Bigarray_compat.Array1.blit src dst

external from_bytes_buf : t -> Cstruct.buffer -> unit
  = "fiat_p256_caml_from_bytes"
  [@@noalloc]

let checked_buffer cs =
  assert (Cstruct.len cs = 32);
  Cstruct.to_bigarray cs

let from_bytes fe cs = from_bytes_buf fe (checked_buffer cs)

let one_bytes =
  Cstruct.of_hex
    "010000000000000000000000fffffffffffffffffffffffffeffffff00000000"

let one () =
  let fe = create () in
  from_bytes fe one_bytes;
  fe

external nz : t -> bool = "fiat_p256_caml_nz" [@@noalloc]

external sqr : t -> t -> unit = "fiat_p256_caml_sqr"

external from_montgomery : t -> unit = "fiat_p256_caml_from_montgomery"
  [@@noalloc]

external to_bytes_buf : Cstruct.buffer -> t -> unit = "fiat_p256_caml_to_bytes"
  [@@noalloc]

let to_bytes cs fe = to_bytes_buf (checked_buffer cs) fe

external inv : t -> t -> unit = "fiat_p256_caml_inv" [@@noalloc]

let from_be_cstruct cs =
  let cs_rev = Cstruct.rev cs in
  let fe = create () in
  from_bytes fe cs_rev;
  to_montgomery fe;
  fe

external select_c : t -> bool -> t -> t -> unit = "fiat_p256_caml_select"

let select bit ~then_ ~else_ =
  let out = create () in
  select_c out bit then_ else_;
  out
