let dh ~scalar ~point =
  Point.x_of_finite_point (Scalar_mult.scalar_mult scalar point)

let public scalar = Scalar_mult.scalar_mult scalar Point.params_g

type error = Error.point_error

let pp_error = Error.pp_point_error

let check_point = function
  | Ok p when not (Point.is_infinity p) -> Ok p
  | Ok _ -> Error `At_infinity
  | Error _ as e -> e

let point_of_cs c = check_point (Point.of_cstruct c)

let point_to_cs = Point.to_cstruct

type secret = Scalar.t

let secret_of_cs = Scalar.of_cstruct

let rec generate_private_key ~rng () =
  let candidate = rng 32 in
  match secret_of_cs candidate with
  | Ok secret -> secret
  | Error `Invalid_length ->
      failwith "Fiat_p256.gen_key: generator returned an invalid length"
  | Error _ -> generate_private_key ~rng ()

let gen_key ~rng =
  let private_key = generate_private_key ~rng () in
  let public_key = public private_key in
  let to_send = point_to_cs public_key in
  (private_key, to_send)

let key_exchange secret received =
  match point_of_cs received with
  | Error _ as err -> err
  | Ok other_party_public_key ->
      Ok (dh ~scalar:secret ~point:other_party_public_key)
