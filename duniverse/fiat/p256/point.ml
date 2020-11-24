module Fe = Field_element

type t = { f_x : Fe.t; f_y : Fe.t; f_z : Fe.t }

let at_infinity () =
  let f_x = Fe.one () in
  let f_y = Fe.one () in
  let f_z = Fe.create () in
  { f_x; f_y; f_z }

let is_infinity p = not (Fe.nz p.f_z)

let is_solution_to_curve_equation ~x ~y =
  let a = Fe.from_be_cstruct (Hex.to_cstruct Parameters.a) in
  let b = Fe.from_be_cstruct (Hex.to_cstruct Parameters.b) in
  let x3 = Fe.create () in
  Fe.mul x3 x x;
  Fe.mul x3 x3 x;
  let ax = Fe.create () in
  Fe.mul ax a x;
  let y2 = Fe.create () in
  Fe.mul y2 y y;
  let sum = Fe.create () in
  Fe.add sum x3 ax;
  Fe.add sum sum b;
  Fe.sub sum sum y2;
  not (Fe.nz sum)

let check_coordinate cs =
  let p = Hex.to_cstruct Parameters.p in
  (* ensure cs < p: *)
  match Eqaf_cstruct.compare_be_with_len ~len:32 cs p >= 0 with
  | true -> None
  | exception Invalid_argument _ -> None
  | false -> Some (Fe.from_be_cstruct cs)

(** Convert cstruct coordinates to a finite point ensuring:
    - x < p
    - y < p
    - y^2 = ax^3 + ax + b
*)
let validate_finite_point ~x ~y =
  match (check_coordinate x, check_coordinate y) with
  | Some f_x, Some f_y ->
      if is_solution_to_curve_equation ~x:f_x ~y:f_y then
        let f_z = Fe.one () in
        Ok { f_x; f_y; f_z }
      else Error `Not_on_curve
  | _ -> Error `Invalid_range

let first_byte cs =
  if Cstruct.len cs = 0 then None else Some (Cstruct.get_uint8 cs 0)

let of_cstruct cs =
  match (first_byte cs, Cstruct.len cs) with
  | Some 0x00, 1 -> Ok (at_infinity ())
  | Some 0x04, 65 ->
      let x = Cstruct.sub cs 1 32 in
      let y = Cstruct.sub cs 33 32 in
      validate_finite_point ~x ~y
  | Some 0x00, _ | Some 0x04, _ -> Error `Invalid_length
  | _, _ -> Error `Invalid_format

let to_affine p =
  if is_infinity p then None
  else
    let out_x = Cstruct.create 32 in
    let out_y = Cstruct.create 32 in
    let z1 = Fe.create () in
    let z2 = Fe.create () in
    Fe.copy z1 p.f_z;
    Fe.inv z2 z1;
    Fe.sqr z1 z2;
    Fe.from_montgomery z1;
    let x = Fe.create () in
    Fe.copy x p.f_x;
    Fe.mul x x z1;
    Fe.to_bytes out_x x;
    let y = Fe.create () in
    Fe.copy y p.f_y;
    Fe.mul z1 z1 z2;
    Fe.mul y y z1;
    Fe.to_bytes out_y y;
    Some (out_x, out_y)

let to_cstruct p =
  match to_affine p with
  | None -> Cstruct.create 1
  | Some (x, y) ->
      let four = Cstruct.create 1 in
      Cstruct.set_uint8 four 0 4;
      let rev_x = Cstruct.rev x and rev_y = Cstruct.rev y in
      Cstruct.concat [ four; rev_x; rev_y ]

external double_c : t -> t -> unit = "fiat_p256_caml_point_double" [@@noalloc]

let double p =
  let out = { f_x = Fe.create (); f_y = Fe.create (); f_z = Fe.create () } in
  double_c out p;
  out

external add_c : t -> t -> t -> unit = "fiat_p256_caml_point_add" [@@noalloc]

let add fe_p fe_q =
  let out = { f_x = Fe.create (); f_y = Fe.create (); f_z = Fe.create () } in
  add_c out fe_p fe_q;
  out

let x_of_finite_point p =
  match to_affine p with None -> assert false | Some (x, _) -> Cstruct.rev x

let params_g =
  let x = Hex.to_cstruct Parameters.g_x in
  let y = Hex.to_cstruct Parameters.g_y in
  match validate_finite_point ~x ~y with Ok p -> p | Error _ -> assert false

let select bit ~then_ ~else_ =
  {
    f_x = Fe.select bit ~then_:then_.f_x ~else_:else_.f_x;
    f_y = Fe.select bit ~then_:then_.f_y ~else_:else_.f_y;
    f_z = Fe.select bit ~then_:then_.f_z ~else_:else_.f_z;
  }
