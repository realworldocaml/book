type error = [
  | `Invalid_format
  | `Invalid_length
  | `Invalid_range
  | `Not_on_curve
  | `At_infinity
  | `Low_order
]

let error_to_string = function
  | `Invalid_format -> "invalid format"
  | `Not_on_curve -> "point is not on curve"
  | `At_infinity -> "point is at infinity"
  | `Invalid_length -> "invalid length"
  | `Invalid_range -> "invalid range"
  | `Low_order -> "low order"

let pp_error fmt e =
  Format.fprintf fmt "Cannot parse point: %s" (error_to_string e)

exception Message_too_long

let bit_at buf i =
  let byte_num = i / 8 in
  let bit_num = i mod 8 in
  let byte = Cstruct.get_uint8 buf byte_num in
  byte land (1 lsl bit_num) <> 0

module type Dh = sig
  type secret

  val secret_of_cs : ?compress:bool -> Cstruct.t ->
    (secret * Cstruct.t, error) result

  val gen_key : ?compress:bool -> ?g:Mirage_crypto_rng.g -> unit ->
    secret * Cstruct.t

  val key_exchange : secret -> Cstruct.t -> (Cstruct.t, error) result
end

module type Dsa = sig
  type priv

  type pub

  val priv_of_cstruct : Cstruct.t -> (priv, error) result

  val priv_to_cstruct : priv -> Cstruct.t

  val pub_of_cstruct : Cstruct.t -> (pub, error) result

  val pub_to_cstruct : ?compress:bool -> pub -> Cstruct.t

  val pub_of_priv : priv -> pub

  val generate : ?g:Mirage_crypto_rng.g -> unit -> priv * pub

  val sign : key:priv -> ?k:Cstruct.t -> Cstruct.t -> Cstruct.t * Cstruct.t

  val verify : key:pub -> Cstruct.t * Cstruct.t -> Cstruct.t -> bool

  module K_gen (H : Mirage_crypto.Hash.S) : sig

    val generate : key:priv -> Cstruct.t -> Cstruct.t
  end
end
module type Dh_dsa = sig
  module Dh : Dh
  module Dsa : Dsa
end

module type Parameters = sig
  val a : Cstruct.t
  val b : Cstruct.t
  val g_x : Cstruct.t
  val g_y : Cstruct.t
  val p : Cstruct.t
  val n : Cstruct.t
  val pident: Cstruct.t
  val byte_length : int
  val fe_length : int
  val first_byte_bits : int option
end

type field_element = Cstruct.buffer

type point = { f_x : field_element; f_y : field_element; f_z : field_element }

type scalar = Scalar of Cstruct.t

module type Foreign = sig
  val mul : field_element -> field_element -> field_element -> unit
  val sub : field_element -> field_element -> field_element -> unit
  val add : field_element -> field_element -> field_element -> unit
  val to_montgomery : field_element -> unit
  val from_bytes_buf : field_element -> Cstruct.buffer -> unit
  val set_one : field_element -> unit
  val nz : field_element -> bool
  val sqr : field_element -> field_element -> unit
  val from_montgomery : field_element -> unit
  val to_bytes_buf : Cstruct.buffer -> field_element -> unit
  val inv : field_element -> field_element -> unit
  val select_c : field_element -> bool -> field_element -> field_element -> unit

  val double_c : point -> point -> unit
  val add_c : point -> point -> point -> unit
end

module type Field_element = sig
  val create : unit -> field_element

  val copy : field_element -> field_element -> unit

  val one : unit -> field_element

  val to_bytes : Cstruct.t -> field_element -> unit

  val from_montgomery : field_element -> unit

  val add : field_element -> field_element -> field_element -> unit

  val sub : field_element -> field_element -> field_element -> unit

  val mul : field_element -> field_element -> field_element -> unit

  val nz : field_element -> bool

  val sqr : field_element -> field_element -> unit

  val inv : field_element -> field_element -> unit

  val from_be_cstruct : Cstruct.t -> field_element

  val select : bool -> then_:field_element -> else_:field_element -> field_element
end

module Make_field_element (P : Parameters) (F : Foreign) : Field_element = struct
  include F

  let create () = Cstruct.to_bigarray (Cstruct.create P.fe_length)

  let copy dst src = Bigarray.Array1.blit src dst

  let checked_buffer cs =
    assert (Cstruct.length cs = P.byte_length);
    Cstruct.to_bigarray cs

  let from_bytes fe cs =
    F.from_bytes_buf fe (checked_buffer cs)

  let one () =
    let fe = create () in
    F.set_one fe;
    fe

  let to_bytes cs fe =
    F.to_bytes_buf (checked_buffer cs) fe

  let from_be_cstruct cs =
    let cs_rev = Cstruct.rev cs in
    let fe = create () in
    from_bytes fe cs_rev;
    F.to_montgomery fe;
    fe

  let select bit ~then_ ~else_ =
    let out = create () in
    F.select_c out bit then_ else_;
    out
end

module type Point = sig
  val at_infinity : unit -> point

  val is_infinity : point -> bool

  val add : point -> point -> point

  val double : point -> point

  val of_cstruct : Cstruct.t -> (point, error) result

  val to_cstruct : compress:bool -> point -> Cstruct.t

  val to_affine_raw : point -> (field_element * field_element) option

  val x_of_finite_point : point -> Cstruct.t

  val params_g : point

  val select : bool -> then_:point -> else_:point -> point
end

module Make_point (P : Parameters) (F : Foreign) : Point = struct
  module Fe = Make_field_element(P)(F)

  let at_infinity () =
    let f_x = Fe.one () in
    let f_y = Fe.one () in
    let f_z = Fe.create () in
    { f_x; f_y; f_z }

  let is_infinity p = not (Fe.nz p.f_z)

  let is_solution_to_curve_equation =
    let a = Fe.from_be_cstruct P.a in
    let b = Fe.from_be_cstruct P.b in
    fun ~x ~y ->
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
    (* ensure cs < p: *)
    match Eqaf_cstruct.compare_be_with_len ~len:P.byte_length cs P.p >= 0 with
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

  let to_affine_raw p =
    if is_infinity p then
      None
    else
      let z1 = Fe.create () in
      let z2 = Fe.create () in
      Fe.copy z1 p.f_z;
      Fe.from_montgomery z1;
      Fe.inv z2 z1;
      Fe.sqr z1 z2;
      Fe.from_montgomery z1;
      let x = Fe.create () in
      Fe.copy x p.f_x;
      Fe.mul x x z1;
      let y = Fe.create () in
      Fe.copy y p.f_y;
      Fe.mul z1 z1 z2;
      Fe.mul y y z1;
      Some (x, y)

  let to_affine p =
    match to_affine_raw p with
    | None -> None
    | Some (x, y) ->
      let out_x = Cstruct.create P.byte_length in
      let out_y = Cstruct.create P.byte_length in
      Fe.to_bytes out_x x;
      Fe.to_bytes out_y y;
      Some (out_x, out_y)

  let to_cstruct ~compress p =
    let buf =
      match to_affine p with
      | None -> Cstruct.create 1
      | Some (x, y) ->
        let four = Cstruct.create 1 in
        Cstruct.set_uint8 four 0 4;
        let rev_x = Cstruct.rev x and rev_y = Cstruct.rev y in
        Cstruct.concat [ four; rev_x; rev_y ]
    in
    if compress then
      let out = Cstruct.create (P.byte_length + 1) in
      let ident =
        2 + (Cstruct.get_uint8 buf ((P.byte_length * 2) - 1)) land 1
      in
      Cstruct.blit buf 1 out 1 P.byte_length;
      Cstruct.set_uint8 out 0 ident;
      out
    else
      buf

  let double p =
    let out = { f_x = Fe.create (); f_y = Fe.create (); f_z = Fe.create () } in
    F.double_c out p;
    out

  let add fe_p fe_q =
    let out = { f_x = Fe.create (); f_y = Fe.create (); f_z = Fe.create () } in
    F.add_c out fe_p fe_q;
    out

  let x_of_finite_point p =
    match to_affine p with None -> assert false | Some (x, _) -> Cstruct.rev x

  let params_g =
    match validate_finite_point ~x:P.g_x ~y:P.g_y with
    | Ok p -> p
    | Error _ -> assert false

  let select bit ~then_ ~else_ =
    {
      f_x = Fe.select bit ~then_:then_.f_x ~else_:else_.f_x;
      f_y = Fe.select bit ~then_:then_.f_y ~else_:else_.f_y;
      f_z = Fe.select bit ~then_:then_.f_z ~else_:else_.f_z;
    }

  let pow =
    let mult a b =
      let r = Fe.create () in
      Fe.mul r a b;
      r
    in
    let sqr x =
      let r = Fe.create () in
      Fe.sqr r x;
      r
    in
    fun x exp ->
    let r0 = ref (Fe.one ()) in
    let r1 =  ref x in
    for i = P.byte_length * 8 - 1 downto 0 do
      let bit = bit_at exp i in
      let multiplied = mult !r0 !r1 in
      let r0_sqr = sqr !r0 in
      let r1_sqr = sqr !r1 in
      r0 := Fe.select bit ~then_:multiplied ~else_:r0_sqr;
      r1 := Fe.select bit ~then_:r1_sqr ~else_:multiplied;
    done;
    !r0

  let decompress =
  (* When p = 4*k+3, as is the case of NIST-P256, there is an efficient square
     root algorithm to recover the y, as follows:

    Given the compact representation of Q as x,
     y2 = x^3 + a*x + b
     y' = y2^((p+1)/4)
     y = min(y',p-y')
     Q=(x,y) is the canonical representation of the point
  *)
    let pident = P.pident (* (Params.p + 1) / 4*) in
    let a = Fe.from_be_cstruct P.a in
    let b = Fe.from_be_cstruct P.b in
    let p = Fe.from_be_cstruct P.p in
    fun pk_cstruct ->
      let x = Fe.from_be_cstruct (Cstruct.sub pk_cstruct 1 P.byte_length) in
      let x3 = Fe.create () in
      let ax = Fe.create () in
      let sum = Fe.create () in
      Fe.mul x3 x x;
      Fe.mul x3 x3 x; (* x3 *)
      Fe.mul ax a x;  (* ax *)
      Fe.add sum x3 ax;
      Fe.add sum sum b; (* y^2 *)
      let y = pow sum pident in (* https://tools.ietf.org/id/draft-jivsov-ecc-compact-00.xml#sqrt point 4.3*)
      let y' = Fe.create () in
      Fe.sub y' p y;
      let y_struct = Cstruct.create (P.byte_length) in
      Fe.from_montgomery y;
      Fe.to_bytes y_struct y; (* number must not be in montgomery domain*)
      let y_struct = Cstruct.rev y_struct in
      let y_struct2 = Cstruct.create (P.byte_length) in
      Fe.from_montgomery y';
      Fe.to_bytes y_struct2 y';(* number must not be in montgomery domain*)
      let y_struct2 = Cstruct.rev y_struct2 in
      let ident = Cstruct.get_uint8 pk_cstruct 0 in
      let signY =
        2 + (Cstruct.get_uint8 y_struct (P.byte_length - 2)) land 1
      in
      let res = if Int.equal signY ident then y_struct else y_struct2 in
      let out = Cstruct.create ((P.byte_length * 2) + 1) in
      Cstruct.set_uint8 out 0 4;
      Cstruct.blit pk_cstruct 1 out 1 P.byte_length;
      Cstruct.blit res 0 out (P.byte_length + 1) P.byte_length;
      out

  let of_cstruct cs =
    let len = P.byte_length in
    if Cstruct.length cs = 0 then
      Error `Invalid_format
    else
      let of_cs cs =
        let x = Cstruct.sub cs 1 len in
        let y = Cstruct.sub cs (1 + len) len in
        validate_finite_point ~x ~y
      in
      match Cstruct.get_uint8 cs 0 with
      | 0x00 when Cstruct.length cs = 1 -> Ok (at_infinity ())
      | 0x02 | 0x03 when Cstruct.length P.pident > 0 ->
        let decompressed = decompress cs in
        of_cs decompressed
      | 0x04 when Cstruct.length cs = 1 + len + len ->
        of_cs cs
      | 0x00 | 0x04 -> Error `Invalid_length
      | _ -> Error `Invalid_format
end

module type Scalar = sig
  val not_zero : Cstruct.t -> bool

  val is_in_range : Cstruct.t -> bool

  val of_cstruct : Cstruct.t -> (scalar, error) result

  val to_cstruct : scalar -> Cstruct.t

  val scalar_mult : scalar -> point -> point
end

module Make_scalar (Param : Parameters) (P : Point) : Scalar = struct
  let not_zero =
    let zero = Cstruct.create Param.byte_length in
    fun cs -> not (Eqaf_cstruct.equal cs zero)

  let is_in_range cs =
    not_zero cs
    && Eqaf_cstruct.compare_be_with_len ~len:Param.byte_length Param.n cs > 0

  let of_cstruct cs =
    match is_in_range cs with
    | exception Invalid_argument _ -> Error `Invalid_length
    | true -> Ok (Scalar (Cstruct.rev cs))
    | false -> Error `Invalid_range

  let to_cstruct (Scalar cs) = Cstruct.rev cs

  let scalar_mult (Scalar s) p =
    let r0 = ref (P.at_infinity ()) in
    let r1 = ref p in
    for i = Param.byte_length * 8 - 1 downto 0 do
      let bit = bit_at s i in
      let sum = P.add !r0 !r1 in
      let r0_double = P.double !r0 in
      let r1_double = P.double !r1 in
      r0 := P.select bit ~then_:sum ~else_:r0_double;
      r1 := P.select bit ~then_:r1_double ~else_:sum
    done;
    !r0
end

module Make_dh (Param : Parameters) (P : Point) (S : Scalar) : Dh = struct
  let point_of_cs c =
    match P.of_cstruct c with
    | Ok p when not (P.is_infinity p) -> Ok p
    | Ok _ -> Error `At_infinity
    | Error _ as e -> e

  let point_to_cs = P.to_cstruct

  type secret = scalar

  let share ?(compress = false) private_key =
    let public_key = S.scalar_mult private_key P.params_g in
    point_to_cs ~compress public_key

  let secret_of_cs ?compress s =
    match S.of_cstruct s with
    | Ok p -> Ok (p, share ?compress p)
    | Error _ as e -> e

  let rec generate_private_key ?g () =
    let candidate = Mirage_crypto_rng.generate ?g Param.byte_length in
    match S.of_cstruct candidate with
    | Ok secret -> secret
    | Error _ -> generate_private_key ?g ()

  let gen_key ?compress ?g () =
    let private_key = generate_private_key ?g () in
    (private_key, share ?compress private_key)

  let key_exchange secret received =
    match point_of_cs received with
    | Error _ as err -> err
    | Ok shared -> Ok (P.x_of_finite_point (S.scalar_mult secret shared))
end

module type Foreign_n = sig
  val mul : field_element -> field_element -> field_element -> unit
  val add : field_element -> field_element -> field_element -> unit
  val inv : field_element -> field_element -> unit
  val one : field_element -> unit
  val from_bytes : field_element -> Cstruct.buffer -> unit
  val to_bytes : Cstruct.buffer -> field_element -> unit
  val from_montgomery : field_element -> field_element -> unit
  val to_montgomery : field_element -> field_element -> unit
end

module Make_dsa (Param : Parameters) (F : Foreign_n) (P : Point) (S : Scalar) (H : Mirage_crypto.Hash.S) = struct
  let create () = Cstruct.to_bigarray (Cstruct.create Param.fe_length)

  type priv = scalar

  let priv_of_cstruct = S.of_cstruct

  let priv_to_cstruct = S.to_cstruct

  let padded msg =
    let l = Cstruct.length msg in
    let bl = Param.byte_length in
    let first_byte_ok () =
      match Param.first_byte_bits with
      | None -> true
      | Some m -> (Cstruct.get_uint8 msg 0) land (0xFF land (lnot m)) = 0
    in
    if l > bl || (l = bl && not (first_byte_ok ())) then
      raise Message_too_long
    else if l = bl then
      msg
    else
      Cstruct.append (Cstruct.create (bl - l)) msg

  let from_be_cstruct v =
    let v' = create () in
    F.from_bytes v' (Cstruct.to_bigarray (Cstruct.rev v));
    v'

  let to_be_cstruct v =
    let cs = Cstruct.create Param.byte_length in
    F.to_bytes (Cstruct.to_bigarray cs) v;
    Cstruct.rev cs

  (* RFC 6979: compute a deterministic k *)
  module K_gen (H : Mirage_crypto.Hash.S) = struct

    let drbg : 'a Mirage_crypto_rng.generator =
      let module M = Mirage_crypto_rng.Hmac_drbg (H) in (module M)

    let g ~key cs =
      let g = Mirage_crypto_rng.create ~strict:true drbg in
      Mirage_crypto_rng.reseed ~g
        (Cstruct.append (S.to_cstruct key) cs);
      g

    (* take qbit length, and ensure it is suitable for ECDSA (> 0 & < n) *)
    let gen g =
      let rec go () =
        let r = Mirage_crypto_rng.generate ~g Param.byte_length in
        if S.is_in_range r then r else go ()
      in
      go ()

    let generate ~key cs = gen (g ~key (padded cs))
  end

  module K_gen_default = K_gen(H)

  type pub = point

  let pub_of_cstruct = P.of_cstruct

  let pub_to_cstruct ?(compress = false) pk = P.to_cstruct ~compress pk

  let generate ?g () =
    (* FIPS 186-4 B 4.2 *)
    let d =
      let rec one () =
        match S.of_cstruct (Mirage_crypto_rng.generate ?g Param.byte_length) with
        | Ok x -> x
        | Error _ -> one ()
      in
      one ()
    in
    let q = S.scalar_mult d P.params_g in
    (d, q)

  let x_of_finite_point_mod_n p =
    match P.to_affine_raw p with
    | None -> None
    | Some (x, _) ->
      F.to_montgomery x x;
      let o = create () in
      F.one o;
      F.mul x x o;
      F.from_montgomery x x;
      Some (to_be_cstruct x)

  let sign ~key ?k msg =
    let msg = padded msg in
    let e = from_be_cstruct msg in
    let g = K_gen_default.g ~key msg in
    let rec do_sign g =
      let again () =
        match k with
        | None -> do_sign g
        | Some _ -> invalid_arg "k not suitable"
      in
      let k' = match k with None -> K_gen_default.gen g | Some k -> k in
      let ksc = match S.of_cstruct k' with
        | Ok ksc -> ksc
        | Error _ -> invalid_arg "k not in range" (* if no k is provided, this cannot happen since K_gen_*.gen already preserves the Scalar invariants *)
      in
      let point = S.scalar_mult ksc P.params_g in
      match x_of_finite_point_mod_n point with
      | None -> again ()
      | Some r ->
        let r_mon = from_be_cstruct r in
        F.to_montgomery r_mon r_mon;
        let kinv = create () in
        let kmon = from_be_cstruct k' in
        F.to_montgomery kmon kmon;
        F.inv kinv kmon;
        F.to_montgomery kmon kinv;
        let rd = create () in
        let dmon = from_be_cstruct (S.to_cstruct key) in
        F.to_montgomery dmon dmon;
        F.mul rd r_mon dmon;
        let cmon = create () in
        let zmon = create () in
        F.to_montgomery zmon e;
        F.add cmon zmon rd;
        let smon = create () in
        F.mul smon kmon cmon;
        let s = create () in
        F.from_montgomery s smon;
        let s = to_be_cstruct s in
        if S.not_zero s && S.not_zero r then
          r, s
        else
          again ()
    in
    do_sign g

  let pub_of_priv priv = S.scalar_mult priv P.params_g

  let verify ~key (r, s) msg =
    try
      let r = padded r and s = padded s in
      if not (S.is_in_range r && S.is_in_range s) then
        false
      else
        let msg = padded msg in
        let z = from_be_cstruct msg in
        let s_inv = create () in
        let s_mon = from_be_cstruct s in
        F.to_montgomery s_mon s_mon;
        F.inv s_inv s_mon;
        let u1 = create () in
        F.to_montgomery s_inv s_inv;
        F.to_montgomery z z;
        F.mul u1 z s_inv;
        let u2 = create () in
        let r_mon = from_be_cstruct r in
        F.to_montgomery r_mon r_mon;
        F.mul u2 r_mon s_inv;
        F.from_montgomery u1 u1;
        F.from_montgomery u2 u2;
        match
          S.of_cstruct (to_be_cstruct u1),
          S.of_cstruct (to_be_cstruct u2)
        with
        | Ok u1, Ok u2 ->
          let point =
            P.add
              (S.scalar_mult u1 P.params_g)
              (S.scalar_mult u2 key)
          in
          begin match x_of_finite_point_mod_n point with
            | None -> false (* point is infinity *)
            | Some r' -> Cstruct.equal r r'
          end
        | Error _, _ | _, Error _ -> false
    with
    | Message_too_long -> false
end

module P224 : Dh_dsa = struct
  module Params = struct
    let a = Cstruct.of_hex "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFFFFFFFFFFFFFFFFFE"
    let b = Cstruct.of_hex "B4050A850C04B3ABF54132565044B0B7D7BFD8BA270B39432355FFB4"
    let g_x = Cstruct.of_hex "B70E0CBD6BB4BF7F321390B94A03C1D356C21122343280D6115C1D21"
    let g_y = Cstruct.of_hex "BD376388B5F723FB4C22DFE6CD4375A05A07476444D5819985007E34"
    let p = Cstruct.of_hex "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000001"
    let n = Cstruct.of_hex "FFFFFFFFFFFFFFFFFFFFFFFFFFFF16A2E0B8F03E13DD29455C5C2A3D"
    let pident = Cstruct.empty
    let byte_length = 28
    let fe_length = if Sys.word_size == 64 then 32 else 28 (* TODO: is this congruent with C code? *)
    let first_byte_bits = None
  end

  module Foreign = struct
    external mul : field_element -> field_element -> field_element -> unit = "mc_p224_mul" [@@noalloc]
    external sub : field_element -> field_element -> field_element -> unit = "mc_p224_sub" [@@noalloc]
    external add : field_element -> field_element -> field_element -> unit = "mc_p224_add" [@@noalloc]
    external to_montgomery : field_element -> unit = "mc_p224_to_montgomery" [@@noalloc]
    external from_bytes_buf : field_element -> Cstruct.buffer -> unit = "mc_p224_from_bytes" [@@noalloc]
    external set_one : field_element -> unit = "mc_p224_set_one" [@@noalloc]
    external nz : field_element -> bool = "mc_p224_nz" [@@noalloc]
    external sqr : field_element -> field_element -> unit = "mc_p224_sqr" [@@noalloc]
    external from_montgomery : field_element -> unit = "mc_p224_from_montgomery" [@@noalloc]
    external to_bytes_buf : Cstruct.buffer -> field_element -> unit = "mc_p224_to_bytes" [@@noalloc]
    external inv : field_element -> field_element -> unit = "mc_p224_inv" [@@noalloc]
    external select_c : field_element -> bool -> field_element -> field_element -> unit = "mc_p224_select" [@@noalloc]

    external double_c : point -> point -> unit = "mc_p224_point_double" [@@noalloc]
    external add_c : point -> point -> point -> unit = "mc_p224_point_add" [@@noalloc]
  end

  module Foreign_n = struct
    external mul : field_element -> field_element -> field_element -> unit = "mc_np224_mul" [@@noalloc]
    external add : field_element -> field_element -> field_element -> unit = "mc_np224_add" [@@noalloc]
    external inv : field_element -> field_element -> unit = "mc_np224_inv" [@@noalloc]
    external one : field_element -> unit = "mc_np224_one" [@@noalloc]
    external from_bytes : field_element -> Cstruct.buffer -> unit = "mc_np224_from_bytes" [@@noalloc]
    external to_bytes : Cstruct.buffer -> field_element -> unit = "mc_np224_to_bytes" [@@noalloc]
    external from_montgomery : field_element -> field_element -> unit = "mc_np224_from_montgomery" [@@noalloc]
    external to_montgomery : field_element -> field_element -> unit = "mc_np224_to_montgomery" [@@noalloc]
  end

  module P = Make_point(Params)(Foreign)
  module S = Make_scalar(Params)(P)
  module Dh = Make_dh(Params)(P)(S)
  module Dsa = Make_dsa(Params)(Foreign_n)(P)(S)(Mirage_crypto.Hash.SHA256)
end

module P256 : Dh_dsa  = struct
  module Params = struct
    let a = Cstruct.of_hex "FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFC"
    let b = Cstruct.of_hex "5AC635D8AA3A93E7B3EBBD55769886BC651D06B0CC53B0F63BCE3C3E27D2604B"
    let g_x =
      Cstruct.of_hex "6B17D1F2E12C4247F8BCE6E563A440F277037D812DEB33A0F4A13945D898C296"
    let g_y =
      Cstruct.of_hex "4FE342E2FE1A7F9B8EE7EB4A7C0F9E162BCE33576B315ECECBB6406837BF51F5"
    let p = Cstruct.of_hex "FFFFFFFF00000001000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF"
    let n = Cstruct.of_hex "FFFFFFFF00000000FFFFFFFFFFFFFFFFBCE6FAADA7179E84F3B9CAC2FC632551"
    let pident = Cstruct.of_hex "3FFFFFFFC0000000400000000000000000000000400000000000000000000000" |> Cstruct.rev (* (Params.p + 1) / 4*)
    let byte_length = 32
    let fe_length = 32
    let first_byte_bits = None
  end

  module Foreign = struct
    external mul : field_element -> field_element -> field_element -> unit = "mc_p256_mul" [@@noalloc]
    external sub : field_element -> field_element -> field_element -> unit = "mc_p256_sub" [@@noalloc]
    external add : field_element -> field_element -> field_element -> unit = "mc_p256_add" [@@noalloc]
    external to_montgomery : field_element -> unit = "mc_p256_to_montgomery" [@@noalloc]
    external from_bytes_buf : field_element -> Cstruct.buffer -> unit = "mc_p256_from_bytes" [@@noalloc]
    external set_one : field_element -> unit = "mc_p256_set_one" [@@noalloc]
    external nz : field_element -> bool = "mc_p256_nz" [@@noalloc]
    external sqr : field_element -> field_element -> unit = "mc_p256_sqr" [@@noalloc]
    external from_montgomery : field_element -> unit = "mc_p256_from_montgomery" [@@noalloc]
    external to_bytes_buf : Cstruct.buffer -> field_element -> unit = "mc_p256_to_bytes" [@@noalloc]
    external inv : field_element -> field_element -> unit = "mc_p256_inv" [@@noalloc]
    external select_c : field_element -> bool -> field_element -> field_element -> unit = "mc_p256_select" [@@noalloc]

    external double_c : point -> point -> unit = "mc_p256_point_double" [@@noalloc]
    external add_c : point -> point -> point -> unit = "mc_p256_point_add" [@@noalloc]
  end

  module Foreign_n = struct
    external mul : field_element -> field_element -> field_element -> unit = "mc_np256_mul" [@@noalloc]
    external add : field_element -> field_element -> field_element -> unit = "mc_np256_add" [@@noalloc]
    external inv : field_element -> field_element -> unit = "mc_np256_inv" [@@noalloc]
    external one : field_element -> unit = "mc_np256_one" [@@noalloc]
    external from_bytes : field_element -> Cstruct.buffer -> unit = "mc_np256_from_bytes" [@@noalloc]
    external to_bytes : Cstruct.buffer -> field_element -> unit = "mc_np256_to_bytes" [@@noalloc]
    external from_montgomery : field_element -> field_element -> unit = "mc_np256_from_montgomery" [@@noalloc]
    external to_montgomery : field_element -> field_element -> unit = "mc_np256_to_montgomery" [@@noalloc]
  end

  module P = Make_point(Params)(Foreign)
  module S = Make_scalar(Params)(P)
  module Dh = Make_dh(Params)(P)(S)
  module Dsa = Make_dsa(Params)(Foreign_n)(P)(S)(Mirage_crypto.Hash.SHA256)
end

module P384 : Dh_dsa = struct
  module Params = struct
    let a = Cstruct.of_hex "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFC"
    let b = Cstruct.of_hex "B3312FA7E23EE7E4988E056BE3F82D19181D9C6EFE8141120314088F5013875AC656398D8A2ED19D2A85C8EDD3EC2AEF"
    let g_x =
      Cstruct.of_hex "AA87CA22BE8B05378EB1C71EF320AD746E1D3B628BA79B9859F741E082542A385502F25DBF55296C3A545E3872760AB7"
    let g_y =
      Cstruct.of_hex "3617de4a96262c6f5d9e98bf9292dc29f8f41dbd289a147ce9da3113b5f0b8c00a60b1ce1d7e819d7a431d7c90ea0e5f"
    let p = Cstruct.of_hex "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFFFFFFFF0000000000000000FFFFFFFF"
    let n = Cstruct.of_hex "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC7634D81F4372DDF581A0DB248B0A77AECEC196ACCC52973"
    let pident = Cstruct.of_hex "3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFBFFFFFFFC00000000000000040000000" |> Cstruct.rev (* (Params.p + 1) / 4*)
    let byte_length = 48
    let fe_length = 48
    let first_byte_bits = None
  end

  module Foreign = struct
    external mul : field_element -> field_element -> field_element -> unit = "mc_p384_mul" [@@noalloc]
    external sub : field_element -> field_element -> field_element -> unit = "mc_p384_sub" [@@noalloc]
    external add : field_element -> field_element -> field_element -> unit = "mc_p384_add" [@@noalloc]
    external to_montgomery : field_element -> unit = "mc_p384_to_montgomery" [@@noalloc]
    external from_bytes_buf : field_element -> Cstruct.buffer -> unit = "mc_p384_from_bytes" [@@noalloc]
    external set_one : field_element -> unit = "mc_p384_set_one" [@@noalloc]
    external nz : field_element -> bool = "mc_p384_nz" [@@noalloc]
    external sqr : field_element -> field_element -> unit = "mc_p384_sqr" [@@noalloc]
    external from_montgomery : field_element -> unit = "mc_p384_from_montgomery" [@@noalloc]
    external to_bytes_buf : Cstruct.buffer -> field_element -> unit = "mc_p384_to_bytes" [@@noalloc]
    external inv : field_element -> field_element -> unit = "mc_p384_inv" [@@noalloc]
    external select_c : field_element -> bool -> field_element -> field_element -> unit = "mc_p384_select" [@@noalloc]

    external double_c : point -> point -> unit = "mc_p384_point_double" [@@noalloc]
    external add_c : point -> point -> point -> unit = "mc_p384_point_add" [@@noalloc]
  end

  module Foreign_n = struct
    external mul : field_element -> field_element -> field_element -> unit = "mc_np384_mul" [@@noalloc]
    external add : field_element -> field_element -> field_element -> unit = "mc_np384_add" [@@noalloc]
    external inv : field_element -> field_element -> unit = "mc_np384_inv" [@@noalloc]
    external one : field_element -> unit = "mc_np384_one" [@@noalloc]
    external from_bytes : field_element -> Cstruct.buffer -> unit = "mc_np384_from_bytes" [@@noalloc]
    external to_bytes : Cstruct.buffer -> field_element -> unit = "mc_np384_to_bytes" [@@noalloc]
    external from_montgomery : field_element -> field_element -> unit = "mc_np384_from_montgomery" [@@noalloc]
    external to_montgomery : field_element -> field_element -> unit = "mc_np384_to_montgomery" [@@noalloc]
  end

  module P = Make_point(Params)(Foreign)
  module S = Make_scalar(Params)(P)
  module Dh = Make_dh(Params)(P)(S)
  module Dsa = Make_dsa(Params)(Foreign_n)(P)(S)(Mirage_crypto.Hash.SHA384)
end

module P521 : Dh_dsa = struct
  module Params = struct
    let a = Cstruct.of_hex "01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC"
    let b = Cstruct.of_hex "0051953EB9618E1C9A1F929A21A0B68540EEA2DA725B99B315F3B8B489918EF109E156193951EC7E937B1652C0BD3BB1BF073573DF883D2C34F1EF451FD46B503F00"
    let g_x =
      Cstruct.of_hex "00C6858E06B70404E9CD9E3ECB662395B4429C648139053FB521F828AF606B4D3DBAA14B5E77EFE75928FE1DC127A2FFA8DE3348B3C1856A429BF97E7E31C2E5BD66"
    let g_y =
      Cstruct.of_hex "011839296a789a3bc0045c8a5fb42c7d1bd998f54449579b446817afbd17273e662c97ee72995ef42640c550b9013fad0761353c7086a272c24088be94769fd16650"
    let p = Cstruct.of_hex "01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"
    let n = Cstruct.of_hex "01FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA51868783BF2F966B7FCC0148F709A5D03BB5C9B8899C47AEBB6FB71E91386409"
    let pident = Cstruct.of_hex "017fffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff" |> Cstruct.rev
    let byte_length = 66
    let fe_length = if Sys.word_size == 64 then 72 else 68  (* TODO: is this congruent with C code? *)
    let first_byte_bits = Some 0x01
  end

  module Foreign = struct
    external mul : field_element -> field_element -> field_element -> unit = "mc_p521_mul" [@@noalloc]
    external sub : field_element -> field_element -> field_element -> unit = "mc_p521_sub" [@@noalloc]
    external add : field_element -> field_element -> field_element -> unit = "mc_p521_add" [@@noalloc]
    external to_montgomery : field_element -> unit = "mc_p521_to_montgomery" [@@noalloc]
    external from_bytes_buf : field_element -> Cstruct.buffer -> unit = "mc_p521_from_bytes" [@@noalloc]
    external set_one : field_element -> unit = "mc_p521_set_one" [@@noalloc]
    external nz : field_element -> bool = "mc_p521_nz" [@@noalloc]
    external sqr : field_element -> field_element -> unit = "mc_p521_sqr" [@@noalloc]
    external from_montgomery : field_element -> unit = "mc_p521_from_montgomery" [@@noalloc]
    external to_bytes_buf : Cstruct.buffer -> field_element -> unit = "mc_p521_to_bytes" [@@noalloc]
    external inv : field_element -> field_element -> unit = "mc_p521_inv" [@@noalloc]
    external select_c : field_element -> bool -> field_element -> field_element -> unit = "mc_p521_select" [@@noalloc]

    external double_c : point -> point -> unit = "mc_p521_point_double" [@@noalloc]
    external add_c : point -> point -> point -> unit = "mc_p521_point_add" [@@noalloc]
  end

  module Foreign_n = struct
    external mul : field_element -> field_element -> field_element -> unit = "mc_np521_mul" [@@noalloc]
    external add : field_element -> field_element -> field_element -> unit = "mc_np521_add" [@@noalloc]
    external inv : field_element -> field_element -> unit = "mc_np521_inv" [@@noalloc]
    external one : field_element -> unit = "mc_np521_one" [@@noalloc]
    external from_bytes : field_element -> Cstruct.buffer -> unit = "mc_np521_from_bytes" [@@noalloc]
    external to_bytes : Cstruct.buffer -> field_element -> unit = "mc_np521_to_bytes" [@@noalloc]
    external from_montgomery : field_element -> field_element -> unit = "mc_np521_from_montgomery" [@@noalloc]
    external to_montgomery : field_element -> field_element -> unit = "mc_np521_to_montgomery" [@@noalloc]
  end

  module P = Make_point(Params)(Foreign)
  module S = Make_scalar(Params)(P)
  module Dh = Make_dh(Params)(P)(S)
  module Dsa = Make_dsa(Params)(Foreign_n)(P)(S)(Mirage_crypto.Hash.SHA512)
end

module X25519 = struct
  (* RFC 7748 *)
  external x25519_scalar_mult_generic : Cstruct.buffer -> Cstruct.buffer -> int -> Cstruct.buffer -> int -> unit = "mc_x25519_scalar_mult_generic" [@@noalloc]

  let key_len = 32

  let scalar_mult in_ base =
    let out = Cstruct.create key_len in
    x25519_scalar_mult_generic out.Cstruct.buffer
      in_.Cstruct.buffer in_.Cstruct.off base.Cstruct.buffer base.Cstruct.off;
    out

  type secret = Cstruct.t

  let basepoint =
    let data = Cstruct.create key_len in
    Cstruct.set_uint8 data 0 9;
    data

  let public priv = scalar_mult priv basepoint

  let gen_key ?compress:_ ?g () =
    let secret = Mirage_crypto_rng.generate ?g key_len in
    secret, public secret

  let secret_of_cs ?compress:_ s =
    if Cstruct.length s = key_len then
      Ok (s, public s)
    else
      Error `Invalid_length

  let is_zero =
    let zero = Cstruct.create key_len in
    fun cs -> Cstruct.equal zero cs

  let key_exchange secret public =
    if Cstruct.length public = key_len then
      let res = scalar_mult secret public in
      if is_zero res then Error `Low_order else Ok res
    else
      Error `Invalid_length
end

module Ed25519 = struct

  external scalar_mult_base_to_bytes : Cstruct.buffer -> Cstruct.buffer -> unit = "mc_25519_scalar_mult_base" [@@noalloc]
  external reduce_l : Cstruct.buffer -> unit = "mc_25519_reduce_l" [@@noalloc]
  external muladd : Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer -> unit = "mc_25519_muladd" [@@noalloc]
  external double_scalar_mult : Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer -> Cstruct.buffer -> int -> bool = "mc_25519_double_scalar_mult" [@@noalloc]
  external pub_ok : Cstruct.buffer -> bool = "mc_25519_pub_ok" [@@noalloc]

  type pub = Cstruct.t

  type priv = Cstruct.t

  (* RFC 8032 *)
  let key_len = 32

  let public secret =
    (* section 5.1.5 *)
    (* step 1 *)
    let h = Mirage_crypto.Hash.SHA512.digest secret in
    (* step 2 *)
    let s, rest = Cstruct.split h key_len in
    Cstruct.set_uint8 s 0 (Cstruct.get_uint8 s 0 land 248);
    Cstruct.set_uint8 s 31 ((Cstruct.get_uint8 s 31 land 127) lor 64);
    (* step 3 and 4 *)
    let public = Cstruct.create key_len in
    scalar_mult_base_to_bytes public.Cstruct.buffer s.Cstruct.buffer;
    public, (s, rest)

  let pub_of_priv secret = fst (public secret)

  let priv_of_cstruct cs =
    if Cstruct.length cs = key_len then Ok cs else Error `Invalid_length

  let priv_to_cstruct priv = priv

  let pub_of_cstruct cs =
    if Cstruct.length cs = key_len then
      let cs_copy = Cstruct.create key_len in
      Cstruct.blit cs 0 cs_copy 0 key_len;
      if pub_ok cs_copy.Cstruct.buffer then
        Ok cs_copy
      else
        Error `Not_on_curve
    else
      Error `Invalid_length

  let pub_to_cstruct pub = pub

  let generate ?g () =
    let secret = Mirage_crypto_rng.generate ?g key_len in
    secret, pub_of_priv secret

  let sign ~key msg =
    (* section 5.1.6 *)
    let pub, (s, prefix) = public key in
    let r = Mirage_crypto.Hash.SHA512.digest (Cstruct.append prefix msg) in
    reduce_l r.Cstruct.buffer;
    let r_big = Cstruct.create key_len in
    scalar_mult_base_to_bytes r_big.Cstruct.buffer r.Cstruct.buffer;
    let k = Mirage_crypto.Hash.SHA512.digest (Cstruct.concat [ r_big ; pub ; msg ]) in
    reduce_l k.Cstruct.buffer;
    let s_out = Cstruct.create key_len in
    muladd s_out.Cstruct.buffer k.Cstruct.buffer s.Cstruct.buffer r.Cstruct.buffer;
    Cstruct.append r_big s_out

  let verify ~key signature ~msg =
    (* section 5.1.7 *)
    if Cstruct.length signature = 2 * key_len then
      let r, s = Cstruct.split signature key_len in
      let s_smaller_l =
        (* check s within 0 <= s < L *)
        let s' = Cstruct.create (key_len * 2) in
        Cstruct.blit s 0 s' 0 key_len;
        reduce_l s'.Cstruct.buffer;
        let s'' = Cstruct.(append s (create key_len)) in
        Cstruct.equal s'' s'
      in
      if s_smaller_l then begin
        let k =
          Mirage_crypto.Hash.SHA512.digest (Cstruct.concat [ r ; key ; msg ])
        in
        reduce_l k.Cstruct.buffer;
        let r' = Cstruct.create key_len in
        let success =
          double_scalar_mult r'.Cstruct.buffer k.Cstruct.buffer
            key.Cstruct.buffer s.Cstruct.buffer s.Cstruct.off
        in
        success && Cstruct.equal r r'
      end else
        false
    else
      false
end
