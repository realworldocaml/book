module Make (H : No_hash.S) = struct

  open No_uncommon

  type g =
    { mutable k      : Cstruct.t
    ; mutable v      : Cstruct.t
    ; mutable seeded : bool
    }

  let block = H.digest_size

  let (bx00, bx01) = Cs.(b 0x00, b 0x01)

  let k0 = Cs.create ~init:0x00 H.digest_size
  and v0 = Cs.create ~init:0x01 H.digest_size

  let create () = { k = k0 ; v = v0 ; seeded = false }

  let seeded ~g = g.seeded

  let reseed ~g cs =
    let (k, v) = (g.k, g.v) in
    let k = H.hmac ~key:k @@ Cstruct.concat [v; bx00; cs] in
    let v = H.hmac ~key:k v in
    let k = H.hmac ~key:k @@ Cstruct.concat [v; bx01; cs] in
    let v = H.hmac ~key:k v in
    g.k <- k ; g.v <- v ; g.seeded <- true

  let generate ~g bytes =
    if not g.seeded then raise Boot.Unseeded_generator ;
    let rec go acc k v = function
      | 0 -> (v, Cstruct.concat @@ List.rev acc)
      | i -> let v = H.hmac ~key:k v in go (v::acc) k v (pred i) in
    let (v, cs) = go [] g.k g.v (bytes // H.digest_size) in
    g.k <- H.hmac ~key:g.k Cs.(v <+> bx00);
    g.v <- H.hmac ~key:g.k v;
    Cstruct.sub cs 0 (imax 0 bytes)

  (* XXX *)
  let accumulate ~g:_ = invalid_arg "Implement Hmac_drgb.accumulate..."
end
