
module type S = sig
  val extract : ?salt:Cstruct.t -> Cstruct.t -> Cstruct.t
  val expand : prk:Cstruct.t -> ?info:Cstruct.t -> int -> Cstruct.t
end

module Make (H : Mirage_crypto.Hash.S) : S = struct
  let extract ?salt ikm =
    let key = match salt with
      | None -> Cstruct.create H.digest_size
      | Some x -> x
    in
    H.hmac ~key ikm

  let expand ~prk ?info len =
    let info = match info with
      | None -> Cstruct.empty
      | Some x -> x
    in
    let t n last =
      let nc = Cstruct.create 1 in
      Cstruct.set_uint8 nc 0 n ;
      H.hmac ~key:prk (Cstruct.concat [last ; info ; nc])
    in
    let n = succ (len / H.digest_size) in
    let rec compute acc count = match count, acc with
      | c, xs when c > n -> Cstruct.concat (List.rev xs)
      | c, x::_ -> compute (t c x :: acc) (succ c)
      | _, [] -> invalid_arg "can not happen"
    in
    let buf = compute [Cstruct.empty] 1 in
    Cstruct.sub buf 0 len
end

let extract ~hash ?salt ikm =
  let module H = (val (Mirage_crypto.Hash.module_of hash)) in
  let module HKDF = Make (H) in
  HKDF.extract ?salt ikm

let expand ~hash ~prk ?info len =
  let module H = (val (Mirage_crypto.Hash.module_of hash)) in
  let module HKDF = Make (H) in
  HKDF.expand ~prk ?info len
