open No_uncommon

type digest = Cstruct.t

type 'a iter = 'a No_uncommon.iter

type 'a or_digest = [ `Message of 'a | `Digest of digest ]

module type S = sig

  val digest_size : int

  type t

  val empty : t
  val feed  : t -> Cstruct.t -> t
  val get   : t -> Cstruct.t

  val digest  : Cstruct.t -> digest
  val hmac    : key:Cstruct.t -> Cstruct.t -> digest

  val feedi   : t -> Cstruct.t iter -> t
  val digesti : Cstruct.t iter -> digest
  val hmaci   : key:Cstruct.t -> Cstruct.t iter -> digest
end

module type Foreign = sig

  open No_native

  val init     : ctx -> unit
  val update   : ctx -> buffer -> int -> int -> unit
  val finalize : ctx -> buffer -> int -> unit
  val ctx_size : unit -> int
end

module type Desc = sig
  val block_size  : int
  val digest_size : int
end

module Core (F : Foreign) (D : Desc) = struct

  type t = No_native.ctx

  include D

  let empty = Bytes.create (F.ctx_size ())

  let _ = F.init empty

  let update t { Cstruct.buffer ; off ; len } =
    F.update t buffer off len

  let finalize t =
    let res = Cstruct.create digest_size in
    F.finalize t res.Cstruct.buffer res.Cstruct.off ;
    res

  let dup = Bytes.copy

  let get t = dup t |> finalize

  let feed t cs = let t = dup t in (update t cs ; t)

  let feedi t iter = let t = dup t in (iter (update t) ; t)

  let digest cs = feed empty cs |> finalize

  let digesti iter = feedi empty iter |> finalize
end

module Hash_of (F : Foreign) (D : Desc) = struct

  open Cs

  include Core (F) (D)

  let opad = create ~init:0x5c block_size
  let ipad = create ~init:0x36 block_size

  let rec norm key =
    match compare (Cstruct.len key) block_size with
    |  1 -> norm (digest key)
    | -1 -> rpad key block_size 0
    |  _ -> key

  let hmaci ~key iter =
    let key = norm key in
    let outer = xor key opad
    and inner = xor key ipad in
    let rest = digesti (fun f -> f inner; iter f) in
    digesti (fun f -> f outer; f rest)

  let hmac ~key message = hmaci ~key (fun f -> f message)
end

module MD5 = Hash_of (No_native.MD5) ( struct
  let (digest_size, block_size) = (16, 64)
end )

module SHA1 = Hash_of (No_native.SHA1) ( struct
  let (digest_size, block_size) = (20, 64)
end )

module SHA224 = Hash_of (No_native.SHA224) ( struct
  let (digest_size, block_size) = (28, 64)
end )

module SHA256 = Hash_of (No_native.SHA256) ( struct
  let (digest_size, block_size) = (32, 64)
end )

module SHA384 = Hash_of (No_native.SHA384) ( struct
  let (digest_size, block_size) = (48, 128)
end )

module SHA512 = Hash_of (No_native.SHA512) ( struct
  let (digest_size, block_size) = (64, 128)
end )

module SHAd256 = struct
  type t = SHA256.t
  let empty     = SHA256.empty
  let get t     = SHA256.(get t |> digest)
  let digest x  = SHA256.(digest x |> digest)
  let digesti i = SHA256.(digesti i |> digest)
  let feedi     = SHA256.feedi
end

type hash = [ `MD5 | `SHA1 | `SHA224 | `SHA256 | `SHA384 | `SHA512 ]
[@@deriving sexp]

let hashes = [ `MD5; `SHA1; `SHA224; `SHA256; `SHA384; `SHA512 ]

let md5    = (module MD5    : S)
and sha1   = (module SHA1   : S)
and sha224 = (module SHA224 : S)
and sha256 = (module SHA256 : S)
and sha384 = (module SHA384 : S)
and sha512 = (module SHA512 : S)

let module_of = function
  | `MD5    -> md5    | `SHA1   -> sha1   | `SHA224 -> sha224
  | `SHA256 -> sha256 | `SHA384 -> sha384 | `SHA512 -> sha512

let digest hash      = let module H = (val (module_of hash)) in H.digest
let digesti hash     = let module H = (val (module_of hash)) in H.digesti
let mac hash         = let module H = (val (module_of hash)) in H.hmac
let maci hash        = let module H = (val (module_of hash)) in H.hmaci
let digest_size hash = let module H = (val (module_of hash)) in H.digest_size

module Digest_or (H : S) = struct
  let digest_or = function
    | `Message msg   -> H.digest msg
    | `Digest digest ->
        let n = digest.Cstruct.len and m = H.digest_size in
        if n = m then digest else
          invalid_arg "(`Digest _): %d bytes, expecting %d" n m
end

let digest_or ~hash =
  let module H = (val (module_of hash)) in
  let module D = Digest_or (H) in
  D.digest_or
