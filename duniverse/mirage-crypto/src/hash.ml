open Uncommon

type digest = Cstruct.t

type 'a iter = 'a Uncommon.iter

module type S = sig

  val digest_size : int

  type t

  val empty : t
  val feed  : t -> Cstruct.t -> t
  val get   : t -> Cstruct.t

  type hmac

  val hmac_empty : key:Cstruct.t -> hmac
  val hmac_feed : hmac -> Cstruct.t -> hmac
  val hmac_get : hmac -> digest

  val digest  : Cstruct.t -> digest
  val hmac    : key:Cstruct.t -> Cstruct.t -> digest

  val feedi   : t -> Cstruct.t iter -> t
  val digesti : Cstruct.t iter -> digest
  val hmaci   : key:Cstruct.t -> Cstruct.t iter -> digest
end

module type Foreign = sig

  open Native

  val init     : ctx -> unit
  val update   : ctx -> buffer -> int -> unit
  val finalize : ctx -> buffer -> unit
  val ctx_size : unit -> int
end

module type Desc = sig
  val block_size  : int
  val digest_size : int
end

module Core (F : Foreign) (D : Desc) = struct

  type t = Native.ctx

  include D

  let empty = Bytes.create (F.ctx_size ())

  let _ = F.init empty

  let update t buf =
    (* see issue #70 #81 #140 #143 for alignment considerations
       (allocation below) *)
    let l = Cstruct.length buf in
    let b =
      if buf.Cstruct.off = 0 then
        buf
      else
        let b = Cstruct.create l in
        Cstruct.blit buf 0 b 0 l;
        b
    in
    F.update t b.Cstruct.buffer l

  let finalize t =
    let res = Cstruct.create digest_size in
    F.finalize t res.Cstruct.buffer ;
    res

  let dup = Bytes.copy

  let get t = dup t |> finalize

  let feed t cs = let t = dup t in (update t cs ; t)

  let feedi t iter = let t = dup t in (iter (update t) ; t)

  let digest cs = feed empty cs |> finalize

  let digesti iter = feedi empty iter |> finalize
end

module Hash_of (F : Foreign) (D : Desc) = struct

  include Core (F) (D)

  type hmac = t * t

  let opad =
    let buf = Cstruct.create block_size in
    Cstruct.memset buf 0x5c;
    buf
  let ipad =
    let buf = Cstruct.create block_size in
    Cstruct.memset buf 0x36;
    buf

  let rec norm key =
    match compare (Cstruct.length key) block_size with
    |  1 -> norm (digest key)
    | -1 -> Cs.rpad key block_size 0
    |  _ -> key

  let hmac_empty ~key =
    let key = norm key in
    let outer = Cs.xor key opad
    and inner = Cs.xor key ipad in
    feed empty inner, feed empty outer

  let hmac_feed (t, outer) cs =
    feed t cs, outer

  let hmac_get (t, outer) =
    get (feed outer (get t))

  let hmaci ~key iter =
    let key = norm key in
    let outer = Cs.xor key opad
    and inner = Cs.xor key ipad in
    let rest = digesti (fun f -> f inner; iter f) in
    digesti (fun f -> f outer; f rest)

  let hmac ~key message = hmaci ~key (fun f -> f message)
end

module MD5 = Hash_of (Native.MD5) ( struct
  let (digest_size, block_size) = (16, 64)
end )

module SHA1 = Hash_of (Native.SHA1) ( struct
  let (digest_size, block_size) = (20, 64)
end )

module SHA224 = Hash_of (Native.SHA224) ( struct
  let (digest_size, block_size) = (28, 64)
end )

module SHA256 = Hash_of (Native.SHA256) ( struct
  let (digest_size, block_size) = (32, 64)
end )

module SHA384 = Hash_of (Native.SHA384) ( struct
  let (digest_size, block_size) = (48, 128)
end )

module SHA512 = Hash_of (Native.SHA512) ( struct
  let (digest_size, block_size) = (64, 128)
end )

type hash = [ `MD5 | `SHA1 | `SHA224 | `SHA256 | `SHA384 | `SHA512 ]

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
