open Mirage_crypto
open Mirage_crypto.Uncommon

module AES_CTR = Cipher_block.AES.CTR

module SHAd256 = struct
  open Hash
  type t = SHA256.t
  let empty     = SHA256.empty
  let get t     = SHA256.(get t |> digest)
  let digest x  = SHA256.(digest x |> digest)
  let digesti i = SHA256.(digesti i |> digest)
  let feedi     = SHA256.feedi
end

let block = 16

(* the minimal amount of bytes in a pool to trigger a reseed *)
let min_pool_size = 64
(* the minimal duration between two reseeds *)
let min_time_duration = 1_000_000_000L
(* number of pools *)
let pools = 32

(* XXX Locking!! *)
type g =
  { mutable ctr    : AES_CTR.ctr
  ; mutable secret : Cstruct.t
  ; mutable key    : AES_CTR.key
  ; pools          : SHAd256.t array
  ; mutable pool0_size : int
  ; mutable reseed_count : int
  ; mutable last_reseed : int64
  ; time : (unit -> int64) option
  }

let create ?time () =
  let k = Cstruct.create 32 in
  { ctr    = (0L, 0L)
  ; secret = k
  ; key    = AES_CTR.of_secret k
  ; pools  = Array.make pools SHAd256.empty
  ; pool0_size = 0
  ; reseed_count = 0
  ; last_reseed = 0L
  ; time
  }

let seeded ~g =
  let lo, hi = g.ctr in
  not (Int64.equal lo 0L && Int64.equal hi 0L)

(* XXX We might want to erase the old key. *)
let set_key ~g sec =
  g.secret <- sec ;
  g.key    <- AES_CTR.of_secret sec

let reseedi ~g iter =
  set_key ~g @@ SHAd256.digesti (fun f -> f g.secret; iter f);
  g.ctr <- AES_CTR.add_ctr g.ctr 1L

let iter1 a     f = f a

let reseed ~g cs = reseedi ~g (iter1 cs)

let generate_rekey ~g bytes =
  let b  = bytes // block + 2 in
  let n  = b * block in
  let r  = AES_CTR.stream ~key:g.key ~ctr:g.ctr n in
  let r1 = Cstruct.sub r 0 bytes
  and r2 = Cstruct.sub r (n - 32) 32 in
  set_key ~g r2 ;
  g.ctr <- AES_CTR.add_ctr g.ctr (Int64.of_int b);
  r1

let add_pool_entropy g =
  if g.pool0_size > min_pool_size then
    let should_reseed, now =
      match g.time with
      | None -> true, 0L
      | Some f ->
        let now = f () in
        Int64.(sub now g.last_reseed > min_time_duration), now
    in
    if should_reseed then begin
      g.reseed_count <- g.reseed_count + 1;
      g.last_reseed <- now;
      g.pool0_size <- 0;
      reseedi ~g @@ fun add ->
      for i = 0 to pools - 1 do
        if g.reseed_count land ((1 lsl i) - 1) = 0 then
          (SHAd256.get g.pools.(i) |> add; g.pools.(i) <- SHAd256.empty)
      done
    end

let generate ~g bytes =
  add_pool_entropy g;
  if not (seeded ~g) then raise Rng.Unseeded_generator ;
  let rec chunk acc = function
    | i when i <= 0 -> acc
    | n -> let n' = imin n 0x10000 in
           chunk (generate_rekey ~g n' :: acc) (n - n') in
  Cstruct.concat @@ chunk [] bytes

let add ~g (source, _) ~pool data =
  let pool   = pool land (pools - 1)
  and source = source land 0xff in
  let header = Cs.of_bytes [ source ; Cstruct.len data ] in
  g.pools.(pool) <- SHAd256.feedi g.pools.(pool) (iter2 header data);
  if pool = 0 then g.pool0_size <- g.pool0_size + Cstruct.len data

(* XXX
 * Schneier recommends against using generator-imposed pool-seeding schedule
 * but it just makes for a horrid api.
 *)
let accumulate ~g source =
  let pool = ref 0 in
  `Acc (fun cs ->
    add ~g source ~pool:!pool cs ;
    incr pool)
