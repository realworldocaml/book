open No_uncommon
open No_hash

module AES_CTR = No_cipher_block.AES.CTR

let block = 16

(* XXX Locking!! *)
type g =
  { mutable ctr    : AES_CTR.ctr
  ; mutable secret : Cstruct.t
  ; mutable key    : AES_CTR.key
  ; mutable trap   : (unit -> unit) option
  ; mutable seeded : bool
  }

let create () =
  let k = Cs.create 32 in
  { ctr    = (0L, 0L)
  ; secret = k
  ; key    = AES_CTR.of_secret k
  ; trap   = None
  ; seeded = false
  }

let clone ~g = { g with trap = None }

let seeded ~g = g.seeded

(* XXX We might want to erase the old key. *)
let set_key ~g sec =
  g.secret <- sec ;
  g.key    <- AES_CTR.of_secret sec

let reseedi ~g iter =
  set_key ~g @@ SHAd256.digesti (fun f -> f g.secret; iter f);
  g.ctr <- AES_CTR.add_ctr g.ctr 1L;
  g.seeded <- true

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

let generate ~g bytes =
  ( match g.trap with None -> () | Some f -> g.trap <- None ; f () );
  if not g.seeded then raise Boot.Unseeded_generator ;
  let rec chunk acc = function
    | i when i <= 0 -> acc
    | n -> let n' = imin n 0x10000 in
           chunk (generate_rekey ~g n' :: acc) (n - n') in
  Cstruct.concat @@ chunk [] bytes


module Accumulator = struct

  type t = {
    mutable count : int ;
    pools         : SHAd256.t array ;
    gen           : g ;
  }

  let create ~g = {
    pools = Array.make 32 SHAd256.empty ;
    count = 0 ;
    gen   = g
  }

  let fire acc =
    acc.count <- acc.count + 1;
    reseedi ~g:acc.gen @@ fun add ->
      for i = 0 to 31 do
        if acc.count land (1 lsl i - 1) = 0 then
          (SHAd256.get acc.pools.(i) |> add; acc.pools.(i) <- SHAd256.empty)
      done

  let add ~acc ~source ~pool data =
    let pool   = pool land 0x1f
    and source = source land 0xff in
    let header = Cs.of_bytes [ source ; Cstruct.len data ] in
    acc.pools.(pool) <- SHAd256.feedi acc.pools.(pool) (iter2 header data);
    (* XXX This is clobbered on multi-pool. *)
    acc.gen.trap <- Some (fun () -> fire acc)
end

(* XXX
 * Schneier recommends against using generator-imposed pool-seeding schedule
 * but it just makes for a horrid api.
 *)
let accumulate ~g =
  let acc  = Accumulator.create ~g
  and pool = ref 0 in
  `Acc (fun ~source cs ->
    Accumulator.add ~acc ~source ~pool:!pool cs ;
    incr pool)
