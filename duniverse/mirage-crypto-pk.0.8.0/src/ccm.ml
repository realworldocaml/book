open Uncommon

let (<+>) = Cs.(<+>)

let block_size = 16

let flags bit6 len1 len2 =
  let byte = Cstruct.create 1
  and data = bit6 lsl 6 + len1 lsl 3 + len2 in
  Cstruct.set_uint8 byte 0 data ;
  byte

let encode_len_buf size value buf =
  let rec ass num = function
    | 0 -> Cstruct.set_uint8 buf 0 num
    | m -> Cstruct.set_uint8 buf m (num land 0xff) ; ass (num lsr 8) (pred m)
  in
  ass value (pred size)

let encode_len size value =
  let b = Cstruct.create size in
  encode_len_buf size value b ;
  b

let format nonce adata q t (* mac len *) =
  (* assume n <- [7..13] *)
  (* assume t is valid mac size *)
  (* n + q = 15 *)
  (* a < 2 ^ 64 *)
  let n = Cstruct.len nonce in
  let small_q = 15 - n in
  (* first byte (flags): *)
  (* reserved | adata | (t - 2) / 2 | q - 1 *)
  let b6 = if Cstruct.len adata = 0 then 0 else 1 in
  let flag = flags b6 ((t - 2) / 2) (small_q - 1) in
  (* first octet block:
     0          : flags
     1..15 - q  : N
     16 - q..15 : Q *)
  let qblock = encode_len small_q q in
  flag <+> nonce <+> qblock

let pad_block b =
  let size = Cstruct.len b in
  Cs.rpad b (size // block_size * block_size) 0

let gen_adata a =
  let lbuf =
    match Cstruct.len a with
    | x when x < (1 lsl 16 - 1 lsl 8) ->
       let buf = Cstruct.create 2 in
       Cstruct.BE.set_uint16 buf 0 x ;
       buf
    | x when x < (1 lsl 32)           ->
       let buf = Cstruct.create 4 in
       Cstruct.BE.set_uint32 buf 0 (Int32.of_int x) ;
       Cs.of_bytes [0xff ; 0xfe] <+> buf
    | x                               ->
       let buf = Cstruct.create 8 in
       Cstruct.BE.set_uint64 buf 0 (Int64.of_int x) ;
       Cs.of_bytes [0xff ; 0xff] <+> buf
  in
  pad_block (lbuf <+> a)

let gen_ctr_prefix nonce =
  let n = Cstruct.len nonce in
  let small_q = 15 - n in
  let flag = flags 0 0 (small_q - 1) in
  (flag <+> nonce, succ n, small_q)

let gen_ctr nonce i =
  let pre, _, q = gen_ctr_prefix nonce in
  pre <+> encode_len q i

let prepare_header nonce adata plen tlen =
  let ada = if Cstruct.len adata = 0 then Cstruct.empty else gen_adata adata in
  format nonce adata plen tlen <+> ada

type mode = Encrypt | Decrypt

let crypto_core ~cipher ~mode ~key ~nonce ~maclen ?(adata = Cstruct.empty) data =
  let datalen = Cstruct.len data in
  let cbcheader = prepare_header nonce adata datalen maclen in
  let target = Cstruct.create datalen in

  let blkprefix, blkpreflen, preflen = gen_ctr_prefix nonce in
  let ctrblock i block =
    Cstruct.blit blkprefix 0 block 0 blkpreflen ;
    encode_len_buf preflen i (Cstruct.shift block blkpreflen) ;
    cipher ~key block block
  in

  let cbc iv block =
    Cs.xor_into iv block block_size ;
    cipher ~key block block
  in

  let cbcprep =
    let rec doit iv block =
      match Cstruct.len block with
      | 0 -> iv
      | _ ->
         cbc iv block ;
         doit (Cstruct.sub block 0 block_size)
              (Cstruct.shift block block_size)
    in
    doit (Cstruct.create block_size) cbcheader
  in

  let rec loop iv ctr src target =
    let cbcblock =
      match mode with
      | Encrypt -> src
      | Decrypt -> target
    in
    match Cstruct.len src with
    | 0 -> iv
    | x when x < block_size ->
       let ctrbl = pad_block target in
       ctrblock ctr ctrbl ;
       Cstruct.blit ctrbl 0 target 0 x ;
       Cs.xor_into src target x ;
       let cbblock = pad_block cbcblock in
       cbc cbblock iv ;
       iv
    | _ ->
       ctrblock ctr target ;
       Cs.xor_into src target block_size ;
       cbc cbcblock iv ;
       loop iv
            (succ ctr)
            (Cstruct.shift src block_size)
            (Cstruct.shift target block_size)
  in
  let last = loop cbcprep 1 data target in
  let t = Cstruct.sub last 0 maclen in
  (target, t)

let crypto_t t nonce cipher key =
  let ctr = gen_ctr nonce 0 in
  cipher ~key ctr ctr ;
  Cs.xor_into ctr t (Cstruct.len t)

let valid_nonce nonce =
  let nsize = Cstruct.len nonce in
  if nsize < 7 || nsize > 13 then
    invalid_arg "CCM: nonce length not between 7 and 13: %d" nsize

let generation_encryption ~cipher ~key ~nonce ~maclen ?adata data =
  valid_nonce nonce;
  let cdata, t = crypto_core ~cipher ~mode:Encrypt ~key ~nonce ~maclen ?adata data in
  crypto_t t nonce cipher key ;
  cdata <+> t

let decryption_verification ~cipher ~key ~nonce ~maclen ?adata data =
  valid_nonce nonce;
  if Cstruct.len data < maclen then
    None
  else
    let pclen = Cstruct.len data - maclen in
    let cdata, t = crypto_core ~cipher ~mode:Decrypt ~key ~nonce ~maclen ?adata (Cstruct.sub data 0 pclen) in
    let t' = Cs.clone (Cstruct.sub data pclen maclen) in
    crypto_t t' nonce cipher key ;
    match Cstruct.equal t' t with
    | true  -> Some cdata
    | false -> None
