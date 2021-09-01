
let pitable = [|
  0xd9; 0x78; 0xf9; 0xc4; 0x19; 0xdd; 0xb5; 0xed;  0x28; 0xe9; 0xfd; 0x79; 0x4a; 0xa0; 0xd8; 0x9d;
  0xc6; 0x7e; 0x37; 0x83; 0x2b; 0x76; 0x53; 0x8e;  0x62; 0x4c; 0x64; 0x88; 0x44; 0x8b; 0xfb; 0xa2;
  0x17; 0x9a; 0x59; 0xf5; 0x87; 0xb3; 0x4f; 0x13;  0x61; 0x45; 0x6d; 0x8d; 0x09; 0x81; 0x7d; 0x32;
  0xbd; 0x8f; 0x40; 0xeb; 0x86; 0xb7; 0x7b; 0x0b;  0xf0; 0x95; 0x21; 0x22; 0x5c; 0x6b; 0x4e; 0x82;
  0x54; 0xd6; 0x65; 0x93; 0xce; 0x60; 0xb2; 0x1c;  0x73; 0x56; 0xc0; 0x14; 0xa7; 0x8c; 0xf1; 0xdc;
  0x12; 0x75; 0xca; 0x1f; 0x3b; 0xbe; 0xe4; 0xd1;  0x42; 0x3d; 0xd4; 0x30; 0xa3; 0x3c; 0xb6; 0x26;
  0x6f; 0xbf; 0x0e; 0xda; 0x46; 0x69; 0x07; 0x57;  0x27; 0xf2; 0x1d; 0x9b; 0xbc; 0x94; 0x43; 0x03;
  0xf8; 0x11; 0xc7; 0xf6; 0x90; 0xef; 0x3e; 0xe7;  0x06; 0xc3; 0xd5; 0x2f; 0xc8; 0x66; 0x1e; 0xd7;
  0x08; 0xe8; 0xea; 0xde; 0x80; 0x52; 0xee; 0xf7;  0x84; 0xaa; 0x72; 0xac; 0x35; 0x4d; 0x6a; 0x2a;
  0x96; 0x1a; 0xd2; 0x71; 0x5a; 0x15; 0x49; 0x74;  0x4b; 0x9f; 0xd0; 0x5e; 0x04; 0x18; 0xa4; 0xec;
  0xc2; 0xe0; 0x41; 0x6e; 0x0f; 0x51; 0xcb; 0xcc;  0x24; 0x91; 0xaf; 0x50; 0xa1; 0xf4; 0x70; 0x39;
  0x99; 0x7c; 0x3a; 0x85; 0x23; 0xb8; 0xb4; 0x7a;  0xfc; 0x02; 0x36; 0x5b; 0x25; 0x55; 0x97; 0x31;
  0x2d; 0x5d; 0xfa; 0x98; 0xe3; 0x8a; 0x92; 0xae;  0x05; 0xdf; 0x29; 0x10; 0x67; 0x6c; 0xba; 0xc9;
  0xd3; 0x00; 0xe6; 0xcf; 0xe1; 0x9e; 0xa8; 0x2c;  0x63; 0x16; 0x01; 0x3f; 0x58; 0xe2; 0x89; 0xa9;
  0x0d; 0x38; 0x34; 0x1b; 0xab; 0x33; 0xff; 0xb0;  0xbb; 0x48; 0x0c; 0x5f; 0xb9; 0xb1; 0xcd; 0x2e;
  0xc5; 0xf3; 0xdb; 0x47; 0xe5; 0xa5; 0x9c; 0x77;  0x0a; 0xa6; 0x20; 0x68; 0xfe; 0x7f; 0xc1; 0xad
|]

(* effective is sometimes named t1 *)
let tm effective =
  let t8 = (effective + 7) / 8 in
  (* RFC says (TM = 255 MOD 2^(8 + effective - 8*T8)) *)
  let bits = 8 + effective - 8 * t8 in
  (* likely there's a smarter way to do this *)
  let rec c acc = function
    | 0 -> acc
    | n -> c ((acc lsl 1) + 1) (pred n)
  in
  t8, c 0 bits

(* L[i] is the i-th byte of the key; K[i] is the i-th 16-bit-word of the key *)
let key_expansion effective key =
  (* result is a 128 byte key, where we need the words.. *)
  let t = Cstruct.length key in
  let l = Array.init 128 (fun idx -> if idx < t then Cstruct.get_uint8 key idx else 0) in
  let t8, tm = tm effective in
  for i = t to 127 do
    l.(i) <- pitable.((l.(i - 1) + l.(i - t)) mod 256)
  done;
  l.(128 - t8) <- pitable.(l.(128 - t8) land tm);
  for i = 127 - t8 downto 0 do
    l.(i) <- pitable.(l.(i + 1) lxor l.(i + t8));
  done;
  Array.init 64 (fun idx -> l.(2 * idx) + 256 * l.(2 * idx + 1))

let mod16 f = 0xFFFF land f

let rol16 x k = mod16 ((x lsl k) lor (x lsr (16 - k)))

let ror16 x k = mod16 ((x lsr k) lor (x lsl (16 - k)))

let not16 x = mod16 (lnot x)

let s = Array.init 4 (function 0 -> 1 | 1 -> 2 | 2 -> 3 | 3 -> 5 | _ -> assert false)

let pmod a =
  let b = 4 in
  let r = a mod b in
  if r < 0 then (r + b) mod b else r

(* only used for encryption which we don't support
let mix r i k j =
  r.(i) <- mod16 (r.(i) + k.(j) + r.(pmod (i - 1)) land r.(pmod (i - 2)) +
                  (not16 r.(pmod (i - 1))) land r.(pmod (i - 3)));
  let j = succ j in
  r.(i) <- rol16 r.(i) s.(i);
  j

let mix_round r k j =
  let j' = mix r 0 k j in
  let j'' = mix r 1 k j' in
  let j''' = mix r 2 k j'' in
  let j'''' = mix r 3 k j''' in
  j''''

let mash r i k =
  r.(i) <- mod16 (r.(i) + k.(r.(pmod (i - 1)) land 63))

let mash_round r k =
  mash r 0 k;
  mash r 1 k;
  mash r 2 k;
  mash r 3 k

let encrypt_one ~key ~data =
  let r = Array.init 4 (fun idx -> Cstruct.LE.get_uint16 data (idx * 2)) in
  let j = 0 in
  let j = mix_round r key j in
  let j = mix_round r key j in
  let j = mix_round r key j in
  let j = mix_round r key j in
  let j = mix_round r key j in
  mash_round r key;
  let j = mix_round r key j in
  let j = mix_round r key j in
  let j = mix_round r key j in
  let j = mix_round r key j in
  let j = mix_round r key j in
  let j = mix_round r key j in
  mash_round r key;
  let j = mix_round r key j in
  let j = mix_round r key j in
  let j = mix_round r key j in
  let j = mix_round r key j in
  let _j = mix_round r key j in
  let out = Cstruct.create 8 in
  Cstruct.LE.set_uint16 out 0 r.(0);
  Cstruct.LE.set_uint16 out 2 r.(1);
  Cstruct.LE.set_uint16 out 4 r.(2);
  Cstruct.LE.set_uint16 out 6 r.(3);
  out
*)

let r_mix r i k j =
  r.(i) <- ror16 r.(i) s.(i);
  r.(i) <- mod16 (r.(i) - k.(j) -
                  (r.(pmod (i - 1)) land r.(pmod (i - 2))) -
                  (not16 r.(pmod (i - 1)) land (r.(pmod (i - 3)))));
  pred j

let r_mix_round r k j =
  let j' = r_mix r 3 k j in
  let j'' = r_mix r 2 k j' in
  let j''' = r_mix r 1 k j'' in
  let j'''' = r_mix r 0 k j''' in
  j''''

let r_mash r i k =
  r.(i) <- mod16 (r.(i) - k.(r.(pmod (i - 1)) land 63))

let r_mash_round r k =
  r_mash r 3 k;
  r_mash r 2 k;
  r_mash r 1 k;
  r_mash r 0 k

let decrypt_one ~key ~data ?(off = 0) dst =
  let r = Array.init 4 (fun idx -> Cstruct.LE.get_uint16 data (off + idx * 2)) in
  let j = 63 in
  let j = r_mix_round r key j in
  let j = r_mix_round r key j in
  let j = r_mix_round r key j in
  let j = r_mix_round r key j in
  let j = r_mix_round r key j in
  r_mash_round r key;
  let j = r_mix_round r key j in
  let j = r_mix_round r key j in
  let j = r_mix_round r key j in
  let j = r_mix_round r key j in
  let j = r_mix_round r key j in
  let j = r_mix_round r key j in
  r_mash_round r key;
  let j = r_mix_round r key j in
  let j = r_mix_round r key j in
  let j = r_mix_round r key j in
  let j = r_mix_round r key j in
  let _j = r_mix_round r key j in
  Cstruct.LE.set_uint16 dst (off + 0) r.(0);
  Cstruct.LE.set_uint16 dst (off + 2) r.(1);
  Cstruct.LE.set_uint16 dst (off + 4) r.(2);
  Cstruct.LE.set_uint16 dst (off + 6) r.(3)

let decrypt_cbc ?(effective = 128) ~key ~iv data =
  let block = 8 in
  let key = key_expansion effective key in
  let l = Cstruct.length data in
  let dst = Cstruct.create l in
  for i = 0 to pred ((l + pred block) / block) do
    decrypt_one ~key ~data ~off:(i * block) dst
  done;
  Mirage_crypto.Uncommon.Cs.xor_into iv dst block;
  Mirage_crypto.Uncommon.Cs.xor_into data (Cstruct.shift dst block) (l - block);
  dst
