open Nocrypto
open Uncommon


let devices  = [ "/dev/urandom"; "/dev/random" ]

let a_little = 32
let a_lot    = 1024


let fs_exists name =
  Unix.(try ignore (stat name); true with Unix_error (ENOENT, _, _) -> false)

let sys_rng =
  try List.find fs_exists devices with Not_found ->
    failwith "Nocrypto_entropy_unix: no random device"

(* XXX We need a direct form of this. *)
let read_cs fd n =
  let buf = Bytes.create n in
  let k   = Unix.read fd buf 0 n in
  let cs  = Cstruct.create k in
  Cstruct.blit_from_bytes buf 0 cs 0 k ;
  cs

let reseed ?(bytes = a_little) ?(device = sys_rng) g =
  let rec feed n fd =
    if n > 0 then
      let cs = read_cs fd n in
      Rng.reseed ~g cs ;
      feed (n - Cstruct.len cs) fd in
  bracket
    ~init:Unix.(fun () -> openfile device [O_RDONLY] 0)
    ~fini:Unix.close
    (feed bytes)

let initialize () =
  let g = !Rng.generator in
  if not (Rng.seeded (Some g)) then reseed ~bytes:a_lot g
