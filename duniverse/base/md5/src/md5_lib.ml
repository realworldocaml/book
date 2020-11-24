type t = string

(* Share the digest of the empty string *)
let empty = Digest.string ""
let make s =
  if s = empty then
    empty
  else
    s

let compare = compare

let length = 16

let to_binary s = s
let of_binary_exn s = assert (String.length s = length); make s
let unsafe_of_binary = make

let to_hex = Digest.to_hex
let of_hex_exn s = make (Digest.from_hex s)

let string s = make (Digest.string s)

let bytes s = make (Digest.bytes s)

let subbytes bytes ~pos ~len = make (Digest.subbytes bytes pos len)
