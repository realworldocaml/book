
let description = "siphash"

type state
type hash_value = int
type seed = string

external alloc          : unit            -> state  = "siphash_alloc"
external reset_to       : state -> seed   -> state  = "siphash_reset"          [@@noalloc]
external fold_int64     : state -> int64  -> state  = "siphash_fold_int64"     [@@noalloc]
external fold_int       : state -> int    -> state  = "siphash_fold_int"       [@@noalloc]
external fold_float     : state -> float  -> state  = "siphash_fold_float"     [@@noalloc]
external fold_string    : state -> string -> state  = "siphash_fold_string"    [@@noalloc]
external get_hash_value : state -> hash_value       = "siphash_get_hash_value" [@@noalloc]

let default_seed = "the_default_seed"

let reset ?(seed=default_seed) t = reset_to t seed

module For_tests = struct
  external blit_state_to_bytes : state -> bytes -> unit =
    "siphash_blit_hash_to_bytes" [@@noalloc]
  let state_to_string state =
    let bytes = Bytes.create (8 * 4) in
    blit_state_to_bytes state bytes;
    Bytes.to_string bytes

  let compare_state a b = compare (state_to_string a) (state_to_string b)

end
