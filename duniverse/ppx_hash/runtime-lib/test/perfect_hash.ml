open Core
open Poly

(* This is not intended as a realistic candidate hash function since it allocates when
   hashing, but as some kind of `perfect' baseline against which other hash functions can
   be compared and judged.

   It is a perfect hash in the sense that it produces no collisions of intermediate state
   (trivially). It's also achieving about as good quality as possible in [get_hash_value]
   by virtue of using a (former) cryptographically secure hash function.

   Additionally, it tries to enforce the invariant well-behaved [hash_fold_t] functions
   must obey: different values of the same type must produce mix-in sequences
   of form (a @ [b1] @ c1) and (a @ [b2] @ c2) where b1 and b2 are "meaningfully"
   different (see the checks in [compare]).

   This requirement is a way to resolve possible systematic collisions resulting from e.g.
   forgetting to write down a tag of a variant, or a length of an array.

   It's not crazy to think about relaxing this requirement, but you can't relax it
   too much: you can't allow some [hash_fold_t] functions to write their tag
   to the end because that leads to a problem:
   even though [1; 2] differs from [1] in last position and [3] differs from [2; 3] in
   first position, ([1; 2] @ [3]) and ([1] @ [2; 3]) is a collision!
*)

let description = "perfect hash"

type hash_value = int

type v =
  | Int of int
  | Int64 of int64
  | String of string
  | Float of float
[@@deriving sexp]

let compare_v a b =
  match a, b with
  | Int a, Int b -> compare a b
  | Int64 a, Int64 b -> compare a b
  | String a, String b -> compare a b
  | Float a, Float b -> compare a b
  | _, _ -> failwith "uncomparable"
;;

module State = struct
  module T = struct
    type t = v list [@@deriving sexp]

    let rec compare a b =
      match a, b with
      | x :: xs, y :: ys -> [%compare: v * t] (x, xs) (y, ys)
      | [], [] -> 0
      | _, _ -> failwith "perfect hashes of different lengths"
    ;;

    let compare a b = compare (List.rev a) (List.rev b)
  end

  include T
  include Comparable.Make (T)
end

type state = State.t

let fold_int t i = Int i :: t
let fold_int64 t i = Int64 i :: t
let fold_string t i = String i :: t
let fold_float t i = Float i :: t

type seed = unit

let alloc () = []
let reset ?seed:_ _ = []

let get_hash_value t =
  Stdlib.Int64.to_int
    (Int64.of_string
       ("0x"
        ^ String.prefix
            (Md5.to_hex (Md5.digest_string (Sexplib.Sexp.to_string (State.sexp_of_t t))))
            16))
;;

module For_tests = struct
  let state_to_string t = Sexplib.Sexp.to_string (State.sexp_of_t t)
  let compare_state = State.compare
end
