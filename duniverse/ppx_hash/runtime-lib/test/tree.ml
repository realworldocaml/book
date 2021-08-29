open Core_kernel

(* Demonstrate the behaviour of folding-style hash functions *)

type result = Null | Elem of int | Combine of result * result [@@deriving sexp,compare]
let (%) a b = Combine(a,b)

(* Shadow the standard library *)
module Ppx_hash_lib = struct
  module Std = struct
    module Hash = struct
      type hash_value = result
      type state = result
      let fold_int s x = Combine (s, Elem x)
      let get_hash_value = Fn.id
      type seed = unit
      let create ?seed:_ () = Null
    end
    module Builtin = struct
      let hash_fold_int = Hash.fold_int
    end
  end
end

open Ppx_hash_lib.Std
open Builtin

module Tree = struct
  type t = L of int | N of t * t [@@deriving hash]
end

let a,b = 100,200
let node = 1
let leaf = 0
let tree = Tree.(N (L a, L b))

let%test_unit _ =
  let v = Tree.hash tree in
  (* Note: [%] associates to the left *)
  [%test_result: result] v ~expect: (Null % Elem node % Elem leaf % Elem a % Elem leaf % Elem b)
