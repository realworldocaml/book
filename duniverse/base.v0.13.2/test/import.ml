include Base
include Stdio
include Base_for_tests
include Expect_test_helpers_kernel

module Quickcheck = struct
  include Core_kernel.Quickcheck
  module Bool = Core_kernel.Bool
  module Char = Core_kernel.Char
  module Int = Core_kernel.Int
  module Int32 = Core_kernel.Int32
  module Int64 = Core_kernel.Int64
  module List = Core_kernel.List
  module Nativeint = Core_kernel.Nativeint
  module String = Core_kernel.String
end

module Core_kernel = struct end
[@@deprecated "[since 1970-01] Don't use Core_kernel in Base tests. Use Base."]

let () = Int_conversions.sexp_of_int_style := `Underscores
let is_none = Option.is_none
let is_some = Option.is_some
let ok_exn = Or_error.ok_exn
let stage = Staged.stage
let unstage = Staged.unstage

module type Hash = sig
  type t [@@deriving hash, sexp_of]
end

let check_hash_coherence (type t) here (module T : Hash with type t = t) ts =
  List.iter ts ~f:(fun t ->
    let hash1 = T.hash t in
    let hash2 = [%hash: T.t] t in
    require
      here
      (hash1 = hash2)
      ~cr:CR_soon
      ~if_false_then_print_s:
        (lazy [%message "" ~value:(t : T.t) (hash1 : int) (hash2 : int)]))
;;

module type Int_hash = sig
  include Hash

  val of_int_exn : int -> t
  val min_value : t
  val max_value : t
end

let check_int_hash_coherence (type t) here (module I : Int_hash with type t = t) =
  check_hash_coherence
    here
    (module I)
    [ I.min_value; I.of_int_exn 0; I.of_int_exn 37; I.max_value ]
;;
