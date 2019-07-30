open Core_kernel

(* record wrapped in variant *)
type r = {i:int;s:string} [@@deriving hash]
type vr = A of r | B [@@deriving hash]
let vr = A {i=42;s="hey"}

(* inline record *)
type ir = A of {i:int;s:string} | B [@@deriving hash]
let ir = A {i=42;s="hey"}

let%test_unit "hash unchanged by use of inline records" =
  [%test_eq: int] ([%hash: vr] vr) ([%hash: ir] ir)
