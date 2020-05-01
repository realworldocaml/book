module Bs = Spec_bs
module J = Spec_j

open Spec_t

type 'a j = 'a -> Yojson.Safe.t

module type Json = sig
  val r1     : r1 j
  val r2     : r2 j
  val r3     : r3 j
  val r4     : r4 j
  val r5     : r5 j
  val r6     : r6 j
  val r7     : r7 j
  val r8     : r8 j
  val j1     : j1 j
  val j2     : j2 j
  val j3     : j3 j
  val j4     : j4 j
  val o1     : o1 j
  val o2     : o2 j
  val t1     : t1 j
  val t2     : t2 j
  val v1list : v1list j
  val v2     : v2 j
  val v3list : v3list j
  val ages   : ages j
end

module Make (J : Json) = struct
  open J
  let massive_json =
    `List
      [ r1 { r1 = "testing"}
      ; r2 { r2 = Some 2 }
      ; r2 { r2 = None }
      ; r3 { r3 = Some 3 }
      ; r3 { r3 = None }
      ; r4 { r4 = true }
      ; r5 { r5 = Some 5 }
      ; r5 { r5 = None }
      ; r6 { r6 = 6 }
      ; r6 { r6 = 42 }
      ; r7 { r7 = -1_000 }
      ; r8 { r888 = [1; 2; 3] }
      ; j1 ["foo"; "bar"]
      ; j1 []
      ; j2 ()
      ; j3 [|1; 2; 3|]
      ; j4 'c'
      ; o1 [ "foo", 7; "bar", 8; "baz", 43 ]
      ; o2 [| "foo2", 5; "bar2", 6; "baz2", 41; "42", 42 |]
      ; t1 (100, "foo")
      ; t2 (100, 200, 42)
      ; t2 (100, 200, -1)
      ; v1list [ `V1; `V2; `V3 "testing";
                      `V4 255; `V5 None; `V5 (Some true)]
      ; v2 { v2 = `A }
      ; v2 { v2 = `B 100 }
      ; v3list [C1 ; C2 true; C2 false]
      ; ages [ `Age 50; `Age 30; `Age (-1); `Age 400]
      ]

  let pp_json fmt json =
    Format.pp_print_string fmt (Yojson.Safe.pretty_to_string ~std:true json)

  let run () = pp_json Format.std_formatter massive_json
end
