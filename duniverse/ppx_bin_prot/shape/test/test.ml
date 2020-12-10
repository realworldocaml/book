open Core
open Poly

module Shape = struct
  include Bin_prot.Shape
  let annotate s xs = annotate (Uuid.of_string s) xs
  let basetype s x = basetype (Uuid.of_string s) x
end

module Canonical = struct
  include Shape.Canonical
  include Shape.Canonical.Create

  let annotate x t  = annotate (Shape.Uuid.of_string x) t
  let basetype s xs = basetype (Shape.Uuid.of_string s) xs

  let base0 s    = basetype s []
  let base1 s t1 = basetype s [t1]

  let unit       = base0 "unit"
  let bool       = base0 "bool"
  let string     = base0 "string"
  let char       = base0 "char"
  let int        = base0 "int"

  let float      = base0 "float"
  let option     = base1 "option"
  let list       = base1 "list"
  let array      = base1 "array"

  let dvariant x = define (variant x)

  let poly_variant = poly_variant (Shape.Location.of_string "somewhere")

end

let expect_raise f =
  assert(try (f (); false) with | _ -> true)

let eval_to_digest (exp:Shape.t) : string =
  (* Test result from `quick' [eval_to_digest] always matches the `slow' sequence:
     [eval] ; [to_digest]. *)
  let res_quick = Shape.eval_to_digest exp in
  let res_slow = Canonical.to_digest (Shape.eval exp) in
  [%test_result: Shape.Digest.t] res_quick ~expect:res_slow;
  Shape.Digest.to_hex res_quick

let ensure_all_different exps =
  let alist = List.map exps ~f:(fun e -> (eval_to_digest e, e)) in
  let ht = String.Map.of_alist_multi alist in
  List.iter (Map.to_alist ht) ~f:(fun (h,exps) ->
    match exps with
    | [] -> assert false
    | [_] -> ()
    | e1::_::_ ->
      failwithf "shapes are not all different: %s -> %s"
        (Canonical.to_string_hum (Shape.eval e1))
        h ()
  )

let ensure_all_same exps =
  let alist = List.map exps ~f:(fun e -> (eval_to_digest e, e)) in
  let m = String.Map.of_alist_multi alist in
  match Map.to_alist m with
  | [] -> ()
  | [_] -> ()
  | _::_::_ as xs ->
    failwithf "shapes are not all the same: %s" (
      String.concat ~sep:", " (
        List.map xs ~f:(fun (h,exps) ->
          let e1 = (match exps with [] -> assert false | e::_ -> e) in
          sprintf "%s -> %s"
            (Canonical.to_string_hum (Shape.eval e1))
            h))) ()

let ensure_shape exp ~expect =
  (* Test that a Shape.t [exp], evaluates to the expected
     Canonical.t [expect] *)
  [%test_result: Canonical.t] (Shape.eval exp) ~expect

let (=) x y = (eval_to_digest x = eval_to_digest y)
let (!=) x y = not (x = y)

(* meta-test that we are using sensible bindings for [=] and [!=] *)
let%test _ = ([%bin_shape: int] = [%bin_shape: int])
let%test _ = not ([%bin_shape: int] != [%bin_shape: int])

(* int/string have distinct hashes; but self-equal *)
let%test _ = ([%bin_shape: int] = [%bin_shape: int])
let%test _ = ([%bin_shape: string] = [%bin_shape: string])
let%test _ = ([%bin_shape: int] != [%bin_shape: string])

(* existence of some predefined bin_shape_<t> functions *)
let%test _ = ([%bin_shape: int] = bin_shape_int)
let%test _ = ([%bin_shape: string] = bin_shape_string)

(* different types must have different hashes *)
let%test_unit _ = ensure_all_different [
  [%bin_shape: int];
  [%bin_shape: string];
  [%bin_shape: float];
  [%bin_shape: int * int];
  [%bin_shape: int * string];
  [%bin_shape: string * int];
  [%bin_shape: string * string];
  [%bin_shape: int * int * int];
  [%bin_shape: (int * int) * int];
  [%bin_shape: int * (int * int)];
]

(* deriving for simple type_declaration *)
type my_int = int [@@deriving bin_shape]
let%test _ = ([%bin_shape: my_int] = [%bin_shape: int])
let%test _ = ([%bin_shape: my_int] = bin_shape_my_int)

(* deriving in structures/signatures *)
module My_int : sig
  type t = int [@@deriving bin_shape]
end = struct
  type t = int [@@deriving bin_shape]
end

(* Tvars in signatures.. *)
module Signatures = struct
  module type P1_sig = sig
    type 'a t = 'a * int [@@deriving bin_shape]
  end
  module P1 : P1_sig = struct
    type 'a t = 'a * int [@@deriving bin_shape]
  end
  module P2 : sig
    type ('a,'b) t = 'a * int * 'b [@@deriving bin_shape]
  end = struct
    type ('a,'b) t = 'a * int * 'b [@@deriving bin_shape]
  end
end

let%test _ = ([%bin_shape: My_int.t] = [%bin_shape: int])
let%test _ = ([%bin_shape: My_int.t] = My_int.bin_shape_t)

(* tuples *)
type ipair = int * int [@@deriving bin_shape]
let%test _ = ([%bin_shape: ipair] = bin_shape_ipair)
let%test _ = ([%bin_shape: ipair] = [%bin_shape: int * int])

type ipair2 = int * my_int [@@deriving bin_shape]
let%test _ = ([%bin_shape: ipair2] = [%bin_shape: ipair])

(* type variables and applications *)
type 'a pair = 'a * 'a [@@deriving bin_shape]
type 'a i_pair = int * 'a [@@deriving bin_shape]
type 'a pair_i = 'a * int [@@deriving bin_shape]
type ('a,'b) gpair = 'a * 'b [@@deriving bin_shape]
type ('a,'b) gpairR = 'b * 'a [@@deriving bin_shape]

(* alias type definitions are expanded *)
let%test_unit _ =
  ensure_shape [%bin_shape: int pair]
    ~expect:Canonical.(create (tuple [int;int]))

let%test_unit _ = ensure_all_different [
  [%bin_shape: int];
  [%bin_shape: int * int];
]

let%test_unit _ = ensure_all_same [
  [%bin_shape: int * int];
  [%bin_shape: int pair];
  [%bin_shape: int i_pair];
  [%bin_shape: int pair_i];
  [%bin_shape: (int,int) gpair];
  [%bin_shape: (int,int) gpairR];
]

let%test_unit _ = ensure_all_same [
  [%bin_shape: int * string];
  [%bin_shape: string i_pair];
  [%bin_shape: (int,string) gpair];
  [%bin_shape: (string,int) gpairR];
]

let%test_unit _ = ensure_all_same [
  [%bin_shape: string * int];
  [%bin_shape: string pair_i];
  [%bin_shape: (string,int) gpair];
  [%bin_shape: (int,string) gpairR];
]

let%test_unit _ = ensure_all_same [
  [%bin_shape: int * (int * int)];
  [%bin_shape: (int, int pair) gpair];
]

let%test_unit _ = ensure_all_same [
  [%bin_shape: (int * int) * (int * int)];
  [%bin_shape: (int pair, int pair) gpair];
  [%bin_shape: int pair pair];
]

let%test_unit _ =
  ensure_shape [%bin_shape: int pair pair]
    ~expect:Canonical.(create (tuple [tuple [int;int]; tuple [int;int]]))

(* records *)

type r1 = {i:int; s:string;} [@@deriving bin_shape]

type 'a id = 'a [@@deriving bin_shape]
type r1_copy1 = r1 [@@deriving bin_shape]
type r1_copy2 = r1 id [@@deriving bin_shape]
type r1_redef = {i:int; s:string;} [@@deriving bin_shape]

type 'a abstracted_r1 = {i:'a; s:string;} [@@deriving bin_shape]

let%test_unit _ = ensure_all_same [
  [%bin_shape: r1];
  [%bin_shape: r1_copy1];
  [%bin_shape: r1_copy2];
  [%bin_shape: r1_redef];
]

let%test_unit _ = ensure_all_same [
  [%bin_shape: r1];
  [%bin_shape: int abstracted_r1];
]

let%test_unit _ =
  ensure_shape [%bin_shape: r1]
    ~expect:Canonical.(create (
      record [
        "i", int;
        "s", string;
      ]))

type r2 = {i:int; s:int;} [@@deriving bin_shape]
type r3 = {i:int*int; s:string;} [@@deriving bin_shape]
type r4 = {ii:int; s:string;} [@@deriving bin_shape]
type r5 = {i:int; ss:string;} [@@deriving bin_shape]
type r6 = {i:int; s:string;x:int} [@@deriving bin_shape]
type r7 = {s:string; i:int} [@@deriving bin_shape]

let%test_unit _ = ensure_all_different [
  [%bin_shape: int * string];
  [%bin_shape: string * int];
  [%bin_shape: r1];
  [%bin_shape: string abstracted_r1];
  [%bin_shape: r2];
  [%bin_shape: r3];
  [%bin_shape: r4];
  [%bin_shape: r5];
  [%bin_shape: r6];
  [%bin_shape: r7];
]

module What_about_this_one = struct
  type r1 = {i:int;} [@@deriving bin_shape]
  type r2 = {i:int;} [@@deriving bin_shape]

  type r1_r1 = r1 * r1 [@@deriving bin_shape]
  type r1_r2 = r1 * r2 [@@deriving bin_shape]

  let%test_unit _ = ensure_all_same [
    [%bin_shape: r1_r1];
    [%bin_shape: r1_r2];
  ]

  (* for what it's worth, these do actually satisfy the same signature: *)
  module type S = sig
    type r1 = {i:int;}
    type r2 = {i:int;}
    type r1_r2 = r1 * r2
  end
  module S1 : S = struct
    type nonrec r1 = r1 = {i:int;}
    type nonrec r2 = r2 = {i:int;}
    type r1_r2 = r1 * r2
  end
  module S2 : S = struct
    type nonrec r1 = r1 = {i:int;}
    type r2 = r1 = {i:int;}
    type r1_r2 = r1 * r2
  end

end

module Variants = struct

  type v1 = E1 | E2 [@@deriving bin_shape]
  type v2 = E1 | E2 of int [@@deriving bin_shape]
  type v3 = XE1 | E2 of int [@@deriving bin_shape]

  let%test_unit _ = ensure_all_different [
    [%bin_shape: v1];
    [%bin_shape: v2];
    [%bin_shape: v3];
  ]

  type v2_copy1 = E1 | E2 of int [@@deriving bin_shape]
  type v2_copy2 = E1 | E2 of my_int [@@deriving bin_shape]

  let%test_unit _ = ensure_all_same [
    [%bin_shape: v2];
    [%bin_shape: v2_copy1];
    [%bin_shape: v2_copy2];
    [%bin_shape: v2_copy2 id];
  ]

  type a1 = A1 | A2 [@@deriving bin_shape]
  type a2 = A2 | A1 [@@deriving bin_shape]

  let%test_unit _ = ensure_all_different [
    [%bin_shape: a1];
    [%bin_shape: a2];
  ]

  type b1 = A of int * int [@@deriving bin_shape]
  type b2 = A of (int * int) [@@deriving bin_shape]

  let%test_unit _ = ensure_all_different [
    [%bin_shape: b1];
    [%bin_shape: b2];
  ]
end

module Empty_types = struct
  type t1 = Nothing.t [@@deriving bin_shape]
  type t2 [@@deriving bin_shape]

  let%test_unit _ = ensure_all_same [
    [%bin_shape: t1];
    [%bin_shape: t2];
  ]
end

module Recursive_types = struct

  type nat = Zero | Succ of nat [@@deriving bin_shape]
  type nat1 = Zero | Succ of nat [@@deriving bin_shape]
  type nat2 = Zero | Succ of nat2 [@@deriving bin_shape]

  let%test_unit _ = ensure_all_different [
    [%bin_shape: int];
    [%bin_shape: nat];
    [%bin_shape: nat1];
  ]

  let%test_unit _ = ensure_all_same [
    [%bin_shape: nat];
    [%bin_shape: nat2];
  ]

end

module Mutually_recursive_types = struct

  type t1 = TT of t1 | TU of u1 | TB
  and u1 = UT of t1 | UU of u1 | UB
      [@@deriving bin_shape]

  let%test_unit _ = ensure_all_different [
    [%bin_shape: t1];
    [%bin_shape: u1];
  ]

  type u2 = UT of t2 | UU of u2 | UB (* swap order: t,u *)
  and t2 = TT of t2 | TU of u2 | TB
      [@@deriving bin_shape]

  let%test_unit _ = ensure_all_same [
    [%bin_shape: t1];
    [%bin_shape: t2];
  ]

  let%test_unit _ = ensure_all_same [
    [%bin_shape: u1];
    [%bin_shape: u2];
  ]

  type t3 = TT of u3 | TU of t3 | TB (* swap types w.r.t. t1 *)
  and u3 = UT of t3 | UU of u3 | UB
      [@@deriving bin_shape]

  let%test_unit _ = ensure_all_different [
    [%bin_shape: t1];
    [%bin_shape: t3];
    [%bin_shape: u1];
    [%bin_shape: u3];
  ]

end

module Blowup1 = struct

  (* No blowup in code here, but the values constructed do increase in size exponentially *)
  type t0 = int     [@@deriving bin_shape]
  type t1 = t0 * t0 [@@deriving bin_shape]
  type t2 = t1 * t1 [@@deriving bin_shape]
  type t3 = t2 * t2 [@@deriving bin_shape]
  type t4 = t3 * t3 [@@deriving bin_shape]

end

module Blowup2 = struct

  (* Exponential blowup in code size here *)
  type t0 = int
  and t1 = t0 * t0
  and t2 = t1 * t1
  and t3 = t2 * t2
  and t4 = t3 * t3
  [@@deriving bin_shape]

end

module Blowup3 = struct

  (* Reverse the order of the decs...*)
  (* Still have exponential blowup in code size here *)
  type t4 = t3 * t3
  and t3 = t2 * t2
  and t2 = t1 * t1
  and t1 = t0 * t0
  and t0 = int
  [@@deriving bin_shape]

end

module Blowup4 = struct


  type q1 = unit [@@deriving bin_shape]
  type q2 = q1 * q1 [@@deriving bin_shape]
  type q3 = q2 * q2 [@@deriving bin_shape]
  type q4 = q3 * q3 [@@deriving bin_shape]
  type q5 = q4 * q4 [@@deriving bin_shape]
  type q6 = q5 * q5 [@@deriving bin_shape]
  type q7 = q6 * q6 [@@deriving bin_shape]
  type q8 = q7 * q7 [@@deriving bin_shape]
  type q9 = q8 * q8 [@@deriving bin_shape]
  type q10 = q9 * q9 [@@deriving bin_shape]
  type q11 = q10 * q10 [@@deriving bin_shape]
  type q12 = q11 * q11 [@@deriving bin_shape]
  type q13 = q12 * q12 [@@deriving bin_shape]
  type q14 = q13 * q13 [@@deriving bin_shape]
  type q15 = q14 * q14 [@@deriving bin_shape]

end


let%test_unit _ = ensure_all_same [
  [%bin_shape: Blowup1.t4];
  [%bin_shape: Blowup2.t4];
  [%bin_shape: Blowup3.t4];
]

module Tricky_mutual_recursion = struct

  module A = struct
    type t1 = A of t1
    and t2 = B of t1 * t2 [@@deriving bin_shape]
  end

  module B = struct
    type t0 = t1
    and t1 = A of t0
    and t2 = B of t1 * t2 [@@deriving bin_shape]
  end

  let%test_unit _ = ensure_all_same [
    [%bin_shape: A.t2];
    [%bin_shape: B.t2];
  ]

end

(* examples which make use of predefined list *)
module List_example = struct

  let%test_unit _ = ensure_all_different [
    [%bin_shape: int];
    [%bin_shape: int list];
    [%bin_shape: float list];
    [%bin_shape: int list list];
  ]

  type 'a copy_predef_list = 'a list [@@deriving bin_shape]
  type my_int_predef_list = int list [@@deriving bin_shape]

  let%test_unit _ = ensure_all_same [
    [%bin_shape: int list];
    [%bin_shape: int copy_predef_list];
    [%bin_shape: my_int_predef_list];
  ]

end

(* examples which make use of mylist *)
module My_list = struct

  type 'a mylist = Nil | Cons of 'a * 'a mylist [@@deriving bin_shape]

  let%test_unit _ = ensure_all_different [
    [%bin_shape: int list];
    [%bin_shape: int mylist];
  ]

  let%test_unit _ =
    ensure_shape [%bin_shape: int mylist]
      ~expect:Canonical.(create (apply (
        dvariant [
          "Nil", [];
          "Cons", [var 0; recurse 0 [var 0]]
        ]) [int]))

  let%test_unit _ =
    ensure_shape [%bin_shape: int mylist mylist]
      ~expect:Canonical.(create (
        let mylist =
          dvariant [
            "Nil", [];
            "Cons", [var 0; recurse 0 [var 0]]
          ] in
        apply mylist [apply mylist [int]]))

end

(* Support for predefined types... *)
let%test_unit _ = ensure_all_different [
  [%bin_shape: unit];
  [%bin_shape: bool];
  [%bin_shape: string];
  [%bin_shape: char];
  [%bin_shape: float];
  [%bin_shape: int32];
  [%bin_shape: int64];
]

let%test_unit _ =
  ensure_shape [%bin_shape: unit]
    ~expect:Canonical.(create unit)

let%test_unit _ =
  ensure_shape [%bin_shape: bool]
    ~expect:Canonical.(create bool)

let%test_unit _ =
  ensure_shape [%bin_shape: char option array]
    ~expect:Canonical.(create (array(option char)))

(* Support for predefined type constructors... *)
let%test_unit _ = ensure_all_different [
  [%bin_shape: unit];
  [%bin_shape: unit ref];
  [%bin_shape: unit option];
  [%bin_shape: unit list];
  [%bin_shape: unit array];
]

(* being lazy does not change the shape *)
let%test_unit _ = ensure_all_same [
  [%bin_shape: unit];
  [%bin_shape: unit lazy_t];
]

(* examples which make use of predefined array *)
module Array = struct

  let%test_unit _ = ensure_all_different [
    [%bin_shape: int];
    [%bin_shape: int array];
    [%bin_shape: float array];
    [%bin_shape: int array array];
  ]

  type 'a copy_predef_array = 'a array [@@deriving bin_shape]
  type my_int_predef_array = int array [@@deriving bin_shape]

  let%test_unit _ = ensure_all_same [
    [%bin_shape: int array];
    [%bin_shape: int copy_predef_array];
    [%bin_shape: my_int_predef_array];
  ]

end

module Non_regular = struct

  (* Types [a] and [b] make no `base' use of their polymorphic variable ['a] *)
  type 'a a = A of 'a a
      [@@deriving bin_shape]

  (* But we regard applications to different types as having different shapes *)
  let%test_unit _ = ensure_all_different [
    [%bin_shape: int   a];
    [%bin_shape: float a];
  ]

  (* This is because of our handling of recursion which is general enough to handle non
     regular recursion *)

  let%test_unit _ =
    ensure_shape [%bin_shape: int a]
      ~expect:Canonical.(create (apply (
        dvariant ["A", [recurse 0 [var 0]]]) [int]))

  let%test_unit _ =
    ensure_shape [%bin_shape: float a]
      ~expect:Canonical.(create (apply (
        dvariant ["A", [recurse 0 [var 0]]]) [float]))

  (* And here is an example of non-regular recursion *)
  type 'a b = B of 'a list b
      [@@deriving bin_shape]

  let%test_unit _ = ensure_all_different [
    [%bin_shape: int   b];
    [%bin_shape: float b];
  ]

  let%test_unit _ =
    ensure_shape [%bin_shape: int b]
      ~expect:Canonical.(create (apply (
        dvariant ["B", [recurse 0 [list (var 0)]]]) [int]))

end

module Polymorphism_and_recursion = struct (* example from Valentin *)

  type 'a t = A of 'a t list    [@@deriving bin_shape]
  type u1 = int t               [@@deriving bin_shape]

  type u2 = A of u2 list        [@@deriving bin_shape]

  let%test_unit _ =
    ensure_shape [%bin_shape: u1]
      ~expect:Canonical.(create (apply (
        dvariant ["A", [list (recurse 0 [var 0])]]) [int]))

  let%test_unit _ =
    ensure_shape [%bin_shape: u2]
      ~expect:Canonical.(create (apply (
        dvariant ["A", [list (recurse 0 [])]]) []))

  (* So these types are not equivalent. Which is a shame. *)
  let%test_unit _ = ensure_all_different [
    [%bin_shape: u1];
    [%bin_shape: u2];
  ]

end

module Polymorphic_variants = struct

  type v0 = [ `E1 ]                     [@@deriving bin_shape]
  type v1 = [ `E1 | `E2 ]               [@@deriving bin_shape]
  type v2 = [ `E1 | `E2 of int ]        [@@deriving bin_shape]
  type v3 = [ `XE1 | `E2 of int ]       [@@deriving bin_shape]

  let%test_unit _ = ensure_all_different [
    [%bin_shape: v0];
    [%bin_shape: v1];
    [%bin_shape: v2];
    [%bin_shape: v3];
    [%bin_shape: Variants.v1];
  ]

  type v4 = [ `E1 | `E2 ]               [@@deriving bin_shape]

  let%test_unit _ = ensure_all_same [
    [%bin_shape: v1];
    [%bin_shape: v4];
  ]

  (* Polymorphic_variants allow reordering (unlike normal variants) *)
  type v5 = [ `E2 | `E1 ]               [@@deriving bin_shape]
  let%test_unit _ = ensure_all_same [
    [%bin_shape: v1];
    [%bin_shape: v5]; (* reordered *)
  ]

  let%test_unit _ =
    ensure_shape [%bin_shape: v1]
      ~expect:Canonical.(create (
        poly_variant [
          "E1", None;
          "E2", None;
        ]))

  (* Support for [Rinherit] *)

  type v6 = [ | v1 ]                    [@@deriving bin_shape]
  type v7 = [ v0 | `E2 ]                [@@deriving bin_shape]
  type v8 = [ v1 | `E2 ]                [@@deriving bin_shape]

  let%test_unit _ =
    ensure_shape [%bin_shape: v6]
      ~expect:Canonical.(create (
        poly_variant [
          "E1", None;
          "E2", None;
        ]))

  let%test_unit _ =
    ensure_shape [%bin_shape: v7]
      ~expect:Canonical.(create (
        poly_variant [
          "E1", None;
          "E2", None;
        ]))

  let%test_unit _ = ensure_all_same [
    [%bin_shape: v1];
    [%bin_shape: v6];
    [%bin_shape: v7];
    [%bin_shape: v8];
  ]

  type q = [ `a | `b | `c ] [@@deriving bin_shape]
  type q1 = [ `a | `b ] [@@deriving bin_shape]
  type q2 = [ `b | `c ] [@@deriving bin_shape]
  type q3 = [ q1 | q2 ] [@@deriving bin_shape]

  let%test_unit _ = ensure_all_same [
    [%bin_shape: q];
    [%bin_shape: q3];
  ]

end

module Parameters_and_orders = struct

  type a1 = A of a3
  and a2 = B of a1
  and a3 = C of a2
  [@@deriving bin_shape]

  type b3 = C of b2
  and b2 = B of b1
  and b1 = A of b3
  [@@deriving bin_shape]

  type ('a, 'b) c1 = 'a * 'b [@@deriving bin_shape]
  type ('b, 'a) c2 = 'a * 'b [@@deriving bin_shape]

  type t1 = (b1, b2) c1 [@@deriving bin_shape]
  type t2 = (b2, b1) c2 [@@deriving bin_shape]

  let%test_unit _ = ensure_all_same [
    [%bin_shape: t1];
    [%bin_shape: t2];
  ]
end

module Recursive_polymorphic_variant = struct
  type t1 = [ `A | `B of t1 ] [@@deriving bin_shape]
  let%test_unit _ =
    let (_ : Shape.Digest.t) = Shape.eval_to_digest bin_shape_t1 in
    ()
end

module Inheriting_from_recursive_polymorphic_variant = struct

  let%test_unit _ =
    expect_raise (fun () ->
      let module M = struct
        type t1 = [ `A | `B of t1 ] [@@deriving bin_shape]
        type t2 = [ t1 | `C ] [@@deriving bin_shape]
      end in
      let _ = Shape.eval [%bin_shape: M.t2] in
      ())

end

module Inheriting_from_recursive_polymorphic_variant2 = struct

  let%test_unit _ =
    expect_raise (fun () ->
      let module M = struct
        type t1 = [ `A | `B of t1 ] [@@deriving bin_shape]
        type t2 = [ `Q | `P of t2 | t1] [@@deriving bin_shape]
        type t3 = [ t2 | `C ] [@@deriving bin_shape]
      end in
      let _ = Shape.eval [%bin_shape: M.t3]
      in ())

end

module Unrolling_bad_0 = struct

  let%test_unit _ =
    let module M = struct

      type t1 =
      [ `A
      | `B of t1
      ] [@@deriving bin_shape]

      type t2 =
      [ `A
      | `B of [ `A | `B of t2 ]
      ] [@@deriving bin_shape]

      let () =
        ensure_shape [%bin_shape: t1]
          ~expect:Canonical.(create (apply (
            define (poly_variant [
              "A", None;
              "B", Some (recurse 0 [])
            ])) []))

      let () =
        ensure_shape [%bin_shape: t2]
          ~expect:Canonical.(create (apply (
            define (poly_variant [
              "A", None;
              "B", Some (
                poly_variant [
                  "A", None;
                  "B", Some (recurse 0 [])
                ])])) []))

      let () = ensure_all_different [
        [%bin_shape: t1];
        [%bin_shape: t2];
      ]

    end in ()

end

let%test_unit _ =
  let module Unrolling_good_1 = struct

    type t = [ `A of t ] [@@deriving bin_shape]
    type u = [ `A of u ] [@@deriving bin_shape]

    let () = ensure_all_same [
      [%bin_shape: t];
      [%bin_shape: u];
    ]

  end in ()

let%test_unit _ =
  let module Unrolling_bad_1 = struct

    type t = [ `A of t ] [@@deriving bin_shape]
    type u = [ `A of t ] [@@deriving bin_shape] (* like good, except: [of u] -> [of t] *)

    let () = ensure_all_different [
      [%bin_shape: t];
      [%bin_shape: u];
    ]

  end in ()

module Unrolling_bad_2 = struct

  type 'a named = [`A | `B of 'a] [@@deriving bin_shape]

  type t1 = t1 named [@@deriving bin_shape]
  type t2 = t2 named named [@@deriving bin_shape]

  (* We get different shapes for t1 and t2, because we dont regard a type as equivalent to
     an unrolled version of itself. *)

  let%test_unit _ =
    ensure_shape [%bin_shape: t1]
      ~expect:Canonical.(create (apply (
        define (
          poly_variant [
            "A", None;
            "B", Some (recurse 0 [])])
      ) []))

  let%test_unit _ =
    ensure_shape [%bin_shape: t2]
      ~expect:Canonical.(create (apply (
        define (
          poly_variant [
            "A", None;
            "B", Some (
              poly_variant [
                "A", None;
                "B", Some (recurse 0 [])])])
      ) []))

  let%test_unit _ = ensure_all_different [
    [%bin_shape: t1];
    [%bin_shape: t2];
  ]

end

module Tricky1 = struct

  type 'a inner = Tight of 'a inner | Loose of 'a [@@deriving bin_shape]
  type outer = Z | S of outer inner  [@@deriving bin_shape]

  let%test_unit _ = ensure_all_different [
    [%bin_shape: int];
    [%bin_shape: outer];
  ]

end

module Tricky_mutual = struct

  type 'a inner = Tight of 'a inner | Loose of 'a
  and outer = Z | S of outer inner  [@@deriving bin_shape]

  let%test_unit _ = ensure_all_different [
    [%bin_shape: int];
    [%bin_shape: outer];
  ]

end

module Cyclic_app = struct

  type ('a, 'b) t =
  | A of ('b, 'a) t
  | B
      [@@deriving bin_shape]

  let%test_unit _ =
    ensure_shape [%bin_shape: (int, string) t]
      ~expect:Canonical.(create (apply (
        dvariant ["A", [recurse 0 [var 1; var 0]];
                 "B", []]
      ) [int; string]))

  let%test_unit _ =
    ensure_shape [%bin_shape: (string, int) t]
      ~expect:Canonical.(create (apply (
        dvariant ["A", [recurse 0 [var 1; var 0]];
                 "B", []]
      ) [string; int]))

  (* So these types are not equivalent. *)
  let%test_unit _ = ensure_all_different [
    [%bin_shape: (int, string) t];
    [%bin_shape: (string, int) t];
  ]

end

module Non_regular_recursion = struct

  type 'a mylist = Nil | Cons of 'a * 'a mylist [@@deriving bin_shape]
  type 'a mylist2 = Nil | Cons of 'a * string mylist2 [@@deriving bin_shape]
  type 'a mylist3 = Nil | Cons of 'a * float mylist3 [@@deriving bin_shape]

  let%test_unit _ = ensure_all_different [
    [%bin_shape: int mylist];
    [%bin_shape: int mylist2];
    [%bin_shape: int mylist3];
  ]

end

module Dub = struct

  type 'a dub = 'a * 'a [@@deriving bin_shape]

  type t0 = B | Knot of t0 [@@deriving bin_shape]
  type t1 = B | Knot of t1 dub [@@deriving bin_shape]
  type t2 = B | Knot of t2 dub dub [@@deriving bin_shape]
  type t3 = B | Knot of t3 dub dub dub [@@deriving bin_shape]
  type t4 = B | Knot of t4 dub dub dub dub [@@deriving bin_shape]

  let r0 = Canonical.(recurse 0 [])

  let%test_unit _ =
    ensure_shape [%bin_shape: t2]
      ~expect:Canonical.(create (apply (
        dvariant [
          "B", [];
          "Knot", [
            tuple [tuple [r0;r0]; tuple [r0;r0]]
          ]]) []))

  let%test_unit _ =
    ensure_shape [%bin_shape: t3]
      ~expect:Canonical.(create (apply (
        dvariant [
          "B", [];
          "Knot", [
            tuple [
              tuple [tuple [r0;r0]; tuple [r0;r0]];
              tuple [tuple [r0;r0]; tuple [r0;r0]];
            ]]]) []))

  (* Testing for specific digest values - fails if the digest scheme is changed *)

  let%test_unit _ =
    [%test_result: string] (eval_to_digest [%bin_shape: t3])
      ~expect:"e502ebd5fc3a340527fe4aa39b68bee5"

  let%test_unit _ =
    [%test_result: string] (eval_to_digest [%bin_shape: t4])
      ~expect:"a66734cfcaea50f37e04cd706cf9c7d0"

  (* Use [bin_digest..] function/extension *)
  let%test_unit _ =
    [%test_result: string] [%bin_digest: t4]
      ~expect:"a66734cfcaea50f37e04cd706cf9c7d0"

  module By_hand = struct

    module type Unary_with_shape = sig type 'a t [@@deriving bin_shape] end

    let add_dub (module M : Unary_with_shape) : (module Unary_with_shape) =
      (module struct
        type 'a t = 'a M.t dub [@@deriving bin_shape]
      end)

    let dubs n : (module Unary_with_shape) =
      Fn.apply_n_times ~n add_dub (module struct
        type 'a t = 'a [@@deriving bin_shape]
      end)

    let gen_t' n =
      let module Dubs = (val dubs n) in
      let module M = struct
        type t = B | Knot of t Dubs.t [@@deriving bin_shape]
        let _f () = Knot (assert false)
        let _f () = B
      end
      in
      M.bin_shape_t

    (* [gen_t n] constructs a type-expression for t<n>, which if tricky to
       express directly in Ocaml. But possible. See [gen_t'] above. *)
    let gen_t n =
      assert (n>=0);
      let open Shape in
      let name = Tid.of_string "any_name_will_do" in
      let _group = group (Location.of_string "group") [
        name, [], (
          variant [
            "B", [];
            "Knot", [
              Fn.apply_n_times ~n bin_shape_dub (rec_app name [])
            ]])]
      in
      top_app _group name []

    let%test_unit _ = ensure_all_same [
      [%bin_shape: t4];
      gen_t 4;
      gen_t' 4;
    ]

  end

  let test_eval_time n allowed =
    let exp = By_hand.gen_t n in
    let before = Time.now() in
    let _res = Shape.eval_to_digest exp in
    let after = Time.now() in
    [%test_pred: Time.Span.t] (fun x -> x < allowed) (Time.diff after before)

  let%test_unit _ = test_eval_time 15 (sec 1.)

  (* demonstate that we have no exponential blowup *)
  let%test_unit _ = test_eval_time 10000 (sec 1.)

end

(* Captures all kinds of shape *)
module Complex_type = struct
  type t0 = A | B of t0 [@@deriving bin_shape]
  let bin_shape_t0 = Shape.annotate "t0" bin_shape_t0
  type 'a t1 = Z | Q of 'a t1  [@@deriving bin_shape]
  type base_types =
    { a : unit;
      b : bool;
      c : string;
      d : char;
      f : float;
      g : int32;
      h : int64;
      i : int64 ref;
      j : int64 Lazy.t;
      k : int64 option;
      l : int64 list;
      m : int64 array;
      o : Bigstring.Stable.V1.t;
    }
  [@@deriving bin_shape]
  type 'a all_stuff =
    [ `Nullary
    | `Variant of t0
    | `Record of base_types
    | `Big_tuple of (int64 * string * unit * bool)
    | `List of int64 list
    | `Application of int64 t1
    | `Var of 'a
    ] [@@deriving bin_shape]


  let%test_unit _ =
    [%test_result: string] (eval_to_digest [%bin_shape: bool all_stuff])
      ~expect:"23cb21f19c45331a6a0227af4eeb55f5"

end

module Too_aggressive_memoization_1 = struct
  type t = A of t [@@deriving bin_shape]
  type t1 = A of t1 [@@deriving bin_shape]

  module A = struct
    type u = A of t [@@deriving bin_shape]
    type s = T of t | U of u [@@deriving bin_shape]
  end

  module B = struct
    type u = A of t [@@deriving bin_shape]
    type s = T of t1 | U of u [@@deriving bin_shape]
  end

  (* This test fails if we do memoization too aggressively. *)
  let%test_unit _ = ensure_all_same [
    [%bin_shape: A.s];
    [%bin_shape: B.s];
  ]
end

module Too_aggressive_memoization_2 = struct
  type t = A of t [@@deriving bin_shape]
  type t1 = A of t [@@deriving bin_shape]

  type
    u1 = X of v1
  and
    v1 = A of u1
  [@@deriving bin_shape]

  type u2 = X of t [@@deriving bin_shape]

  let%test_unit _ = ensure_all_different [
    [%bin_shape: u1];
    [%bin_shape: u2];
  ]

  module A = struct
    type s = T of t | U of u1 [@@deriving bin_shape]
  end

  module B = struct
    type s = T of t | U of u2 [@@deriving bin_shape]
  end

  (* This test fails if we do memoization too aggressively. *)
  let%test_unit _ = ensure_all_different [
    [%bin_shape: A.s];
    [%bin_shape: B.s];
  ]
end

module Example_poly_variants = struct

  type a = [ `qaz | `bar ] [@@deriving bin_shape]
  type b = [ `foo | a ] [@@deriving bin_shape]

  let exp = [%bin_shape: b]

  let expect : Canonical.t =
    let open Canonical in
    create (
      poly_variant [
        ("bar", None);
        ("foo", None);
        ("qaz", None);
      ])

  let%test_unit _ =
    ensure_shape exp ~expect

end

module Example_direct_recusion = struct

  type 'a mylist = Nil | Cons of 'a * 'a mylist [@@deriving bin_shape]

  let exp = [%bin_shape: int mylist]

  let expect : Canonical.t =
    let open Canonical in
    create (apply (
      dvariant [
        "Nil", [];
        "Cons", [var 0; recurse 0 [var 0]]
      ]) [int])

  let%test_unit _ =
    ensure_shape exp ~expect

end

module Example_block_of_3 = struct

  type 'a mylist = Nil | Cons of 'a * 'a mylist
  and 'a and_string = 'a * string
  and t = (int*float) mylist and_string
  [@@deriving bin_shape]

  let exp = [%bin_shape: t]

  let expect : Canonical.t =
    let open Canonical in
    let def =
      dvariant [
        "Nil", [];
        "Cons", [var 0; recurse 0 [var 0]]
      ] in
    create (tuple [apply def [tuple [int; float]]; string])

  let%test_unit _ =
    ensure_shape exp ~expect

end

module Example_inner_outer1 = struct

  (* .. within the same mut-block *)
  type 'a inner = Tight of 'a inner | Loose of 'a
  and outer = Z of outer | S of outer inner
      [@@deriving bin_shape]

  let exp = [%bin_shape: outer]

  let expect : Canonical.t =
    let open Canonical in
    let inner =
      dvariant [
        "Tight", [recurse 1 [var 0]];
        "Loose", [var 0];
      ]
    in
    let outer =
      dvariant [
        "Z", [recurse 0 []];
        "S", [apply inner [recurse 0 []]];
      ]
    in
    create (apply outer [])

  let%test_unit _ =
    ensure_shape exp ~expect

end

module Example_inner_outer2 = struct

  (* .. within  sequence of mut-blocks *)
  type 'a inner = Tight of 'a inner | Loose of 'a
      [@@deriving bin_shape]
  type outer = Z of outer | S of outer inner
      [@@deriving bin_shape]

  let exp = [%bin_shape: outer]

  let expect : Canonical.t =
    let open Canonical in
    let inner =
      dvariant [
        "Tight", [recurse 1 [var 0]];
        "Loose", [var 0];
      ]
    in
    let outer =
      dvariant [
        "Z", [recurse 0 []];
        "S", [apply inner [recurse 0 []]]
      ]
    in
    create (apply outer [])

  let%test_unit _ =
    ensure_shape exp ~expect

end

let%test_unit _ = ensure_all_same [
  [%bin_shape: Example_inner_outer1.outer];
  [%bin_shape: Example_inner_outer2.outer];
]

module Example_mut_recursion_with_extra_aliases = struct

  type t = A of t1 | B of u
  and t1 = t
  and  u = C of t1 | D of u
      [@@deriving bin_shape]

  let exp = [%bin_shape: t1]

  let expect : Canonical.t =
    let open Canonical in
    let u =
      dvariant [
        "C", [recurse 0 []];
        "D", [recurse 1 []];
      ] in
    let t =
      dvariant [
        "A", [recurse 0 []];
        "B", [apply u []];
      ] in
    create (apply t [])

  let%test_unit _ =
    ensure_shape exp ~expect

  module Tiny_variation = struct

    type t = A of t | B of u (* changed [t1] -> [t] ...*)
    and t1 = t
    and  u = C of t1 | D of u
        [@@deriving bin_shape]

    let exp_variation = [%bin_shape: t1]

    (* happily the shape is unchanged *)
    let%test_unit _ =
      ensure_shape exp_variation ~expect

  end

  let%test_unit _ = ensure_all_same [
    [%bin_shape: t];
    [%bin_shape: t1];
    [%bin_shape: Tiny_variation.t];
    [%bin_shape: Tiny_variation.t1];
  ]

end

module Tricky = struct

  type 'b u =
  | Uu of 'b u
  | Ub of 'b
      [@@deriving bin_shape]

  type 'a t =
  | Tt of 'a t
  | Ta of 'a
  | Tu of 'a u
      [@@deriving bin_shape]

  type knot =
  | Base
  | Knot of knot t
      [@@deriving bin_shape]

  let exp = [%bin_shape: knot]

  let expect : Canonical.t =
    let open Canonical in
    let u =
      dvariant [
        "Uu", [recurse 2 [var 0]];
        "Ub", [var 0];
      ] in
    let t =
      dvariant [
        "Tt", [recurse 1 [var 0]];
        "Ta", [var 0];
        "Tu", [apply u [var 0]];
      ] in
    let knot =
      dvariant [
        "Base", [];
        "Knot", [apply t [recurse 0 []]];
      ] in
    create (apply knot [])

  let%test_unit _ =
    ensure_shape exp ~expect

end

module Test_tid_clash = struct

  module Other = struct
    type 'a t =
    | Tt of 'a t
    | Ta of 'a
        [@@deriving bin_shape]
  end

  type t =
  | Base
  | Knot of t Other.t
      [@@deriving bin_shape]

  let exp = [%bin_shape: t]

  let expect : Canonical.t =
    let open Canonical in
    let other =
      dvariant [
        "Tt", [recurse 1 [var 0]];
        "Ta", [var 0];
      ] in
    let knot =
      dvariant [
        "Base", [];
        "Knot", [apply other [recurse 0 []]];
      ] in
    create (apply knot [])

  let%test_unit _ =
    ensure_shape exp ~expect

end

module Test_nested_type_application = struct

  type 'a pair = 'a * 'a
  [@@deriving bin_shape]
  type t = int pair pair
  [@@deriving bin_shape]

  let exp = [%bin_shape: int pair pair]

  let expect =
    Canonical.(create (tuple [tuple [int;int]; tuple [int;int]]))

  let%test_unit _ =
    ensure_shape exp ~expect

end

module Test_recursive_nested_type_application = struct

  type 'a mylist = Nil | Cons of 'a * 'a mylist [@@deriving bin_shape]
  type t = int mylist mylist [@@deriving bin_shape]

  let exp = [%bin_shape: int mylist mylist]

  let expect =
    let open Canonical in
    let mylist =
      dvariant [
        "Nil", [];
        "Cons", [var 0; recurse 0 [var 0]]
      ] in
    create (apply mylist [apply mylist [int]])

  let%test_unit _ =
    ensure_shape exp ~expect

end

module Examples_where_shape_exps_are_constructed_by_hand = struct

  module Example_mut_recursion = struct

    type t = A of t | B of u
    and  u = C of t | D of u
        [@@deriving bin_shape]

    let exp = [%bin_shape: t]

    let expect : Canonical.t =
      let open Canonical in
      let u =
        dvariant [
          "C", [recurse 0 []];
          "D", [recurse 1 []];
        ] in
      let t =
        dvariant [
          "A", [recurse 0 []];
          "B", [apply u []];
        ] in
      create (apply t [])

    module By_hand = struct

      let exp =
        let open Shape in
        let t = Tid.of_string "t" in
        let u = Tid.of_string "u" in
        let _group = group (Location.of_string "group") [
          t, [], (
            variant [
              "A", [rec_app t []];
              "B", [rec_app u []];
            ]);
          u, [], (
            variant [
              "C", [rec_app t []];
              "D", [rec_app u []];
            ]);
        ]
        in
        top_app _group t []

      let%test_unit _ =
        ensure_shape exp ~expect

    end

    let%test_unit _ =
      ensure_shape exp ~expect

  end

  module Example_bad = struct

    type 'a t = A of int | B of string t [@@deriving bin_shape]

    type 'a s = A of 'a | B of string s [@@deriving bin_shape]

    let%test_unit _ = ensure_all_different [
      [%bin_shape: int t];
      [%bin_shape: int s];
    ]

  end

  module Example_annotation = struct

    let exp =
      let open Shape in
      let foo = Tid.of_string "foo" in
      let my_int = Tid.of_string  "my_int" in
      let _group = group (Location.of_string "group") [
        foo, [], (
          annotate "blah" (rec_app my_int [])
        );
        my_int, [], (
          bin_shape_int
        )
      ]
      in
      top_app _group foo []

    let expect : Canonical.t =
      let open Canonical in
      create (annotate "blah" int)

    let%test_unit _ =
      ensure_shape exp ~expect

  end

  module Example_definition_order_1 = struct

    type t1 = A [@@deriving bin_shape]
    type t2 = B [@@deriving bin_shape]
    type t = t1 * t2 [@@deriving bin_shape]

    let exp = [%bin_shape: t]

    let expect : Canonical.t =
      let open Canonical in
      let t1 = variant ["A", [];] in
      let t2 = variant ["B", [];] in
      create (tuple [t1; t2])

    let%test_unit _ =
      ensure_shape exp ~expect

  end

end

module Example_definition_order_2 = struct

  type t1 = A [@@deriving bin_shape]
  type t2 = B [@@deriving bin_shape]

  type ('a, 'b) tup = ('b * 'a) [@@deriving bin_shape]

  type x1 = (t1 * t2) [@@deriving bin_shape]
  type x2 = (t2, t1) tup [@@deriving bin_shape]

  let%test_unit _ = ensure_all_same [
    [%bin_shape: x1];
    [%bin_shape: x2];
  ]

end

module Example_definition_order_3 = struct

  type t1 = A [@@deriving bin_shape]
  type t2 = B [@@deriving bin_shape]

  type x1 = [`t1 of t1 | `t2 of t2 ] [@@deriving bin_shape]
  type x2 = [`t2 of t2 | `t1 of t1 ] [@@deriving bin_shape]

  let%test_unit _ = ensure_all_same [
    [%bin_shape: x1];
    [%bin_shape: x2];
  ]

end

module Example_definition_order_4 = struct

  type t1 = A of t1 [@@deriving bin_shape]
  type t2 = B of t2 [@@deriving bin_shape]

  type ('a, 'b) tup = ('b * 'a) [@@deriving bin_shape]

  type x1 = (t1 * t2) [@@deriving bin_shape]
  type x2 = (t2, t1) tup [@@deriving bin_shape]

  let%test_unit _ = ensure_all_same [
    [%bin_shape: x1];
    [%bin_shape: x2];
  ]

end

module Annotations = struct

  module D1 = struct

    type dollars = float [@@deriving bin_io]
    let bin_shape_dollars = Shape.annotate "dollars" bin_shape_dollars

    type dollars2 = float [@@deriving bin_io]
    let bin_shape_dollars2 = Shape.annotate "dollars2" bin_shape_dollars2

    let%test_unit _ = ensure_all_different [
      [%bin_shape: float];
      [%bin_shape: dollars];
      [%bin_shape: dollars2];
    ]

    type dollars_copy = float [@@deriving bin_io]
    (* Annotations are not generative. A different type with the same annotation has the
       same shape. *)
    let bin_shape_dollars_copy = Shape.annotate "dollars" bin_shape_dollars_copy

    let%test_unit _ = ensure_all_same [
      [%bin_shape: dollars];
      [%bin_shape: dollars_copy];
    ]

  end

  module D2 = struct
    (* Annotations are different from the original un-annotated type.  And different from
       records and new base types. *)
    module Orig = struct
      type t = int [@@deriving bin_io]
    end
    module Record = struct
      type t = { qaz : int } [@@deriving bin_io]
    end
    module Annotation = struct
      type t = int [@@deriving bin_io]
      let bin_shape_t = Shape.annotate "qaz" bin_shape_t
    end
    module New_base = struct
      type t = int [@@deriving bin_io]
      let bin_shape_t = Shape.basetype "qaz" [bin_shape_t]
    end
    let%test_unit _ = ensure_all_different [
      [%bin_shape: Orig.t];
      [%bin_shape: Record.t];
      [%bin_shape: Annotation.t];
      [%bin_shape: New_base.t];
    ]
  end

end

module Annotation_Syntax = struct

  module Dollars = struct

    module Without_syntax = struct
      type t = float [@@deriving bin_shape]
      let bin_shape_t = Shape.annotate "dollars" bin_shape_t
    end

    module With_syntax = struct
      type t = float [@@deriving bin_shape ~annotate:"dollars"]
    end

    let%test_unit _ = ensure_all_same [
      [%bin_shape: Without_syntax.t];
      [%bin_shape: With_syntax.t];
    ]

    module Bin_io_with_annotated_shape = struct

      type t = float [@@deriving bin_io ~annotate:"dollars"]

      let%test_unit _ = ensure_all_same [
        [%bin_shape: Without_syntax.t];
        [%bin_shape: t];
      ]

    end

    module Bin_io_with_annotated_shape_broken = struct


      type t = float [@@deriving bin_shape ~annotate:"dollars", bin_io]

      let%test_unit _ = ensure_all_different [ (* BUG *)
        [%bin_shape: Without_syntax.t];
        [%bin_shape: t];
      ]
    end
  end

end

module Test_nonrec = struct

  module A = struct
    type t = int [@@deriving bin_shape]
  end

  module B = struct
    open A
    type nonrec t = t [@@deriving bin_shape]
  end

  let%test_unit _ = ensure_all_same [
    [%bin_shape: A.t];
    [%bin_shape: B.t];
  ]

end

module Basetype_syntax = struct

  module Kind0 = struct

    module Orig = struct
      type t = int [@@deriving bin_shape]
    end

    type shapeless = Orig.t

    module Without_syntax = struct
      type t = shapeless
      let bin_shape_t = Shape.basetype "my-base" []
    end

    let%test_unit _ = ensure_all_different [
      [%bin_shape: Orig.t];
      [%bin_shape: Without_syntax.t];
    ]

    module With_syntax = struct
      type t = shapeless [@@deriving bin_shape ~basetype:"my-base"]
    end

    let%test_unit _ = ensure_all_same [
      [%bin_shape: Without_syntax.t];
      [%bin_shape: With_syntax.t];
    ]

  end

  module Kind1 = struct

    module Orig = struct
      type 'a t = 'a list [@@deriving bin_shape]
    end

    type 'a shapeless = 'a Orig.t

    module Without_syntax = struct
      type 'a t = 'a shapeless
      let bin_shape_t a = Shape.basetype "my-base1" [a]
    end

    let%test_unit _ = ensure_all_different [
      [%bin_shape: int Orig.t];
      [%bin_shape: int Without_syntax.t];
    ]

    module With_syntax = struct
      type 'a t = 'a shapeless [@@deriving bin_shape ~basetype:"my-base1"]
    end

    let%test_unit _ = ensure_all_same [
      [%bin_shape: int Without_syntax.t];
      [%bin_shape: int With_syntax.t];
    ]

  end

end

module Inline_records = struct

  type r1 = {i:int; s:string;} [@@deriving bin_shape]
  type t = A of r1 [@@deriving bin_shape]

  type t_using_inline_record = A of {i:int; s:string;} [@@deriving bin_shape]

  let%test_unit _ = ensure_all_different [
    [%bin_shape: t_using_inline_record];
    [%bin_shape: r1];
    ]

  let%test_unit _ = ensure_all_same [
    [%bin_shape: t];
    [%bin_shape: t_using_inline_record];
  ]

end

module Wildcard : sig
  type _ abstract [@@deriving bin_shape]

  type _ concrete [@@deriving bin_shape]
end = struct
  type _ abstract [@@deriving bin_shape]

  type 'a concrete = 'a list [@@deriving bin_shape]

  let%test_unit _ = ensure_all_same [
    [%bin_shape: int abstract];
    [%bin_shape: string abstract];
  ]

  let%test_unit _ = ensure_all_different [
    [%bin_shape: int concrete];
    [%bin_shape: string concrete];
  ]
end
