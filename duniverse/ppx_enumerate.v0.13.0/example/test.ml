type t = A | B [@@deriving enumerate]
let%test _ = all = [A; B]

type s = C | D [@@deriving enumerate]
let%test _ = all_of_s = [C; D]

type u = E | F of s [@@deriving enumerate]
let%test _ = all_of_u = [E; F C; F D]

module V = struct
  type v = G of t | H of u [@@deriving enumerate]
  let%test _ = all_of_v = [G A; G B; H E; H (F C); H (F D)]
end

type w = I of V.v [@@deriving enumerate]
let%test _ = all_of_w = [I (V.G A); I (V.G B); I (V.H E); I (V.H (F C)); I (V.H (F D))]

type x = [`A | `B of t] [@@deriving enumerate]
let%test _ = all_of_x = [`A; `B A; `B B]

(* variant with multiple arguments are not special *)
type xx = [ `A of s * s ] [@@deriving enumerate]
let%test _ = all_of_xx = [`A (C, C); `A (D, C); `A (C, D); `A (D, D)]

type variant_inclusion = [ x | `C | x ] [@@deriving enumerate]
let%test _ = all_of_variant_inclusion =
       (all_of_x :> variant_inclusion list)
       @ [ `C ]
       @ (all_of_x :> variant_inclusion list)

type y = J of t * s [@@deriving enumerate]
let%test _ = all_of_y = [J (A,C); J (B,C); J (A,D); J (B,D)]

type z = t * s [@@deriving enumerate]
let%test _ = all_of_z = [(A,C); (B,C); (A,D); (B,D)]

type a = {foo : t; bar : s} [@@deriving enumerate]
let%test _ = all_of_a = [{foo = A; bar = C}; {foo = B; bar = C};
                 {foo = A; bar = D}; {foo = B; bar = D}]

type b = K of t * t * s [@@deriving enumerate]
let%test _ = all_of_b = [K (A,A,C); K (B,A,C); K (A,B,C); K (B,B,C);
                 K (A,A,D); K (B,A,D); K (A,B,D); K (B,B,D);
                ]

type c = t * t * s [@@deriving enumerate]
let%test _ = all_of_c = [(A,A,C); (B,A,C); (A,B,C); (B,B,C);
                 (A,A,D); (B,A,D); (A,B,D); (B,B,D);
                ]

type d = { a : t; b : t; c : s} [@@deriving enumerate]
let%test _ = all_of_d = [{a=A;b=A;c=C}; {a=B;b=A;c=C}; {a=A;b=B;c=C}; {a=B;b=B;c=C};
                 {a=A;b=A;c=D}; {a=B;b=A;c=D}; {a=A;b=B;c=D}; {a=B;b=B;c=D};
                ]
type e = { foo : t } [@@deriving enumerate]
module M = struct
  type nonrec e = { bar : e } [@@deriving enumerate]
end
let%test _ = M.all_of_e = [{M.bar = {foo = A}}; {M.bar = {foo = B}}]

type f = L of [`A | `B] [@@deriving enumerate]
let%test _ = all_of_f = [L `A; L `B]

type g = f [@@deriving enumerate]
let%test _ = all_of_g = all_of_f

type h = M | N
type i = h = M | N [@@deriving enumerate]
let%test _ = all_of_i = [M; N]

type 'a j = 'a [@@deriving enumerate]
type k = i j [@@deriving enumerate]
let%test _ = all_of_k = [M; N]

let%test _ = [%all:[`A of [`B | `C] | `D of [`E of [`F]]]] =
       [`A `B; `A `C; `D (`E `F)]

type l = { baz : bool; quux : unit } [@@deriving enumerate]
let%test _ = all_of_l = [{baz = false; quux = ()}; {baz = true; quux = ()}]

type o = [`A] option [@@deriving enumerate]
let%test _ = all_of_o = [None; Some `A]

(* Check that enumerations are only computed once *)
type 'a count = 'a
let number_of_computations = ref 0
let all_of_count all_of_a =
  incr number_of_computations;
  all_of_a
type p = { baz : bool count; quux : unit count } [@@deriving enumerate]
let%test _ = !number_of_computations = 2

let () = number_of_computations := 0
type p_nested = [ `A of [ `B of unit count * unit count ] * [ `C of unit count * unit count ] ]
[@@deriving enumerate]
let%test _ = !number_of_computations = 4

(* checking the lack of unused value warning *)
type 'a phantom_variable = unit [@@deriving enumerate]
type empty
let%test _ = all_of_phantom_variable ([] : empty list) = [()]

(* check that the coercions happen correctly when nested *)
type q = [x | `C] option option [@@deriving enumerate]
let%test _ = all_of_q = [None; Some None; Some (Some `A); Some (Some (`B A));
                   Some (Some (`B B)); Some (Some `C)]

type 'tt tt = [`G of 'tt | x] [@@deriving enumerate]
let%test _ = all_of_tt [()] = [`G (); `A; `B A; `B B]

type ir = A of { foo : i } | B [@@deriving enumerate]
let%test _ = all_of_ir = [A { foo = M}; A { foo = N }; B]

(* Tricky case where the scoping of type variable prevents generalization. If the
   constraints looked like (... :> [ a tt | `F ]) (instead of the same thing with s/a/'a/)
   where there is a fun (type a) somewhere in scope it would work, but it is simpler to
   remove variables than replace them by local types consistently. *)
type 'a nested_include_with_variable = [ 'a tt | `F ] option [@@deriving enumerate]

type +'a variance = 'a [@@deriving enumerate]

module Check_sigs = struct
  module type S1 = sig
    type t = A | B [@@deriving enumerate]
  end

  module type S2 = sig
    type t = A | B
    val all : t list
  end

  let _ = fun (module M : S1) ->
    let module M : S2 = M in
    let module M : S1 = M in
    ()
end

module Check_sigs_with_params_and_variance = struct
  module type S1 = sig
    type (+'a, 'b) t = A of 'a | B of 'b [@@deriving enumerate]
  end

  module type S2 = sig
    type ('a, +'b) t = A of 'a | B of 'b
    val all : 'a list -> 'b list -> ('a, 'b) t list
  end

  let _ = fun (module M : S1) ->
    let module M : S2 = M in
    let module M : S1 = M in
    ()
end

(* if you remove the "~no_exhaustiveness_check" flag to enumerate, the compilation time
   will noticeably spike. *)
type big_record = {
   field1: t;
   field2: t;
   field3: t;
   field4: t;
   field5: s;
   field6: s;
   field7: t;
   field8: t;
   field9: t;
   fielda: t;
   fieldb: t;
   fieldc: t;
   fieldd: t;
   fielde: t;
   fieldf: t;
   fieldg: u;
   fieldh: t;
   fieldi: t;
   fieldj: t;
   fieldk: t;
   fieldl: t;
   (* (* just keep adding fields to make things worse. *)
   fieldm: t;
   fieldn: t;
   fieldo: s;
   fieldp: t;
   fieldq: s;
   fieldr: s;
   fields: u;
   *)
 } [@@deriving enumerate ~no_exhaustiveness_check]

module Wildcard : sig
  type _ transparent = A | B of bool [@@deriving enumerate]
  type _ opaque [@@deriving enumerate]
end = struct
  type _ transparent = A | B of bool [@@deriving enumerate]
  let%test _ = all_of_transparent all_of_x = [A; B false; B true]

  type 'a opaque = 'a option [@@deriving enumerate]
  let%test _ = all_of_opaque all_of_x = [None; Some `A; Some (`B A); Some (`B B)]
end
