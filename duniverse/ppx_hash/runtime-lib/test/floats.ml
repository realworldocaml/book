open Core

let ( --> ) a b = (not a) || b

let check_safety ~hash ~compare ~sexp_of_t (a, b) =
  let same_as_determined_by_compare = 0 = compare a b in
  let hash_a = hash a in
  let hash_b = hash b in
  let same_hash = 0 = Int.compare hash_a hash_b in
  let safe = same_as_determined_by_compare --> same_hash in
  if not safe
  then
    failwiths
      ~here:[%here]
      "safety violation: equal values hash differently"
      ((a, hash_a), (b, hash_b))
      [%sexp_of: (t * Int.t) * (t * Int.t)]
;;

module Labelled_float = struct
  type t = string * float

  let hash (_, f) = [%hash: float] f
  let compare (_, f1) (_, f2) = compare_float f1 f2
  let sexp_of_t (s, f) = sexp_of_string (Printf.sprintf "(%s)%f" s f)
end

let f int64 float =
  let module Int64 = struct
    include Int64

    let sexp_of_t t = Sexp.Atom (sprintf "%Lx" t)
  end
  in
  [%test_result: Int64.t] ~expect:int64 (Int64.bits_of_float float);
  float
;;

(* the int64 values of various nan expressions are somehow architecture-specific so
   instead of writing down the expression we hard-code their exact representations *)
let f_nan int64 =
  let float = Int64.float_of_bits int64 in
  assert (Float.is_nan float);
  f int64 float
;;

(* hex values to make sure the floats we are testing are actually different *)
let labelled () =
  [ "0.", f 0x0000000000000000L 0.
  ; "1.", f 0x3ff0000000000000L 1.
  ; "-0.", f 0x8000000000000000L (-0.)
  ; "-1.", f 0xbff0000000000000L (-1.)
  ; "Float.nan", f_nan 0x7ff8000000000001L
  ; "Float.infinity", f 0x7ff0000000000000L Float.infinity
  ; "-.Float.nan", f_nan 0xfff8000000000001L
  ; "-.Float.infinity", f 0xfff0000000000000L (-.Float.infinity)
  ; "0./.0.", f 0xfff8000000000000L (0. /. 0.)
    (* nan with a different representation *)
  ]
;;

let%test_unit "float safety test" =
  let open Labelled_float in
  List.iter
    (List.cartesian_product (labelled ()) (labelled ()))
    ~f:(check_safety ~hash ~compare ~sexp_of_t)
;;
