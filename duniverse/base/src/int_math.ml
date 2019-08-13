open! Import

let invalid_argf = Printf.invalid_argf

let negative_exponent () =
  Printf.invalid_argf "exponent can not be negative" ()

let overflow () =
  Printf.invalid_argf "integer overflow in pow" ()

(* To implement [int64_pow], we use C code rather than OCaml to eliminate allocation. *)
external int_math_int_pow   : int   -> int   -> int   = "Base_int_math_int_pow_stub" [@@noalloc]
external int_math_int64_pow : int64 -> int64 -> int64 = "Base_int_math_int64_pow_stub"

let int_pow base exponent =
  if exponent < 0 then negative_exponent ();

  if abs(base) > 1 &&
     (exponent > 63 ||
      abs(base) > Pow_overflow_bounds.int_positive_overflow_bounds.(exponent))
  then overflow ();

  int_math_int_pow base exponent
;;

module Int64_with_comparisons = struct
  include Caml.Int64
  external ( <  ) : int64 -> int64 -> bool = "%lessthan"
  external ( >  ) : int64 -> int64 -> bool = "%greaterthan"
  external ( >= ) : int64 -> int64 -> bool = "%greaterequal"
end

(* we don't do [abs] in int64 case to avoid allocation *)
let int64_pow base exponent =
  let open Int64_with_comparisons in
  if exponent < 0L then negative_exponent ();

  if (base > 1L || base < (-1L)) &&
     (exponent > 63L ||
      (base >= 0L &&
       base > Pow_overflow_bounds.int64_positive_overflow_bounds.(to_int exponent))
      ||
      (base < 0L &&
       base < Pow_overflow_bounds.int64_negative_overflow_bounds.(to_int exponent)))
  then overflow ();

  int_math_int64_pow base exponent
;;


let int63_pow_on_int64 base exponent =
  let open Int64_with_comparisons in
  if exponent < 0L then negative_exponent ();

  if abs(base) > 1L &&
     (exponent > 63L ||
      abs(base) > Pow_overflow_bounds.int63_on_int64_positive_overflow_bounds.(to_int exponent))
  then overflow ();

  int_math_int64_pow base exponent
;;

module type T = sig
  type t
  include Floatable.S  with type t := t
  include Stringable.S with type t := t

  val ( + ) : t -> t -> t
  val ( - ) : t -> t -> t
  val ( * ) : t -> t -> t
  val ( / ) : t -> t -> t
  val ( ~- ) : t -> t
  include Comparisons.Infix with type t := t

  val abs    : t -> t
  val neg    : t -> t
  val zero   : t
  val of_int_exn : int -> t
  val rem : t -> t -> t
end

module Make (X : T) = struct
  open X

  let ( % ) x y =
    if y <= zero then
      invalid_argf
        "%s %% %s in core_int.ml: modulus should be positive"
        (to_string x) (to_string y) ();
    let rval = X.rem x y in
    if rval < zero
    then rval + y
    else rval
  ;;

  let one = of_int_exn 1
  ;;

  let ( /% ) x y =
    if y <= zero then
      invalid_argf
        "%s /%% %s in core_int.ml: divisor should be positive"
        (to_string x) (to_string y) ();
    if x < zero
    then (x + one) / y - one
    else x / y
  ;;

  (** float division of integers *)
  let (//) x y = to_float x /. to_float y
  ;;

  let round_down i ~to_multiple_of:modulus = i - (i % modulus)
  ;;

  let round_up i ~to_multiple_of:modulus =
    let remainder = i % modulus in
    if remainder = zero
    then i
    else i + modulus - remainder
  ;;

  let round_towards_zero i ~to_multiple_of =
    if i = zero then zero else
    if i > zero
    then round_down i ~to_multiple_of
    else round_up   i ~to_multiple_of
  ;;

  let round_nearest i ~to_multiple_of:modulus =
    let remainder = i % modulus in
    if remainder * of_int_exn 2 < modulus
    then i - remainder
    else i - remainder + modulus
  ;;

  let round ?(dir=`Nearest) i ~to_multiple_of =
    match dir with
    | `Nearest -> round_nearest      i ~to_multiple_of
    | `Down    -> round_down         i ~to_multiple_of
    | `Up      -> round_up           i ~to_multiple_of
    | `Zero    -> round_towards_zero i ~to_multiple_of
  ;;
end
