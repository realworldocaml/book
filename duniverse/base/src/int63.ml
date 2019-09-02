open! Import

let raise_s = Error.raise_s
module Repr = Int63_emul.Repr

include Int63_backend

module Overflow_exn = struct
  let ( + ) t u =
    let sum = t + u in
    if bit_or (bit_xor t u) (bit_xor t (bit_not sum)) < zero
    then sum
    else raise_s (Sexp.message "( + ) overflow"
                    [ "t"  , sexp_of_t t
                    ; "u"  , sexp_of_t u
                    ; "sum", sexp_of_t sum
                    ])
  ;;

  let ( - ) t u =
    let diff = t - u in
    let pos_diff = t > u in
    if t <> u && Bool.(<>) pos_diff (is_positive diff) then
      raise_s (Sexp.message "( - ) overflow"
                 [ "t"   , sexp_of_t t
                 ; "u"   , sexp_of_t u
                 ; "diff", sexp_of_t diff
                 ])
    else diff
  ;;

  let abs t = if t = min_value then failwith "abs overflow" else abs t
  let neg t = if t = min_value then failwith "neg overflow" else neg t
end

let () = assert (Int.(=) num_bits 63)

let random_of_int ?(state = Random.State.default) bound =
  of_int (Random.State.int state (to_int_exn bound))

let random_of_int64 ?(state = Random.State.default) bound =
  of_int64_exn (Random.State.int64 state (to_int64 bound))

let random =
  match Word_size.word_size with
  | W64 -> random_of_int
  | W32 -> random_of_int64

let random_incl_of_int ?(state = Random.State.default) lo hi =
  of_int (Random.State.int_incl state (to_int_exn lo) (to_int_exn hi))

let random_incl_of_int64 ?(state = Random.State.default) lo hi =
  of_int64_exn (Random.State.int64_incl state (to_int64 lo) (to_int64 hi))

let random_incl =
  match Word_size.word_size with
  | W64 -> random_incl_of_int
  | W32 -> random_incl_of_int64

let floor_log2 t =
  match Word_size.word_size with
  | W64 -> t |> to_int_exn |> Int.floor_log2
  | W32 ->
    if t <= zero
    then raise_s (Sexp.message "[Int.floor_log2] got invalid input"
                    ["", sexp_of_t t]);
    let floor_log2 = ref (Int.( - ) num_bits 2) in
    while equal zero (bit_and t (shift_left one !floor_log2)) do
      floor_log2 := Int.( - ) !floor_log2 1
    done;
    !floor_log2
;;

module Private = struct
  module Repr = Repr
  let repr = repr
end
