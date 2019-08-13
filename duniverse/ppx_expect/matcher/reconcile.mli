(** Determine whether a test's output matches its expected output. *)

open Expect_test_common.Std

module Result : sig
  type 'a t =
    | Match
    | Correction of 'a

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

val expectation_body
  :  expect                : Fmt.t Cst.t Expectation.Body.t
  -> actual                : string
  -> default_indent        : int
  -> pad_single_line       : bool
  -> allow_output_patterns : bool
  -> Fmt.t Cst.t Expectation.Body.t Result.t
