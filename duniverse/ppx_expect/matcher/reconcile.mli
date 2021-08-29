(** Determine whether a test's output matches its expected output. *)

open! Base
open Import
open Expect_test_common

module Result : sig
  type 'a t =
    | Match
    | Correction of 'a
  [@@deriving_inline compare, sexp_of]

  include sig
    [@@@ocaml.warning "-32"]

    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val sexp_of_t : ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t
  end
  [@@ocaml.doc "@inline"]

  [@@@end]

  val map : 'a t -> f:('a -> 'b) -> 'b t
end

val expectation_body
  :  expect:Fmt.t Cst.t Expectation.Body.t
  -> actual:string
  -> default_indent:int
  -> pad_single_line:bool
  -> allow_output_patterns:bool
  -> Fmt.t Cst.t Expectation.Body.t Result.t

(**/**)

(*_ See the Jane Street Style Guide for an explanation of [Private] submodules:

  https://opensource.janestreet.com/standards/#private-submodules *)
module Private : sig
  val line_matches : expect:Fmt.t -> actual:string -> bool

  val reconcile_line
    :  expect:Fmt.t
    -> actual:string
    -> allow_output_patterns:bool
    -> Fmt.t Cst.Line.t Result.t
end
