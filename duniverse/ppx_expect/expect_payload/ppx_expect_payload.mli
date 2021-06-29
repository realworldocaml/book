open Ppxlib
open Expect_test_common

(** Translate a compile time location to a runtime location *)
val transl_loc : Location.t -> File.Location.t

type data = Location.t * string * string option (* string loc, string, tag *)

type kind =
  | Normal
  | Exact
  | Unreachable
  | Output

val make : kind:kind -> data option -> extension_id_loc:Location.t -> Expectation.Raw.t
val pattern : unit -> (Parsetree.payload, data option -> 'a, 'a) Ast_pattern.t
