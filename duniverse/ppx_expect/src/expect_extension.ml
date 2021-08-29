open Ppxlib
open Extension

(* An expect declaration resembles [%%expect {tag|...|tag}]. We allow arbitrary tags so
   that users can escape their strings properly if need be. *)
let expect =
  Expert.declare
    "expect"
    Context.expression
    (Ppx_expect_payload.pattern ())
    (Ppx_expect_payload.make ~kind:Normal)
;;

(* An expect extension without pretty formatting *)
let expect_exact =
  Expert.declare
    "expect_exact"
    Context.expression
    (Ppx_expect_payload.pattern ())
    (Ppx_expect_payload.make ~kind:Exact)
;;

let expect_output =
  Expert.declare
    "@expect.output"
    Context.expression
    (Ppx_expect_payload.pattern ())
    (Ppx_expect_payload.make ~kind:Output)
;;

let expect_unreachable =
  Expert.declare
    "@expect.unreachable"
    Context.expression
    (Ppx_expect_payload.pattern ())
    (Ppx_expect_payload.make ~kind:Unreachable)
;;

let expectations = [ expect; expect_exact; expect_output; expect_unreachable ]

let match_expectation e =
  match e.pexp_desc with
  | Pexp_extension extension ->
    (match Expert.convert expectations ~loc:e.pexp_loc extension with
     | None -> None
     | Some f -> Some (f ~extension_id_loc:(fst extension).loc))
  | _ -> None
;;
