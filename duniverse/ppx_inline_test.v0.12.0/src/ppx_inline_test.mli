open Ppxlib

type maybe_drop =
  | Keep
  | Drop_with_deadcode
  | Drop

(** How to expand tests if no "-inline-test-drop*" command line flag is passed. *)
val set_default_maybe_drop : maybe_drop -> unit

(** To be called on test extension points that use the ppx_inline_test runtime. Checks
    that tests are allowed with the given ppx command line, and that the tags are
    defined. *)
val validate_extension_point_exn
  : name_of_ppx_rewriter:string -> loc:location -> tags:string list -> unit

val maybe_drop : Location.t -> Parsetree.expression -> Parsetree.structure

(**/**)

val opt_name_and_expr
  :  (Parsetree.expression, 'a, 'b) Ast_pattern.t
  -> (Parsetree.payload, name:string option -> tags:string list -> 'a, 'b)  Ast_pattern.t
val tags : (Parsetree.pattern, string list) Attribute.t
