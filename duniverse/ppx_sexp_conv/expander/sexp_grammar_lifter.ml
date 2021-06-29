open! Base
open! Ppxlib
open! Ast_builder.Default

module Atom = struct
  type t = Sexp.Private.Raw_grammar.Atom.t =
    | String
    | Bool
    | Char
    | Float
    | Int
    | This of
        { ignore_capitalization : bool
        ; string                : string
        }
  [@@deriving traverse_lift]

  let lifter ~loc =
    object
      inherit [expression] lift

      inherit Ppxlib_metaquot_lifters.expression_lifters loc
    end
  ;;
end

type atom      = Atom.t
type var_name  = Sexp.Private.Raw_grammar.var_name
type type_name = Sexp.Private.Raw_grammar.type_name

let lift_string ~loc s = pexp_constant ~loc (Pconst_string (s, loc, None))
let lift_var_name      = lift_string
let lift_type_name     = lift_string

type 't type_ = 't Sexp.Private.Raw_grammar.type_ =
  | Any
  | Apply         of 't type_ * 't type_ list
  | Atom          of atom
  | Explicit_bind of var_name list * 't type_
  | Explicit_var  of int
  | Grammar       of 't
  | Implicit_var  of int
  | List          of 't sequence_type
  | Option        of 't type_
  | Record        of 't record_type
  | Recursive     of type_name
  | Union         of 't type_ list
  | Variant       of 't variant_type

and 't sequence_type = 't component list

and 't component = 't Sexp.Private.Raw_grammar.component =
  | One      of 't type_
  | Optional of 't type_
  | Many     of 't type_
  | Fields   of 't record_type

and 't variant_type = 't Sexp.Private.Raw_grammar.variant_type =
  { ignore_capitalization : bool
  ; alts                  : (label * 't sequence_type) list
  }

and 't record_type = 't Sexp.Private.Raw_grammar.record_type =
  { allow_extra_fields : bool
  ; fields             : (label * 't field) list
  }

and 't field = 't Sexp.Private.Raw_grammar.field =
  { optional : bool
  ; args     : 't sequence_type
  }

(* [traverse_lift] generates references to a type named [t] even if it doesn't exist. So
   make sure it exists. *)
and t = unit [@@deriving traverse_lift, traverse_map]

let lifter ~loc =
  let atom_lifter = Atom.lifter ~loc in
  object
    inherit [expression] lift

    inherit Ppxlib_metaquot_lifters.expression_lifters loc

    method atom atom = atom_lifter#t  atom

    method label     = lift_string    ~loc

    method var_name  = lift_var_name  ~loc

    method type_name = lift_type_name ~loc
  end
;;
