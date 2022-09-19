(** This file covers a lot of cases for [@@deriving], for both interface and
    implementation. They are also exported for validation. *)

type abstract_a [@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val abstract_a_sexp_grammar : abstract_a Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type abstract_b [@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val abstract_b_sexp_grammar : abstract_b Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type integer = int [@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val integer_sexp_grammar : integer Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type tuple = int * string [@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val tuple_sexp_grammar : tuple Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type pos =
  { x : float
  ; y : float
  }
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val pos_sexp_grammar : pos Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type 'a unary = 'a list [@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val unary_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a unary Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type enum =
  | One
  | Two
  | Three
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val enum_sexp_grammar : enum Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type ('a, 'b) which =
  | This of 'a
  | That of 'b
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val which_sexp_grammar
    :  'a Sexplib0.Sexp_grammar.t
    -> 'b Sexplib0.Sexp_grammar.t
    -> ('a, 'b) which Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type 'a optional =
  | No
  | Yes of 'a
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val optional_sexp_grammar
    :  'a Sexplib0.Sexp_grammar.t
    -> 'a optional Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type empty = | [@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val empty_sexp_grammar : empty Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type _ phantom = int [@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val phantom_sexp_grammar
    :  'a__003_ Sexplib0.Sexp_grammar.t
    -> 'a__003_ phantom Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type color =
  [ `Red
  | `Blue
  ]
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val color_sexp_grammar : color Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type adjective =
  [ color
  | `Fast
  | `Slow
  | `Count of int
  ]
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val adjective_sexp_grammar : adjective Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type 'a tree =
  { data : 'a
  ; children : 'a tree list
  }
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val tree_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a tree Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type alpha = int

and beta =
  { alpha : alpha
  ; betas : beta list
  }

and gamma = beta list [@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val alpha_sexp_grammar : alpha Sexplib0.Sexp_grammar.t
  val beta_sexp_grammar : beta Sexplib0.Sexp_grammar.t
  val gamma_sexp_grammar : gamma Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type record_attributes =
  { a : int
  ; b : bool
  ; c : float option
  ; d : string list
  ; e : bytes array
  ; f : Ppx_sexp_conv_lib.Sexp.t
  }
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val record_attributes_sexp_grammar : record_attributes Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type variant_attributes =
  | A
  | B of int list
  | C of
      { a : int
      ; b : bool
      ; c : float option
      ; d : string list
      ; e : bytes array
      ; f : Ppx_sexp_conv_lib.Sexp.t
      }
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val variant_attributes_sexp_grammar : variant_attributes Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type polymorphic_variant_attributes =
  [ `A
  | `B of int list
  ]
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val polymorphic_variant_attributes_sexp_grammar
    : polymorphic_variant_attributes Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]

type opaque =
  { x : string
  ; y : int -> int
  }
[@@deriving sexp] [@@deriving_inline sexp_grammar]

include sig
  [@@@ocaml.warning "-32"]

  val opaque_sexp_grammar : opaque Sexplib0.Sexp_grammar.t
end
[@@ocaml.doc "@inline"]

[@@@end]
