open! Base

type t = T : ('a[@sexp.opaque]) -> t [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : t) -> ()

let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) =
  { untyped =
      Lazy
        (lazy
          (Variant
             { case_sensitivity = Case_sensitive_except_first_character
             ; clauses =
                 [ No_tag
                     { name = "T"
                     ; clause_kind =
                         List_clause
                           { args =
                               Cons (Sexplib0.Sexp_conv.opaque_sexp_grammar.untyped, Empty)
                           }
                     }
                 ]
             }))
  }
;;

let _ = t_sexp_grammar

[@@@end]

type nullary = Nullary : nullary [@@deriving sexp] [@@deriving_inline sexp_grammar]

let _ = fun (_ : nullary) -> ()

let (nullary_sexp_grammar : nullary Sexplib0.Sexp_grammar.t) =
  { untyped =
      Variant
        { case_sensitivity = Case_sensitive_except_first_character
        ; clauses = [ No_tag { name = "Nullary"; clause_kind = Atom_clause } ]
        }
  }
;;

let _ = nullary_sexp_grammar

[@@@end]

(* We can't derive [of_sexp], but we can derive a sensible grammar for this type. *)
type _ grammar_only = Grammar_only : int -> string grammar_only
[@@warning "-37"] [@@deriving_inline sexp_grammar]

let _ = fun (_ : _ grammar_only) -> ()

let (grammar_only_sexp_grammar :
       'a__016_ Sexplib0.Sexp_grammar.t -> 'a__016_ grammar_only Sexplib0.Sexp_grammar.t)
  =
  fun _'a__016__sexp_grammar ->
  { untyped =
      Variant
        { case_sensitivity = Case_sensitive_except_first_character
        ; clauses =
            [ No_tag
                { name = "Grammar_only"
                ; clause_kind =
                    List_clause { args = Cons (int_sexp_grammar.untyped, Empty) }
                }
            ]
        }
  }
;;

let _ = grammar_only_sexp_grammar

[@@@end]
