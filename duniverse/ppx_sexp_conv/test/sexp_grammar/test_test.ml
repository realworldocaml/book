open! Base

module _ = struct
  type t = int [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()
  let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) = int_sexp_grammar
  let _ = t_sexp_grammar

  [@@@deriving.end]
end

module _ = struct
  type 'a t = T of 'a

  and 'a u = U of 'a t option [@@deriving_inline sexp_grammar]

  let _ = fun (_ : 'a t) -> ()
  let _ = fun (_ : 'a u) -> ()

  let (t_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t) =
    fun _'a_sexp_grammar ->
    { untyped =
        Variant
          { case_sensitivity = Case_sensitive_except_first_character
          ; clauses =
              [ No_tag
                  { name = "T"
                  ; clause_kind =
                      List_clause { args = Cons (_'a_sexp_grammar.untyped, Empty) }
                  }
              ]
          }
    }
  ;;

  let _ = t_sexp_grammar

  let (u_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a u Sexplib0.Sexp_grammar.t) =
    fun _'a_sexp_grammar ->
    { untyped =
        Variant
          { case_sensitivity = Case_sensitive_except_first_character
          ; clauses =
              [ No_tag
                  { name = "U"
                  ; clause_kind =
                      List_clause
                        { args =
                            Cons
                              ( (option_sexp_grammar (t_sexp_grammar _'a_sexp_grammar))
                                .untyped
                              , Empty )
                        }
                  }
              ]
          }
    }
  ;;

  let _ = u_sexp_grammar

  [@@@deriving.end]

  (* Avoid unused constructor warnings. *)
  let _ = T ()
  let _ = U None
end

module _ = struct
  type ('a, 'b) t = 'a -> 'b [@@deriving_inline sexp_grammar]

  let _ = fun (_ : ('a, 'b) t) -> ()

  let (t_sexp_grammar :
         'a Sexplib0.Sexp_grammar.t
       -> 'b Sexplib0.Sexp_grammar.t
       -> ('a, 'b) t Sexplib0.Sexp_grammar.t)
    =
    fun _'a_sexp_grammar _'b_sexp_grammar -> Sexplib0.Sexp_conv.fun_sexp_grammar
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end
