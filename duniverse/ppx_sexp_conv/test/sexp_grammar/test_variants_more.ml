open Base

[@@@warning "-37"]

module _ = struct
  type t = A of [ `A of int ] [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) =
    { untyped =
        Lazy
          (lazy
            (Variant
               { case_sensitivity = Case_sensitive_except_first_character
               ; clauses =
                   [ No_tag
                       { name = "A"
                       ; clause_kind =
                           List_clause
                             { args =
                                 Cons
                                   ( Variant
                                       { case_sensitivity = Case_sensitive
                                       ; clauses =
                                           [ No_tag
                                               { name = "A"
                                               ; clause_kind =
                                                   List_clause
                                                     { args =
                                                         Cons
                                                           ( int_sexp_grammar.untyped
                                                           , Empty )
                                                     }
                                               }
                                           ]
                                       }
                                   , Empty )
                             }
                       }
                   ]
               }))
    }
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end

module _ = struct
  type t = { a : [ `A of int ] } [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) =
    { untyped =
        Lazy
          (lazy
            (List
               (Fields
                  { allow_extra_fields = false
                  ; fields =
                      [ No_tag
                          { name = "a"
                          ; required = true
                          ; args =
                              Cons
                                ( Variant
                                    { case_sensitivity = Case_sensitive
                                    ; clauses =
                                        [ No_tag
                                            { name = "A"
                                            ; clause_kind =
                                                List_clause
                                                  { args =
                                                      Cons
                                                        (int_sexp_grammar.untyped, Empty)
                                                  }
                                            }
                                        ]
                                    }
                                , Empty )
                          }
                      ]
                  })))
    }
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end
