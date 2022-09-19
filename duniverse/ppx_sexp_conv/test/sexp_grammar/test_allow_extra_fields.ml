open! Base

module _ = struct
  type t = { a : int } [@@sexp.allow_extra_fields] [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) =
    { untyped =
        Lazy
          (lazy
            (List
               (Fields
                  { allow_extra_fields = true
                  ; fields =
                      [ No_tag
                          { name = "a"
                          ; required = true
                          ; args = Cons (int_sexp_grammar.untyped, Empty)
                          }
                      ]
                  })))
    }
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end

module _ = struct
  type t = { a : int } [@@deriving_inline sexp_grammar]

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
                          ; args = Cons (int_sexp_grammar.untyped, Empty)
                          }
                      ]
                  })))
    }
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end

module _ = struct
  type t =
    | Allow_extra_fields of { foo : int } [@sexp.allow_extra_fields]
    | Forbid_extra_fields of { bar : int }
  [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()

  let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) =
    { untyped =
        Lazy
          (lazy
            (Variant
               { case_sensitivity = Case_sensitive_except_first_character
               ; clauses =
                   [ No_tag
                       { name = "Allow_extra_fields"
                       ; clause_kind =
                           List_clause
                             { args =
                                 Fields
                                   { allow_extra_fields = true
                                   ; fields =
                                       [ No_tag
                                           { name = "foo"
                                           ; required = true
                                           ; args = Cons (int_sexp_grammar.untyped, Empty)
                                           }
                                       ]
                                   }
                             }
                       }
                   ; No_tag
                       { name = "Forbid_extra_fields"
                       ; clause_kind =
                           List_clause
                             { args =
                                 Fields
                                   { allow_extra_fields = false
                                   ; fields =
                                       [ No_tag
                                           { name = "bar"
                                           ; required = true
                                           ; args = Cons (int_sexp_grammar.untyped, Empty)
                                           }
                                       ]
                                   }
                             }
                       }
                   ]
               }))
    }
  ;;

  let _ = t_sexp_grammar

  [@@@end]

  let _ = Allow_extra_fields { foo = 1 }
  let _ = Forbid_extra_fields { bar = 1 }
end
