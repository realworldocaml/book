open Base

[@@@warning "-37"]

module _ = struct
  type t = T of int [@@deriving_inline sexp_grammar]

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
                           List_clause { args = Cons (int_sexp_grammar.untyped, Empty) }
                       }
                   ]
               }))
    }
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end

module _ = struct
  type t =
    | T_int of int
    | T_u of u

  and u =
    | U_int of int
    | U_t of t
  [@@deriving_inline sexp_grammar]

  let _ = fun (_ : t) -> ()
  let _ = fun (_ : u) -> ()

  include struct
    open struct
      let (grammars__001_ : Sexplib0.Sexp_grammar.defn Stdlib.List.t Stdlib.Lazy.t) =
        lazy
          (let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) =
             { untyped = Tycon ("t", []) }
           and (u_sexp_grammar : u Sexplib0.Sexp_grammar.t) =
             { untyped = Tycon ("u", []) }
           in
           [ { tycon = "t"
             ; tyvars = []
             ; grammar =
                 Variant
                   { case_sensitivity = Case_sensitive_except_first_character
                   ; clauses =
                       [ No_tag
                           { name = "T_int"
                           ; clause_kind =
                               List_clause
                                 { args = Cons (int_sexp_grammar.untyped, Empty) }
                           }
                       ; No_tag
                           { name = "T_u"
                           ; clause_kind =
                               List_clause { args = Cons (u_sexp_grammar.untyped, Empty) }
                           }
                       ]
                   }
             }
           ; { tycon = "u"
             ; tyvars = []
             ; grammar =
                 Variant
                   { case_sensitivity = Case_sensitive_except_first_character
                   ; clauses =
                       [ No_tag
                           { name = "U_int"
                           ; clause_kind =
                               List_clause
                                 { args = Cons (int_sexp_grammar.untyped, Empty) }
                           }
                       ; No_tag
                           { name = "U_t"
                           ; clause_kind =
                               List_clause { args = Cons (t_sexp_grammar.untyped, Empty) }
                           }
                       ]
                   }
             }
           ])
      ;;

      let _ = grammars__001_
    end

    let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) =
      { untyped =
          Lazy (lazy (Recursive (Tycon ("t", []), Stdlib.Lazy.force grammars__001_)))
      }

    and (u_sexp_grammar : u Sexplib0.Sexp_grammar.t) =
      { untyped =
          Lazy (lazy (Recursive (Tycon ("u", []), Stdlib.Lazy.force grammars__001_)))
      }
    ;;

    let _ = t_sexp_grammar
    and _ = u_sexp_grammar
  end

  [@@@end]
end
