open! Base

module Maybe = struct
  type 'a t = 'a option [@@deriving_inline sexp_grammar]

  let _ = fun (_ : 'a t) -> ()

  let (t_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t) =
    fun _'a_sexp_grammar -> option_sexp_grammar _'a_sexp_grammar
  ;;

  let _ = t_sexp_grammar

  [@@@end]
end

module Make (T : sig
    type 'a t [@@deriving sexp_grammar]
  end) =
struct
  [@@@warning "-37"]

  type 'a t = T of 'a T.t u

  and 'a u = U of 'a T.t t Maybe.t [@@deriving_inline sexp_grammar]

  let _ = fun (_ : 'a t) -> ()
  let _ = fun (_ : 'a u) -> ()

  include struct
    open struct
      let (grammars__001_ : Sexplib0.Sexp_grammar.defn Stdlib.List.t Stdlib.Lazy.t) =
        lazy
          (let (t_sexp_grammar
                : 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t)
            =
            fun _'a_sexp_grammar ->
              { untyped = Tycon ("t", [ _'a_sexp_grammar.untyped ]) }
           and (u_sexp_grammar
                : 'a Sexplib0.Sexp_grammar.t -> 'a u Sexplib0.Sexp_grammar.t)
             =
             fun _'a_sexp_grammar ->
               { untyped = Tycon ("u", [ _'a_sexp_grammar.untyped ]) }
           in
           [ { tycon = "t"
             ; tyvars = [ "a" ]
             ; grammar =
                 Variant
                   { case_sensitivity = Case_sensitive_except_first_character
                   ; clauses =
                       [ No_tag
                           { name = "T"
                           ; clause_kind =
                               List_clause
                                 { args =
                                     Cons
                                       ( (u_sexp_grammar
                                            (T.t_sexp_grammar { untyped = Tyvar "a" }))
                                         .untyped
                                       , Empty )
                                 }
                           }
                       ]
                   }
             }
           ; { tycon = "u"
             ; tyvars = [ "a" ]
             ; grammar =
                 Variant
                   { case_sensitivity = Case_sensitive_except_first_character
                   ; clauses =
                       [ No_tag
                           { name = "U"
                           ; clause_kind =
                               List_clause
                                 { args =
                                     Cons
                                       ( (Maybe.t_sexp_grammar
                                            (t_sexp_grammar
                                               (T.t_sexp_grammar { untyped = Tyvar "a" })))
                                         .untyped
                                       , Empty )
                                 }
                           }
                       ]
                   }
             }
           ])
      ;;

      let _ = grammars__001_
    end

    let (t_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a t Sexplib0.Sexp_grammar.t) =
      fun _'a_sexp_grammar ->
      { untyped =
          Recursive
            (Tycon ("t", [ _'a_sexp_grammar.untyped ]), Stdlib.Lazy.force grammars__001_)
      }

    and (u_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a u Sexplib0.Sexp_grammar.t) =
      fun _'a_sexp_grammar ->
      { untyped =
          Recursive
            (Tycon ("u", [ _'a_sexp_grammar.untyped ]), Stdlib.Lazy.force grammars__001_)
      }
    ;;

    let _ = t_sexp_grammar
    and _ = u_sexp_grammar
  end

  [@@@end]

  type 'a v = V of 'a t [@@deriving_inline sexp_grammar]

  let _ = fun (_ : 'a v) -> ()

  let (v_sexp_grammar : 'a Sexplib0.Sexp_grammar.t -> 'a v Sexplib0.Sexp_grammar.t) =
    fun _'a_sexp_grammar ->
    { untyped =
        Variant
          { case_sensitivity = Case_sensitive_except_first_character
          ; clauses =
              [ No_tag
                  { name = "V"
                  ; clause_kind =
                      List_clause
                        { args = Cons ((t_sexp_grammar _'a_sexp_grammar).untyped, Empty) }
                  }
              ]
          }
    }
  ;;

  let _ = v_sexp_grammar

  [@@@end]
end

module T1 = Make (Maybe)
module T2 = Make (T1)

type t = int T2.t * int T1.t [@@deriving_inline sexp_grammar]

let _ = fun (_ : t) -> ()

let (t_sexp_grammar : t Sexplib0.Sexp_grammar.t) =
  { untyped =
      Lazy
        (lazy
          (List
             (Cons
                ( (T2.t_sexp_grammar int_sexp_grammar).untyped
                , Cons ((T1.t_sexp_grammar int_sexp_grammar).untyped, Empty) ))))
  }
;;

let _ = t_sexp_grammar

[@@@end]
