module type S = sig
  type t

  val t_sexp_grammar : t Sexplib0.Sexp_grammar.t
end

module type Sexp_grammar = sig
  module type S = S

  include module type of struct
    include Sexplib0.Sexp_grammar
  end

  (** Idiomatic usage looks like this: {[

        let t_of_sexp, t_sexp_grammar =
          remember_to_update_these_together ~t_of_sexp ~t_sexp_grammar

      ]} *)
  val remember_to_update_these_together
    :  t_of_sexp:(Sexp.t -> 'a)
    -> t_sexp_grammar:'a t
    -> (Sexp.t -> 'a) * 'a t
end
