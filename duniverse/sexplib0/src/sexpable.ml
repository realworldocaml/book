module type S = sig
  type t

  val t_of_sexp : Sexp.t -> t
  val sexp_of_t : t -> Sexp.t
end

module type S1 = sig
  type 'a t

  val t_of_sexp : (Sexp.t -> 'a) -> Sexp.t -> 'a t
  val sexp_of_t : ('a -> Sexp.t) -> 'a t -> Sexp.t
end

module type S2 = sig
  type ('a, 'b) t

  val t_of_sexp : (Sexp.t -> 'a) -> (Sexp.t -> 'b) -> Sexp.t -> ('a, 'b) t
  val sexp_of_t : ('a -> Sexp.t) -> ('b -> Sexp.t) -> ('a, 'b) t -> Sexp.t
end

module type S3 = sig
  type ('a, 'b, 'c) t

  val t_of_sexp
    :  (Sexp.t -> 'a)
    -> (Sexp.t -> 'b)
    -> (Sexp.t -> 'c)
    -> Sexp.t
    -> ('a, 'b, 'c) t

  val sexp_of_t
    :  ('a -> Sexp.t)
    -> ('b -> Sexp.t)
    -> ('c -> Sexp.t)
    -> ('a, 'b, 'c) t
    -> Sexp.t
end
