(** Sexp grammar definitions. *)

val unit_sexp_grammar : unit Sexp_grammar.t
val bool_sexp_grammar : bool Sexp_grammar.t
val string_sexp_grammar : string Sexp_grammar.t
val bytes_sexp_grammar : bytes Sexp_grammar.t
val char_sexp_grammar : char Sexp_grammar.t
val int_sexp_grammar : int Sexp_grammar.t
val float_sexp_grammar : float Sexp_grammar.t
val int32_sexp_grammar : int32 Sexp_grammar.t
val int64_sexp_grammar : int64 Sexp_grammar.t
val nativeint_sexp_grammar : nativeint Sexp_grammar.t
val sexp_t_sexp_grammar : Sexp.t Sexp_grammar.t
val ref_sexp_grammar : 'a Sexp_grammar.t -> 'a ref Sexp_grammar.t
val lazy_t_sexp_grammar : 'a Sexp_grammar.t -> 'a lazy_t Sexp_grammar.t
val option_sexp_grammar : 'a Sexp_grammar.t -> 'a option Sexp_grammar.t
val list_sexp_grammar : 'a Sexp_grammar.t -> 'a list Sexp_grammar.t
val array_sexp_grammar : 'a Sexp_grammar.t -> 'a array Sexp_grammar.t
val opaque_sexp_grammar : 'a Sexp_grammar.t
val fun_sexp_grammar : 'a Sexp_grammar.t
