let unit_sexp_grammar : unit Sexp_grammar.t = { untyped = List Empty }
let bool_sexp_grammar : bool Sexp_grammar.t = { untyped = Bool }
let string_sexp_grammar : string Sexp_grammar.t = { untyped = String }
let bytes_sexp_grammar : bytes Sexp_grammar.t = { untyped = String }
let char_sexp_grammar : char Sexp_grammar.t = { untyped = Char }
let int_sexp_grammar : int Sexp_grammar.t = { untyped = Integer }
let float_sexp_grammar : float Sexp_grammar.t = { untyped = Float }
let int32_sexp_grammar : int32 Sexp_grammar.t = { untyped = Integer }
let int64_sexp_grammar : int64 Sexp_grammar.t = { untyped = Integer }
let nativeint_sexp_grammar : nativeint Sexp_grammar.t = { untyped = Integer }
let sexp_t_sexp_grammar : Sexp.t Sexp_grammar.t = { untyped = Any "Sexp.t" }
let ref_sexp_grammar grammar = Sexp_grammar.coerce grammar
let lazy_t_sexp_grammar grammar = Sexp_grammar.coerce grammar

let option_sexp_grammar ({ untyped } : _ Sexp_grammar.t) : _ option Sexp_grammar.t =
  { untyped = Option untyped }
;;

let list_sexp_grammar ({ untyped } : _ Sexp_grammar.t) : _ list Sexp_grammar.t =
  { untyped = List (Many untyped) }
;;

let array_sexp_grammar ({ untyped } : _ Sexp_grammar.t) : _ array Sexp_grammar.t =
  { untyped = List (Many untyped) }
;;

let empty_sexp_grammar : _ Sexp_grammar.t = { untyped = Union [] }
let opaque_sexp_grammar = empty_sexp_grammar
let fun_sexp_grammar = empty_sexp_grammar
