module Hashtbl = struct
  include Hashtbl

  let sexp_of_t = Conv.sexp_of_hashtbl
  let t_of_sexp = Conv.hashtbl_of_sexp
end

module Lazy = struct
  include Lazy

  let t_of_sexp = Conv.lazy_t_of_sexp
  let sexp_of_t = Conv.sexp_of_lazy_t
  let t_sexp_grammar = Sexplib0.Sexp.Private.Raw_grammar.Builtin.lazy_t_sexp_grammar
end

let sexp_of_unit = Conv.sexp_of_unit
let unit_of_sexp = Conv.unit_of_sexp
let unit_sexp_grammar = Sexplib0.Sexp.Private.Raw_grammar.Builtin.unit_sexp_grammar
let sexp_of_bool = Conv.sexp_of_bool
let bool_of_sexp = Conv.bool_of_sexp
let bool_sexp_grammar = Sexplib0.Sexp.Private.Raw_grammar.Builtin.bool_sexp_grammar
let sexp_of_string = Conv.sexp_of_string
let string_of_sexp = Conv.string_of_sexp
let string_sexp_grammar = Sexplib0.Sexp.Private.Raw_grammar.Builtin.string_sexp_grammar
let sexp_of_char = Conv.sexp_of_char
let char_of_sexp = Conv.char_of_sexp
let char_sexp_grammar = Sexplib0.Sexp.Private.Raw_grammar.Builtin.char_sexp_grammar
let sexp_of_int = Conv.sexp_of_int
let int_of_sexp = Conv.int_of_sexp
let int_sexp_grammar = Sexplib0.Sexp.Private.Raw_grammar.Builtin.int_sexp_grammar
let sexp_of_float = Conv.sexp_of_float
let float_of_sexp = Conv.float_of_sexp
let float_sexp_grammar = Sexplib0.Sexp.Private.Raw_grammar.Builtin.float_sexp_grammar
let sexp_of_int32 = Conv.sexp_of_int32
let int32_of_sexp = Conv.int32_of_sexp
let int32_sexp_grammar = Sexplib0.Sexp.Private.Raw_grammar.Builtin.int32_sexp_grammar
let sexp_of_int64 = Conv.sexp_of_int64
let int64_of_sexp = Conv.int64_of_sexp
let int64_sexp_grammar = Sexplib0.Sexp.Private.Raw_grammar.Builtin.int64_sexp_grammar
let sexp_of_nativeint = Conv.sexp_of_nativeint
let nativeint_of_sexp = Conv.nativeint_of_sexp

let nativeint_sexp_grammar =
  Sexplib0.Sexp.Private.Raw_grammar.Builtin.nativeint_sexp_grammar
;;

let sexp_of_ref = Conv.sexp_of_ref
let ref_of_sexp = Conv.ref_of_sexp
let ref_sexp_grammar = Sexplib0.Sexp.Private.Raw_grammar.Builtin.ref_sexp_grammar
let sexp_of_lazy_t = Conv.sexp_of_lazy_t
let lazy_t_of_sexp = Conv.lazy_t_of_sexp
let lazy_t_sexp_grammar = Sexplib0.Sexp.Private.Raw_grammar.Builtin.lazy_t_sexp_grammar
let sexp_of_option = Conv.sexp_of_option
let option_of_sexp = Conv.option_of_sexp
let option_sexp_grammar = Sexplib0.Sexp.Private.Raw_grammar.Builtin.option_sexp_grammar
let sexp_of_list = Conv.sexp_of_list
let list_of_sexp = Conv.list_of_sexp
let list_sexp_grammar = Sexplib0.Sexp.Private.Raw_grammar.Builtin.list_sexp_grammar
let sexp_of_array = Conv.sexp_of_array
let array_of_sexp = Conv.array_of_sexp
let array_sexp_grammar = Sexplib0.Sexp.Private.Raw_grammar.Builtin.array_sexp_grammar
let sexp_of_exn = Conv.sexp_of_exn
let exn_sexp_grammar = Sexplib0.Sexp.t_sexp_grammar
