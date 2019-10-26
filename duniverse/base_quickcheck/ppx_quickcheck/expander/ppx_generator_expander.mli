open! Import

val compound
  :  generator_of_core_type : (core_type -> expression)
  -> loc : location
  -> fields : 'a list
  -> (module Field_syntax.S with type ast = 'a)
  -> expression

val variant
  :  generator_of_core_type : (core_type -> expression)
  -> loc : location
  -> variant_type : core_type
  -> clauses : 'a list
  -> (module Clause_syntax.S with type ast = 'a)
  -> expression

val arrow
  :  generator_of_core_type : (core_type -> expression)
  -> observer_of_core_type : (core_type -> expression)
  -> loc : location
  -> arg_label : arg_label
  -> input_type : core_type
  -> output_type : core_type
  -> expression
