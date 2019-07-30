val to_basic : t -> Basic.t
  (**
     Tuples are converted to JSON arrays,
     Variants are converted to JSON strings or arrays of a string (constructor)
     and a json value (argument).
     Long integers are converted to JSON strings.

     Examples:
{v
`Tuple [ `Int 1; `Float 2.3 ]   ->    `List [ `Int 1; `Float 2.3 ]
`Variant ("A", None)            ->    `String "A"
`Variant ("B", Some x)          ->    `List [ `String "B", x ]
`Intlit "12345678901234567890"  ->    `String "12345678901234567890"
v}
  *)
