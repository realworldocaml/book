In this variant of the calc demo, the parser's semantic actions are
parameterized over a structure, called `Semantics`, which defines how numbers
should be interpreted. The parser is later instantiated with floating-point
numbers, so the calculator actually performs floating-point evaluation -- but
the grammar specification is independent of this detail.
