# Positions

In this directory, five parsers, produced by `ocamlyacc` and by four variants
of Menhir (code back-end versus table back-end; `%inline` enabled versus
`%inline` disabled), are compared concerning the computation of positions.

The source code is in `src`. The test inputs are in `inputs`.
The comparison is carried out by the script `test.sh`.
