# Coq-MiniCalc : a little demo of Lexing/Parsing in Coq

Pierre Letouzey, 2019

License: CC0

This is a toy demo of the Coq backend of `menhir`.

This micro-grammar recognizes arithmetic expressions : numbers, idents, `+` `*` `-` `/` and parentheses.
We provide a hand-written lexer and a minimal final test (compilation should display `OK`).

Tested with Coq 8.8 and Menhir 20190613.
Anything more recent than that should be ok.
