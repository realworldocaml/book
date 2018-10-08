  $ jbuilder build search_with_timeout_no_leak.exe
%% --non-deterministic
  $ ./_build/default/search_with_timeout_no_leak.exe "concurrent programming" ocaml -timeout 0.2s
  concurrent programming
  ----------------------

  DuckDuckGo query failed: Timed out

  ocaml
  -----

  "OCaml, originally named Objective Caml, is the main implementation of
  the programming language Caml, created by Xavier Leroy, Jérôme
  Vouillon, Damien Doligez, Didier Rémy, Ascánder Suárez and others
  in 1996. A member of the ML language family, OCaml extends the core
  Caml language with object-oriented programming constructs."
