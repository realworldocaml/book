Bad attributes generate an error message.

  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executable
  >  (name test)
  >  (modes byte)
  >  (instrumentation (backend bisect_ppx)))
  > EOF
  $ cat > test.ml <<'EOF'
  > [@@@coverage invalid]
  > EOF
  $ dune build --instrument-with bisect_ppx --display quiet
  File "test.ml", line 1, characters 0-21:
  1 | [@@@coverage invalid]
      ^^^^^^^^^^^^^^^^^^^^^
  Error: Bad payload in coverage attribute.
  [1]


Warnings 4 (fragile pattern matching due to wildcard) and 11 (unused match case)
in generated case instrumentation are suppressed. Warning 26 (unused variable
from as binding) is suppressed. Warning 28 (wildcard given to constant
constructor) is suppressed. One instance is still displayed for the user's code.

  $ cat > test.ml <<'EOF'
  > type t = A | B | C
  > let _ =
  >   match A with
  >   | A | B -> ()
  >   | C -> ()
  > let _ =
  >   match A with
  >   | A | B | C -> ()
  > let _ =
  >   match A with
  >   | (A as x) | (B as x) -> x
  >   | C -> C
  > let _ =
  >   match A with
  >   | A _ | B | C -> ()
  > EOF
  $ dune build --instrument-with bisect_ppx --display quiet 2>&1 | sed -e 's/ \[[^]]*\]//g'
  File "test.ml", line 15, characters 6-7:
  15 |   | A _ | B | C -> ()
             ^
  Error (warning 28): wildcard pattern given as argument to a constant constructor


Missing record labels warning (9) is suppressed from inserted documentation. It
is still emitted for the user's code.

  $ cat > test.ml <<'EOF'
  > type t = {a : int; b : int}
  > let _ =
  >   match {a = 0; b = 1} with
  >   | {a} | {a} -> a
  > EOF
  $ dune build --instrument-with bisect_ppx --display quiet 2>&1 | sed -e 's/ \[[^]]*\]//g'
  File "test.ml", line 4, characters 4-7:
  4 |   | {a} | {a} -> a
          ^^^
  Error (warning 9): the following labels are not bound in this record pattern:
  b
  Either bind these labels explicitly or add '; _' to the pattern.
  File "test.ml", line 4, characters 10-13:
  4 |   | {a} | {a} -> a
                ^^^
  Error (warning 9): the following labels are not bound in this record pattern:
  b
  Either bind these labels explicitly or add '; _' to the pattern.
  File "test.ml", line 4, characters 10-13:
  4 |   | {a} | {a} -> a
                ^^^
  Error (warning 12): this sub-pattern is unused.
