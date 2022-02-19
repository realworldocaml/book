A successful run with a complete .coverage file.

  $ echo "(lang dune 2.7)" > dune-project
  $ cat > dune <<'EOF'
  > (executable
  >  (name test)
  >  (instrumentation (backend bisect_ppx)))
  > EOF
  $ dune exec ./test.exe --instrument-with bisect_ppx
  $ mv bisect*.coverage bisect0.coverage
  $ cat bisect0.coverage
  BISECT-COVERAGE-4 1 7 test.ml 6 13 30 45 245 229 212 6 1 0 1 0 0 0
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Coverage: 2/6 (33.33%)


Truncate the .coverage file, clipping one of the integer arrays.

  $ truncate -c -s 56 bisect0.coverage
  $ cat bisect0.coverage
  BISECT-COVERAGE-4 1 7 test.ml 6 13 30 45 245 229 212 6 1
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Error: cannot read coverage file 'bisect0.coverage': bad integer
  [1]

  $ truncate -c -s 55 bisect0.coverage
  $ cat bisect0.coverage
  BISECT-COVERAGE-4 1 7 test.ml 6 13 30 45 245 229 212 6 
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Error: cannot read coverage file 'bisect0.coverage': bad integer
  [1]

  $ truncate -c -s 54 bisect0.coverage
  $ cat bisect0.coverage
  BISECT-COVERAGE-4 1 7 test.ml 6 13 30 45 245 229 212 6
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Error: cannot read coverage file 'bisect0.coverage': bad integer
  [1]


Truncate the whole array.

  $ truncate -c -s 53 bisect0.coverage
  $ cat bisect0.coverage
  BISECT-COVERAGE-4 1 7 test.ml 6 13 30 45 245 229 212 
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Error: cannot read coverage file 'bisect0.coverage': bad integer
  [1]

  $ truncate -c -s 52 bisect0.coverage
  $ cat bisect0.coverage
  BISECT-COVERAGE-4 1 7 test.ml 6 13 30 45 245 229 212
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Error: cannot read coverage file 'bisect0.coverage': bad integer
  [1]


Truncate a string.

  $ truncate -c -s 30 bisect0.coverage
  $ cat bisect0.coverage
  BISECT-COVERAGE-4 1 7 test.ml 
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Error: cannot read coverage file 'bisect0.coverage': bad integer
  [1]

  $ truncate -c -s 29 bisect0.coverage
  $ cat bisect0.coverage
  BISECT-COVERAGE-4 1 7 test.ml
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Error: cannot read coverage file 'bisect0.coverage': bad integer
  [1]

  $ truncate -c -s 28 bisect0.coverage
  $ cat bisect0.coverage
  BISECT-COVERAGE-4 1 7 test.m
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Error: coverage file 'bisect0.coverage' is truncated
  [1]

  $ truncate -c -s 22 bisect0.coverage
  $ cat bisect0.coverage
  BISECT-COVERAGE-4 1 7 
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Error: coverage file 'bisect0.coverage' is truncated
  [1]

  $ truncate -c -s 21 bisect0.coverage
  $ cat bisect0.coverage
  BISECT-COVERAGE-4 1 7
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Error: coverage file 'bisect0.coverage' is truncated
  [1]

  $ truncate -c -s 20 bisect0.coverage
  $ cat bisect0.coverage
  BISECT-COVERAGE-4 1 
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Error: cannot read coverage file 'bisect0.coverage': bad integer
  [1]


Truncate the file header.

  $ truncate -c -s 18 bisect0.coverage
  $ cat bisect0.coverage
  BISECT-COVERAGE-4 
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Error: cannot read coverage file 'bisect0.coverage': bad integer
  [1]

  $ truncate -c -s 17 bisect0.coverage
  $ cat bisect0.coverage
  BISECT-COVERAGE-4
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Error: cannot read coverage file 'bisect0.coverage': bad integer
  [1]

  $ truncate -c -s 16 bisect0.coverage
  $ cat bisect0.coverage
  BISECT-COVERAGE-
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Error: coverage file 'bisect0.coverage' is truncated
  [1]


Truncate the whole file.

  $ truncate -c -s 0 bisect0.coverage
  $ cat bisect0.coverage
  $ bisect-ppx-report summary --verbose
  Info: found *.coverage files in './'
  Error: coverage file 'bisect0.coverage' is truncated
  [1]
