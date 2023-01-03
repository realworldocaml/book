dune subst requires that the project name must exist as a package. However, dune
doesn't verify that this is the case

  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > (name bar)
  > (package (name foo) (authors "John Doe <john@doe.com>"))
  > (package (name baz) (authors "John Doe <john@doe.com>"))
  > EOF

  $ X=%%; cat > file.ml <<EOF
  > let name = "${X}NAME${X}"
  > let authors = "${X}PKG_AUTHORS${X}"
  > let version = "${X}VERSION${X}"
  > EOF

  $ git init --quiet
  $ git add .
  $ git commit -am _ --quiet
  $ git tag -a 1.0 -m 1.0

  $ dune build @all

  $ dune subst
  File "dune-project", line 2, characters 6-9:
  2 | (name bar)
            ^^^
  Error: Package bar doesn't exist.
  [1]

  $ cat file.ml
  let name = "dune"
  let authors = "Jane Street Group, LLC <opensource@janestreet.com>"
  let version = "3.6.1"
