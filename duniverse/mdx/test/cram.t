Test that cram syntax is fully supported.

# Copy of ellipsis.md

Long lines can be replaced by ellipsis:

  $ for i in `seq 1 10`; do echo $i; done
  1
  2
  ...
  10

  $ printf "foo\"\n\nbar"
  foo"
  
  ...

Lines ending with ellipsis

  $ echo Hello world
  Hello...

# Copy of exit.md

  $ exit 0

  $ exit 1
  [1]
  $ exit 10
  [10]

# Subset of non-det.md

<-- non-deterministic output
  $ echo $RANDOM
  4150

<-- non-deterministic command
  $ touch toto

# Copy of heredoc.md

Support for heredoc syntax:

  $ cat <<EOF \
  > hello\
  > world\
  > EOF
  hello
  world
  $ echo foo
  foo

And

  $ cat <<EOF > foo \
  > hello\
  > world\
  > EOF
  $ cat foo
  hello
  world
  $ echo foo
  foo

# Subset of multilines.md

Test multi-lines shell commands:

  $ for i in `seq 1 10`; do \
  >   echo $i; \
  > done
  1
  ...
  10

