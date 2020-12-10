Environment variables can be loeaded in ocaml blocks

# OCaml

Environment variables can be loaded in an ocaml block environment.

```ocaml set-FOO=bar,set-BAR=foo
  # print_endline (Sys.getenv "FOO")
  bar
  - : unit = ()
  # print_endline (Sys.getenv "BAR")
  foo
  - : unit = ()
```

And the variable stays available in subsequent blocks.

```ocaml
  # print_endline (Sys.getenv "FOO")
  bar
  - : unit = ()
```

# Sh/bash

Environment variables can be loaded in an ocaml block environment.

```sh set-FAR=boo,set-BOO=far
  $ echo $FAR
  boo
  $ echo $BOO
  far
```

And the variable stays available in subsequent blocks.

```sh
  $ echo $BOO
  far
```

Environment variables can contain underscores

<!-- $MDX set-TWO_WORDS=success -->
```sh
  $ echo $TWO_WORDS
  success
```
