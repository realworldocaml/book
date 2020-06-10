We are testing the new label syntax on some existing tests.

# Set environment variables

Environment variables can be loaded in an ocaml block environment.

<!-- $MDX set-FOO=bar,set-BAR=foo -->
```ocaml
  # print_endline (Sys.getenv "FOO")
  bar
  - : unit = ()
  # print_endline (Sys.getenv "BAR")
  foo
  - : unit = ()
```

### Non-deterministic Outputs

<!-- $MDX non-deterministic=output -->
```sh
$ echo $RANDOM
4150
```

### Non-deterministic Commands

<!-- $MDX non-deterministic=command -->
```sh
$ touch toto
```
