Environment variable can also loaded in an environment

```ocaml set-FOO=bar,set-BAR=foo
  # print_endline (Sys.getenv "FOO")
  bar
  - : unit = ()
  # print_endline (Sys.getenv "BAR")
  foo
  - : unit = ()
```

And the variable stays available in subsequent blocks

```ocaml
  # print_endline (Sys.getenv "FOO")
  bar
  - : unit = ()
```
