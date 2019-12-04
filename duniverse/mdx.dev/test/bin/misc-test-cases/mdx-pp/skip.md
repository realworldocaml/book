## Skipped blocks

```ocaml
let () = assert true
```

This block should not be executed by the tests:
```ocaml skip
let () = assert false (* Don't print this! *)
```

```ocaml
let () = assert true
```
