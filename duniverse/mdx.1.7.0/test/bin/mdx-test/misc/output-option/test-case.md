`mdx-test` output can be configured via the `--output`/`-o` CLI option. It can be used to write
to a file or to stdout by passing `--output -`.

When not specified, it should default to `<input_file>.corrected`.

Check the dune file in this folder to see the actual tests!

```ocaml
# let x = 1 + 1;;
val x : int = 2
```
