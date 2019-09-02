This test is a reggresion test for [PR 136](https://github.com/realworldocaml/mdx/pull/136).
It verifies that even if the md file is correct, the promotion is still done to the ml file.

```ocaml file=sync_to_ml_with_correct_md.ml
let f = "hello world!"
```
