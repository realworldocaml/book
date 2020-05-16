You can specify which section of a markdown file should be handled by
`ocaml-mdx pp` by using the `--section` option.

## Valuable

```ocaml
let msg0 = "I should be in the .ml file!"
```

### Valuable subsection

```ocaml
let msg1 = "Me too!"
```

## Ignored

```ocaml
let msg2 = "But I shouldn't!"
```

## Ignored as well

```ocaml
let msg3 = "And neither should I!"
```
