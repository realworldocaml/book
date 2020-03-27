This tool, `menhir-generate-printers`, reads a `.cmly` file and produces code
for three functions, namely `print_symbol`, `print_value`, and `print_token`.

```
val print_symbol: MenhirInterpreter.xsymbol -> string
```

By default, `print_symbol` prints the internal name of a (terminal or
nonterminal) symbol. This can however be changed by attaching a `[@name]`
attribute with this symbol. The attribute payload should be an OCaml
expression of type `string`.

```
val print_value: 'a MenhirInterpreter.symbol -> 'a -> string
val print_token: token -> string
```

By default, `print_value` and `print_token` ignore the semantic value and
print just the name of the symbol, like `print_symbol`. This can however be
changed by attaching a `[@printer]` attribute with this symbol. The attribute
payload should be an OCaml expression of type `_ -> string`, where `_` stands
for an appropriate type of semantic values.
