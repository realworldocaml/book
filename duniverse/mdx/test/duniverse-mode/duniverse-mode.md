In duniverse mode, the generated rule for this block should contain a `(package block)` dependency:

```ocaml
# #require "block";;
# 1 + 2
- int : 3
```

The generated rule should also contain `(package x)` deps for every require in the prelude, both
`.ml` preludes and string ones.

Finally it should have a `(package mdx)` dependency as well since with duniverse, mdx should be
vendored!
