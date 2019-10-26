ppx_pipebang
============

A ppx rewriter that inlines reverse application operators `|>` and `|!`.

`ppx_pipebang` rewrites `x |> f` and `x |! f` as `f x`, regardless of whether `|>` and
`|!` have been redefined.

Do not use `|!` in new code, it is there only for backwards compatibility.

This inlining is mostly done for historical reasons but it also allows `f` to have
optional arguments (like `Option.value_exn`).
