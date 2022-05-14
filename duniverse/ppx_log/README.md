ppx_log
=======

A ppx rewriter that defines extension nodes for logging: primarily useful
to avoid the allocation of sexps that will never be logged.

```ocaml
[%log.debug log "example log" (content : Content.t)]
[%log.global.debug "example log" (content : Content.t)]
```

How to add to your project
--------------------------

You can simply `open Async` in the files that you want to log with
`Async.Log`.  Additionally, `open Async.Ppx_log_syntax.No_global` will
prevent you from using `Log.Global` if you want to avoid the global log.

`ppx_log` can add source code position to each logged message,
configurable at a library level by the `-log-source-position` switch:

```sexp
(preprocess (pps (ppx_jane ... -log-source-position)))
```

Finally `[%log.debug]` to your heart's content!

Tests and examples
------------------
Take a look at [the mdx file](test/test-ppx-log.mdx) for tested examples.
