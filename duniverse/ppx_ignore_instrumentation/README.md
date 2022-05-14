ppx_ignore_instrumentation
==========================

A PPX that removes the AST nodes for instrumentation extensions, like
`[%probe]`, which are currently internal to Jane Street.

Instrumentation extensions don't have semantic effects during normal
execution so are safe to ignore.
