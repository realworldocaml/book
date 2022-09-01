ppx_disable_unused_warnings
===========================

The `@disable_unused_warnings` annotation disables the many OCaml
compiler warnings having to do with something being used (variable,
constructor, declaration, `open`, `rec` keyword, etc.).  It can be
used at all three annotation scopes:

- `[@@@disable_unused_warnings]`
- `[@@disable_unused_warnings]`
- `[@disable_unused_warnings]`

As of OCaml 4.10, it translates `[@disable_unused_warnings]` to:

  `[@@@warning "-26-27-32-33-34-35-36-37-38-39-60-66-67]`

