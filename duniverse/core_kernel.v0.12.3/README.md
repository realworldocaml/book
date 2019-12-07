Portable standard library for OCaml
===================================

Core is an industrial-strength alternative to the OCaml standard
library.  It was developed by Jane Street, which is the largest
industrial user of OCaml.

Core_kernel is the system-independent part of Core, Jane Street's
industrial-strength alternative to the OCaml standard library. It is
aimed for cases when the full Core is not available, such as in
Javascript. It provides an overlay on the usual namespace, so the best
way to use it is to start your file with:

```ocaml
open Core_kernel
```

## Relationship to Core and Base

In sum:

- **Base**: Minimal stdlib replacement. Portable and lightweight and
  intended to be highly stable.
- **Core_kernel**: Extension of Base. More fully featured, with more
  code and dependencies, and APIs that evolve more quickly. Portable,
  and works on Javascript.
- **Core**: Core_kernel extended with UNIX APIs.

While Core is an extension of Core\_kernel, you can think of
Core\_kernel as an extension of Base. Many of Core\_kernel's modules are
extensions of modules in Base, where the Core\_kernel version adds
bin\_io support or locks in an API with `Stable`. Some modules, like
`Core_kernel.Map`, extend their Base equivalents to follow Core
conventions for the use of comparators.

---

Please report bugs and feature requests on
[GitHub](https://github.com/janestreet/core_kernel). For everything
else you can contact us at <ocaml-core@googlegroups.com>.

You can find all of Jane Street's open-source libraries on
[GitHub](https://github.com/janestreet).

Documentation can be found
[here](https://ocaml.janestreet.com/ocaml-core/latest/doc/core_kernel/index.html).
