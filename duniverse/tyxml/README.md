# TyXML

TyXML is a library for building statically correct HTML5 and SVG documents:

```ocaml
open Tyxml
let to_ocaml = Html.(a ~a:[a_href "ocaml.org"] [txt "OCaml!"])
```

Tyxml can also be used with the standard HTML syntax, using the PPX:

```ocaml
open Tyxml
let%html to_ocaml = "<a href='ocaml.org'>OCaml!</a>"
```

Finally, TyXML can be used with Reason's JSX syntax: 

```reason
open Tyxml;
let to_reason = <a href="reasonml.github.io/"> "Reason!" </a>
```

TyXML provides a set of combinators which use the OCaml type system
to ensure the validity of the generated document.
TyXML can be used on a wide variety of context, either to produce
textual documents, to manipulate
the DOM tree using [Js_of_ocaml][], build virtual DOM with [virtual-dom][],
or for tierless web programming with [Eliom][].

The documentation can be consulted
[on the TyXML website](https://ocsigen.org/tyxml/). Examples are
available in the [examples](examples) directory.

[Eliom]: https://ocsigen.org/eliom/manual/clientserver-html
[Js_of_ocaml]: https://ocsigen.org/js_of_ocaml/api/Tyxml_js

## Installation

TyXML is available in [OPAM](https://opam.ocaml.org/):
```sh
opam install tyxml
```

To install the PPX:
```sh
opam install tyxml-ppx
```

To install the JSX:
```sh
opam install tyxml-jsx
```
