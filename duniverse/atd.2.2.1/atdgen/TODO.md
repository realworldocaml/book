* Support JSON object syntax for variants, e.g.:
  type t = A | B of int
  Currently supported: "A"
                       ["B", 123]
                       <"A">
                       <"B":123>
  To do: {"A": null}
         {"B": 123}

* Find a good way to support variants represented as records whose type is
  given by one of their fields.

* Plans for atdgen 2:
  - create one (sub)command for each target language
    (atdgen-ocaml, atdgen-java, atdgen-atd, atdgen-ts)
  - imply -std-json, i.e. do not produce code that produces JSON
    in the extended syntax for variants (<"A">, <"B":123>)
    or tuples (("a", 123, {"x":0}))
  - make it possible to produce all outputs in one call to atdgen.
    "atdgen foo -m tjv" would read file "foo.atd" and produce
     files foo_{t|j|v}.{ml|mli}
  - use classic variants instead of polymorphic variants by default
    since ocaml >= 4.01 makes them easier to use

* Support for other languages:
  - merge atdj (JSON serializers for Java) into atdgen
  - translate ATD into TypeScript type definitions
