include module type of Soup

val parse : string -> soup node
  [@@ocaml.deprecated
    "The module name Lambdasoup is deprecated. Use Soup instead."]
