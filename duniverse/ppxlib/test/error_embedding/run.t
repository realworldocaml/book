Most errors happening during ppxlib rewriting process are ultimately turned into
error extension nodes.

Undefined derivers are turned into error nodes

  $ echo "type t = int [@@deriving undefined]" >> undefined_deriver.ml
  $ ./deriver.exe undefined_deriver.ml
  type t = int[@@deriving undefined]
  include
    struct
      let _ = fun (_ : t) -> ()
      [%%ocaml.error
        "Ppxlib.Deriving: 'undefined' is not a supported type deriving generator"]
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

Error nodes are generated when parsing of payload fails.

export_string expects only one argument, a string, and output it.
Anything else will embed an error extension node

  $ echo "let _ = [%export_string \"string\"]" > parsing_payload_extension.ml
  $ echo "let _ = [%export_string \"string\" \"other\"]" >> parsing_payload_extension.ml
  $ echo "let _ = [%export_string identifier]" >> parsing_payload_extension.ml
  $ ./extender.exe parsing_payload_extension.ml
  let _ = "string"
  let _ = [%ocaml.error "constant expected"]
  let _ = [%ocaml.error "constant expected"]

  $ echo "type a = int [@@deriving a_string]" > parsing_payload_deriver.ml
  $ echo "type b = int [@@deriving a_string unexpected_args]" >> parsing_payload_deriver.ml
  $ ./deriver.exe parsing_payload_deriver.ml
  type a = int[@@deriving a_string]
  include struct let _ = fun (_ : a) -> ()
                 let _ = "derived_string" end[@@ocaml.doc "@inline"][@@merlin.hide
                                                                      ]
  type b = int[@@deriving a_string unexpected_args]
  include
    struct
      let _ = fun (_ : b) -> ()
      [%%ocaml.error
        "Ppxlib.Deriving: non-optional labelled argument or record expected"]
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

Error nodes are generated when dependent derivers are not applied.

  $ echo "type a = int [@@deriving a_dependent_string]" > dependent_derivers.ml
  $ ./deriver.exe dependent_derivers.ml
  type a = int[@@deriving a_dependent_string]
  include
    struct
      let _ = fun (_ : a) -> ()
      [%%ocaml.error
        "Deriver a_string is needed for a_dependent_string, you need to add it before in the list"]
      let _ = "derived_string"
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  $ echo "type b = int [@@deriving a_dependent_string, a_string]" > dependent_derivers.ml
  $ ./deriver.exe dependent_derivers.ml
  type b = int[@@deriving (a_dependent_string, a_string)]
  include
    struct
      let _ = fun (_ : b) -> ()
      [%%ocaml.error
        "Deriver a_string is needed for a_dependent_string, you need to add it before in the list"]
      let _ = "derived_string"
      let _ = "derived_string"
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
  $ echo "type b = int [@@deriving a_string, a_dependent_string]" > dependent_derivers.ml
  $ ./deriver.exe dependent_derivers.ml
  type b = int[@@deriving (a_string, a_dependent_string)]
  include
    struct
      let _ = fun (_ : b) -> ()
      let _ = "derived_string"
      let _ = "derived_string"
    end[@@ocaml.doc "@inline"][@@merlin.hide ]
