type dimensions = { rows : int; columns : int }

external get_dimensions : unit -> dimensions option
  = "ocaml_alcotest_get_terminal_dimensions"
