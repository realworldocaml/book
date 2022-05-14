open! Base

let print_constructors () =
  List.iter [%all: Parsexp_symbolic_automaton.Parse_error_reason.t] ~f:(fun reason ->
    Stdio.print_endline
      [%string "| %{reason#Parsexp_symbolic_automaton.Parse_error_reason}"])
;;
