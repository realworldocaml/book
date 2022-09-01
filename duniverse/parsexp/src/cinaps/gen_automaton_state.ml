open! Base

let print_constants () =
  let initial_int = Parsexp_symbolic_automaton.Automaton.State.(to_int initial) in
  let error_int = Parsexp_symbolic_automaton.Automaton.State.to_int Error in
  Stdio.print_endline
    [%string
      {|
let initial_state = %{initial_int#Int}
let error_state = %{error_int#Int}
|}]
;;
