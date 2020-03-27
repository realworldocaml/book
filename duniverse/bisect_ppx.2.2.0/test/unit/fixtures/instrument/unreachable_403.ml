(* Basic. *)
let test = function
  | () -> ()
  | () -> .
  (* This case should not be instrumented, as that would generate code that
     fails type checking: the OCaml type-checker accepts unreachable clauses
     whose expression is just ".", but warns then fails on clauses whose
     expression are of the form "<instrumentation>; .". *)
