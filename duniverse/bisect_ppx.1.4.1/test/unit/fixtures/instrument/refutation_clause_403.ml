let test = function
  | () -> ()
  | () -> .
    (* bisect-ppx should not instrument this clause,
       as it would return code that fails at type-checking time:
       the OCaml type-checker accepts unreachable clauses whose
       expression is just ".", but warns then fails on 
       clauses whose expression are of the form "<instrumentation>; ."
       that bisect-ppx naively generates. *)
