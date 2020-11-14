
(rule
 (alias query_handler_loader)
 (deps
  (source_tree ./correct/query_handler_loader)
  (package core))
 (action
  (system "dune build @all @runtest --root ./correct/query_handler_loader")))

(alias
 (name runtest)
 (deps (alias query_handler_loader)))
