  $ ocamlc -dlambda -c pattern_monomorphic_large.ml 2>&1
  (setglobal Pattern_monomorphic_large!
    (let
      (test/1007 =
         (function v/1008
           (switch* v/1008
            case int 0: 100
            case int 1: 101
            case int 2: 102
            case int 3: 103)))
      (makeblock 0 test/1007)))
