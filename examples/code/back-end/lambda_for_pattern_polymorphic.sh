  $ ocamlc -dlambda -c pattern_polymorphic.ml 2>&1
  (setglobal Pattern_polymorphic!
    (let
      (test/1002 =
         (function v/1003
           (if (!= v/1003 3306965)
             (if (>= v/1003 482771474) (if (>= v/1003 884917024) 100 102)
               (if (>= v/1003 3457716) 104 103))
             101)))
      (makeblock 0 test/1002)))
