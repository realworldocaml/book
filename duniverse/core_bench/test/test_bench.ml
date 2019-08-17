open Core

let main () =
  Command.run (Command.group
                 ~summary:"Several benchmarks"
                 [
                   "basic"     , Basic_tests.     command;
                   "tsc"       , Tsc.             command;
                   "gc"        , Gc_tests.        command;
                   "exceptions", Exception_tests. command;
                   "tuple"     , Tuple_tests.     command;
                   "array"     , Array_tests.     command;
                   "allocation", Allocation_tests.command;

                 ])

let () = main ()







