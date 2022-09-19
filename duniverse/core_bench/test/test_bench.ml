open Core

let main () =
  Command_unix.run
    (Command.group
       ~summary:"Several benchmarks"
       [ "basic", Basic_tests.command
       ; "tsc", Tsc.command
       ; "gc", Gc_tests.command
       ; "exceptions", Exception_tests.command
       ; "tuple", Tuple_tests.command
       ; "array", Array_tests.command
       ; "allocation", Allocation_tests.command
       ; "list", List_tests.command
       ])
;;

let () = main ()
