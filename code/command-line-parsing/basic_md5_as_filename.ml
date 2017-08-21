

[@@@part "1"];;
let command =
  Command.basic
    ~summary:"Generate an MD5 hash of the input data"
    ~readme:(fun () -> "More detailed information")
    Command.Spec.(empty +> anon ("filename" %: file))
    do_hash

let () =
  Command.run ~version:"1.0" ~build_info:"RWO" command
