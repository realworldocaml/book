let v = {
  Part3_t.name = "foo";
  data = Some [
    { Part1_t.x = 1; y = 2 };
    { Part1_t.x = 3; y = 4 };
  ]
}

let () =
  Atdgen_runtime.Util.Json.to_channel Part3_j.write_t3 stdout v;
  print_newline ()
