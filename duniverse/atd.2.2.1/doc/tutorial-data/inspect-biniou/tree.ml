open Printf

(* sample value *)
let tree : Tree_t.tree =
  `Node (
    `Node (`Empty, 1, `Empty),
    2,
    `Node (
      `Node (`Empty, 3, `Empty),
      4,
      `Node (`Empty, 5, `Empty)
    )
  )

let () =
  (* write sample value to file *)
  let fname = "tree.dat" in
  Atdgen_runtime.Util.Biniou.to_file Tree_b.write_tree fname tree;

  (* write sample value to string *)
  let s = Tree_b.string_of_tree tree in
  printf "raw value (saved as %s):\n%S\n" fname s;
  printf "length: %i\n" (String.length s);

  printf "pretty-printed value (without dictionary):\n";
  print_endline (Bi_io.view s);

  printf "pretty-printed value (with dictionary):\n";
  let unhash = Bi_io.make_unhash ["Empty"; "Node"; "foo"; "bar" ] in
  print_endline (Bi_io.view ~unhash s)
