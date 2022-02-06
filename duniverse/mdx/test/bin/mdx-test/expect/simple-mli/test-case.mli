(** This is a doc comment with some code blocks in it:

    {[
      # List.map (fun x -> x * x) [(1 + 9); 2; 3];;
      - : int list = [100; 4; 9]
    ]}

    {[List.map (fun x -> x * x) [1; 2; 3]]}


    {[
      let my_list = [1; 2; 3] in
      let the_list = 0 :: my_list in
      List.map (fun x -> x * x) the_list;;
      let another_phrase = "foo";;
      let the = "the" in
      let last = "last" in
      let phrase = "phrase" in
      print_endline (String.concat " " [the; last; phrase])
    ]}

    With the optional header:

    {@ocaml[
      # List.map (fun x -> x * x) [(1 + 9); 2; 3];;
      - : int list = [100; 4; 9]
      # List.map (fun x -> x * x) [1; 2; 3];;
      - : int list = [1; 4; 9]
    ]}

    A shell block:

    {@sh set-FOO=Hello,set-BAR=Bash[
      $ echo $FOO $BAR
      Hello Bash
    ]}

    A block that doesn't run:

    {@text[
      # 1
      = 2 ?
    ]}
*)
val foo : string

(** {@ocaml[1 + 1 = 3]} *)
val bar : string

(** {@ocaml skip[1 + 1 = 3]} *)
val baz : string
