open Easy_format

let make_data list_param label_param atom_param =
  let obj_param = ("{", ",", "}", list_param) in
  let array_param = ("[", ",", "]", list_param) in
  let at s = Atom (s, atom_param) in
  let obj =
    List (
      obj_param,
      [
	Label (
	  (at "x:", label_param),
	  at "y"
	);
	Label (
	  (at "y:", label_param),
	  List (obj_param, [Label ((at "z:", label_param), at "aaa")])
	);
	Label (
	(at "a:", label_param),
	  List (
	    array_param,
	    [
	      at "abc";
	      at "\"long long long......................................\
                    ....................................................\"";
	    ]
	  )
	);
	Label (
	  (at "\"a long label ..................\
                   .............................\":",
	   label_param),
	  List (
	    array_param,
	    [
	      at "123";
	      at "456";
	      at "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
	    ]
	  )
	)
      ]
    )
  in

  let array =
    List (array_param, [ at "a"; at "b"; at "c"; at "d" ])
  in

  Label (
    (at "abc:", label_param),
    List (
      array_param,
      [
	obj; array; obj;
	at "xyz";
      ]
    )
  )

(* List.init achieves the same but requires ocaml >= 4.06. *)
let list_init n f = Array.to_list (Array.init n f)

(* Test stack overflow *)
let () =
  let data =
    List ( ("[", ",", "]", list),
           list_init 1_000_000 (fun _i -> Atom ("x", atom))
         )
  in

  let (_: string) = Easy_format.Pretty.to_string data in
  ()

let () =
  let x1 = make_data list label atom in
  let x2 =
    make_data
      { list with
	  space_after_opening = false;
	  space_after_separator = false;
	  space_before_closing = false;
	  stick_to_label = false;
	  align_closing = false }
      { label with
	  space_after_label = true }
      atom
  in
  let x3 =
    make_data
      { list with
	  space_after_opening = false;
	  space_before_separator = true;
	  space_after_separator = true;
	  separators_stick_left = false;
	  space_before_closing = false;
	  stick_to_label = true;
	  align_closing = true }
      { label with
	  space_after_label = true }
      atom
  in
  let x4 =
    make_data { list with stick_to_label = false } label atom
  in

  Easy_format.Pretty.to_stdout x1;
  print_newline ();

  Easy_format.Pretty.to_stdout x2;
  print_newline ();

  Easy_format.Pretty.to_stdout x3;
  print_newline ();

  Easy_format.Pretty.to_stdout x4;
  print_newline ();

  Easy_format.Compact.to_stdout x1;
  print_newline ()

