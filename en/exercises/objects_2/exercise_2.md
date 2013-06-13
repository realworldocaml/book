  
## Exercise
  What does the following program print out?
  
```ocaml
  class a (i : int) =
  let () = print_string "A let\n" in
  object
     initializer print_string "A init\n"
  end;;
  
  class b (i : int) =
  let () = print_string "B let\n" in
  object
     inherit a i
     initializer print_string "B init\n"
  end;;
  
  new b 0;;
```
  
