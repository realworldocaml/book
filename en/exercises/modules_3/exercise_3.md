  
## Exercise
  How many lines of output does the following program produce?
  
```ocaml
  module type S = sig val x : bool ref end
  
  module F (A : S) =
  struct
     let x = ref true;;
     if !A.x then begin
         print_string "A.x is true\n";
         A.x := false
     end
  end
  
  module G = F (F (F (struct let x = ref true end)))
```
  
