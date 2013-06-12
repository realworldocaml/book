  
## Exercise
  What is the value of the following expressions?
  
1. \lstinline|let x = ref 1 in let y = x in y := 2; !x|
  
  
1. \lstinline|let x = ref 1 in let y = ref 1 in y := 2|
  
  
1.
  
```ocaml
  let x = ref 1 in
  let y = ref x in
  !y := 2;
  !x
```
  
  
1.
  
```ocaml
  let fst (x, _) = x in
  let snd (_, x) = x in
  let y = ref 1 in
  let x = (y, y) in
  fst x := 2;
  !(snd x)
```
  
  
1.
  
```ocaml
  let x = ref 0 in
  let y = ref [5; 7; 2; 6] in
  while !y <> [] do
     x := !x + 1;
     y := List.tl !y
  done;
  !x
```
  
  
