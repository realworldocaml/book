  
## Exercise
  What is the result of evaluating the following programs?
  
1.
  
```ocaml
  exception A
  try raise A with
     A -> 1
```
  
1.
  
```ocaml
  exception A of int
  let f i =
     raise (A (100 / i));;
  let g i =
     try f i with
        A j -> j;;
  g 100
```
  
1.
  
```ocaml
  exception A of int
  let rec f i =
     if i = 0 then
        raise (A i)
     else
        g (i - 1)
  and g i =
     try f i with
        A i -> i + 1;;
  g 2
```
  
