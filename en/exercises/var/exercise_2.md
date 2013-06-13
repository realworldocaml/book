  
## Exercise
  What are the values of the following expressions?
  
1. `let x = 1 in let x = x + 1 in x`
  
  
1.
  
```ocaml
  let x = 1 in
  let f y = x in
  let x = 2 in
  f 0
```
  
  
1.
  
```ocaml
  let f x = x - 1 in
  let f x = f (x - 1) in
  f 2
```
  
  
1.
  
```ocaml
  let y = 2 in
  let f x = x + y in
  let f x = let y = 3 in f y in
  f 5
```
  
  
1.
  
```ocaml
  let rec factorial i =
     if i = 0 then 1 else i * factorial (i - 1)
  in
     factorial 5
```
  
