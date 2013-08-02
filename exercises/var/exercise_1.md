  
## Exercise
  Which of the following let-expressions is legal? 
  For each expression that is legal, give its type and the value that it evaluates to.
  Otherwise, explain why the expression is not legal.
  
1. `let x = 1 in x`
  
  
1. `let x = 1 in let y = x in y`
  
  
1. `let x = 1 and y = x in y`
  
  
1. `let x = 1 and x = 2 in x`
  
  
1. `let x = 1 in let x = x in x`
  
  
1. `let a' = 1 in a' + 1`
  
  
1. `let 'a = 1 in 'a + 1`
  
  
1. `let a'b'c = 1 in a'b'c`
  
  
1. `let x x = x + 1 in x 2`
  
  
1. `let rec x x = x + x in x 2`
  
  
1.
  
```ocaml
  let (++) f g x = f (g x) in
  let f x = x + 1 in
  let g x = x * 2 in
  (f ++ g) 1
```
  
  
1. `let (-) x y = y - x in 1 - 2 - 3`
  
  
1. `let rec (-) x y = y - x in 1 - 2 - 3`
  
  
1. `let (+) x y z = x + y + z in 5 + 6 7`
  
  
1. `let (++) x = x + 1 in ++x`
  
  
