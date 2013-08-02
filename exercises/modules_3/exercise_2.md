  
## Exercise
  Consider the following well-typed program.
  
```ocaml
  module type T = sig type t val x : t end
  module A = struct type t = int let x = 0 end
  module B = struct type t = int let x = 0 end
  module C = A
  module F (X : T) = X
  module G (X : T) : T = X
  module D1 = F (A)
  module D2 = F (B)
  module D3 = F (C)
  module E1 = G (A)
  module E2 = G (B)
  module E3 = G (C)
```
  Which of the following expressions are legal?  Which have type errors?
  
1.
  
```ocaml
  D1.x + 1
```
  
1.
  
```ocaml
  D1.x = D2.x
```
  
1.
  
```ocaml
  D1.x = D3.x
```
  
1.
  
```ocaml
  E1.x + 1
```
  
1.
  
```ocaml
  E1.x = E2.x
```
  
1.
  
```ocaml
  E1.x = E3.x
```
  
1.
  
```ocaml
  D1.x = E1.x
```
  
