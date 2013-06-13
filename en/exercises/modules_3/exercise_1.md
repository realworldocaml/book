  
## Exercise
  Which of the following are legal programs?  Explain your answers.
  
1.
  
```ocaml
  module type XSig = sig val i : int end
  module F (X : XSig) = X
```
  
1.
  
```ocaml
  module type S = sig end
  module Apply (F : functor (A : S) -> S) (A : S) = F (A)
```
  
1.
  
```ocaml
  module type ISig = sig val i : int end
  module F (I : ISig) : ISig = struct let i = I.i + 1 end
  let j = F(struct let i = 1 end).i
```
  
1.
  
```ocaml
  module X = struct type t = int end
  module F (X) = struct type t = X.t end
```
  
1.
  
```ocaml
  module F (X : sig type t = A | B end) : sig type t = A | B end = X
```
  
1.
  
```ocaml
  module F (X : sig type t = A | B end) : sig type t = A | B end =
  struct type t = A | B end
```
  
1.
  
```ocaml
  module F (X : sig type t = A | B end) : sig type t = A | B end =
  struct type t = X.t end
```
  
  
