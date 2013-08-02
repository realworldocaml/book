1.
  Each function can be defined in its own file using a functor, where the
  functor argument defines both functions.
  
```ocaml
  module type RSig = sig
     val f : int -> int
     val g : int -> int
  end
  
  module FFun (R : RSig) = struct
     open R
     let rec f i = $\cdots$
  end
```
  The glue code can be placed in a third file, using recursive modules
  to ``tie the knot.''
  
```ocaml
  module rec F : sig val f : int -> int end = FFun (R)
  and F : sig val g : int -> int end = GFun (R)
  and R : RSig = struct
     let f = F.f
     let g = G.g
  end
```

