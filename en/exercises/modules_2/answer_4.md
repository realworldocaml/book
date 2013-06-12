1.
  Instead of using a sharing constraint, we can add the type definition
  directly in the signature.
  
```ocaml
  module type Set2Sig = sig
     type 'a set = 'a Set.set
     val empty : 'a set
     ...
  end
```
  In fact, we don't even need to define a separate type.
  
```ocaml
  module type Set2Sig = sig
     val empty : 'a Set.set
     val add : 'a Set.set -> 'a -> 'a Set.set
     val mem : 'a -> 'a Set.set -> bool
  end
```

