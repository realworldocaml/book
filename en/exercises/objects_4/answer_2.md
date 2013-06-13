1.
  Here is one implementation.
  
```ocaml
  class ['key, 'value] imp_map (compare : 'key -> 'key -> comparison) =
     let equal key1 (key2, _) = compare key1 key2 = Equal in
     object (self : 'self)
        val mutable elements : ('key * 'value) list = []
        method insert key value = elements <- (key, value) :: elements
        method find key = snd (List.find (equal key) elements)
     end;;
```

