1.
```ocaml
  class c1 :
  object
     val x : int
     method get : int
  end
```

1.
```ocaml
  class c2 :
  object ('self)
     method copy : 'self
  end
```

1.
```ocaml
  class c3 : int ->
  object
     method f : int -> < h : int >
     method g : int
  end
```

1.
  The type constraint removes the private status of the method `x`.
```ocaml
  class c4 :
  object 
     method x : int
  end
```

