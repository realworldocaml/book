1.
  OCaml doesn't provide a way to rename fields, so the only thing we can
  do is to hide the field and use methods to access it.  Suppose the
  class `programmer` is defined as follows.
  
```ocaml
  class programmer =
  object
     inherit person
     val favorite_language = "OCaml"
  end
```
  The renamed class `programmer'` could be defined as follows.
  
```ocaml
  class type programmer_type' =
  object
     val name : string
     val address : string
     method favorite_programming_language : string
  end
  
  class programmer' : programmer_type' =
  object
     inherit programmer
     method favorite_programming_language = favorite_language
  end
```

