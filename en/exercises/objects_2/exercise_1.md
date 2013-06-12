  
## Exercise
  What are the class types for the following classes?
  
1.
  
```ocaml
  class c1 =
  object
     val x = 1
     method get = x
  end
```
  
  
1.
  
```ocaml
  class c2 =
  object
     method copy = {< >}
  end
```
  
  
1.
  
```ocaml
  class c3 y =
  object (self1)
     method f x =
        object (self2)
           val x = x
           method h = self1#g + x
        end
     method g = y
  end
```
  
  
1.
  
```ocaml
  class c4 =
  object (self : < x : int; .. > as 'self)
     method private x = 1
  end
```
  
