  
  
## Exercise
  Which of the following class definitions are legal?  Explain your answers.
  
1.
  
```ocaml
  class [+'a] cl (x : 'a) =
    object (self : 'self)
      val f : 'a -> unit = fun x -> ()
      method value : unit -> 'a = fun () -> x
    end
```
  
1. 
  
```ocaml
  class [+'a] cl =
    object (self : 'self)
      method f : 'a -> unit = fun x -> ()
    end
```
  
1.
  
```ocaml
  class [+'a] cl =
    object (self : 'self)
      method private f : 'a -> unit = fun x -> ()
    end
```
  
1.
  
```ocaml
  class [+'a] cl =
    object (self : 'a)
      method copy : 'a = {< >}
    end
```
  
1.
  
```ocaml
  class [-'a] cl (x : 'a) =
    object (self : 'self)
      val mutable y = x
      method f x = y <- x
    end;;
```
  
1.
  
```ocaml
  class foo = object end
  class ['a] cl (x : 'a) =
    object
      constraint 'a = #foo
      method value : #foo = x
    end
```
  
1.
  
```ocaml
  class foo = object end
  class [-'a] cl (x : #foo as 'a) =
    object
      method value : #foo = x
    end
```
  
  
