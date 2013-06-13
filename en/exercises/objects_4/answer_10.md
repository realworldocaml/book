1.
  The solution in both cases is to break apart the recursive definition
  by adding a type parameter to the class type \lstinline$c1$ that represents
  the class \lstinline$c2$.
  
```ocaml
  class type ['a, 'c2] pre_c1 = object method f : 'c2 -> 'a end
```
  The solutions are then as follows.
  
1.
  
```ocaml
  class type c2 = object method g : (int, c2) pre_c1 end
  class type ['a] c1 = ['a, c2] pre_c1
```
  
1.
  
```ocaml
  class type c2 = object method g : 'a. ('a, c2) pre_c1 -> 'a end
  class type ['a] c1 = ['a, c2] pre_c1
```

