  
  
## Exercise
  The restriction about free type variables applies only to non-private method
  types.  Which of the following definitions are legal?  For those that
  are legal, give their types.  For those that are not legal, explain
  why.
  
1.
  
```ocaml
  class c1 = object val x = [] end;;
```
  
1.
  
```ocaml
  class c2 = object val x = ref [] end;;
```
  
1.
  
```ocaml
  class c3 x = object val y = x end
```
  
1.
  
```ocaml
  class c4 x = object val y = x method z = y end
```
  
1.
  
```ocaml
  class c5 x = object val y = x + 1 method z = y end
```
  
1.
  
```ocaml
  class c6 (x : 'a) = object constraint 'a = int method y = x end;;
```
  
