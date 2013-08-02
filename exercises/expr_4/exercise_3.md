  
## Exercise
  Records can be used to implement abstract data structures, where the data structure is viewed as
  a record of functions, and the data representation is hidden.  For example, a type definition for a
  functional dictionary is as follows.
  
```ocaml
  type ('key, 'value) dictionary =
     { insert : 'key -> 'value -> ('key, 'value) dictionary;
       find   : 'key -> 'value
     }
  
  val empty : ('key, 'value) dictionary
```
  Implement the empty dictionary `empty`.  Your implementation should be pure, without side-effects.
  You are free to use any internal representation of the dictionary.
  
