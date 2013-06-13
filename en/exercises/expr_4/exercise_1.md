  
## Exercise
  Reference cells are a special case of records, with the following type definition.
  
```ocaml
  type 'a ref = { mutable contents : 'a }
```
  Implement the operations on reference cells.
  
```ocaml
  val ref  : 'a -> 'a ref
  val (!)  : 'a ref -> 'a
  val (:=) : 'a ref -> 'a -> unit
```
  
