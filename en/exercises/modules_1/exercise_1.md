  
  
## Exercise
  Consider a file `f.ml` with the following contents.
```ocaml
  type t = int
  let f x = x
```
  Which of the following are legal `f.mli` files?
  
1. `f.mli` is empty.
  
1. `f.mli`:
```ocaml
  val f : 'a -> 'a
```
  
1. `f.mli`:
```ocaml
  val f : ('a -> 'b) -> ('a -> 'b)
```
  
1. `f.mli`:
```ocaml
  val f : t -> t
```
  
1. `f.mli`:
```ocaml
  type t
  val f : t -> t
```
  
1. `f.mli`:
```ocaml
  type s = int
  val f : s -> s
```
  
