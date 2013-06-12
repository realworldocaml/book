  
  
## Exercise
  Consider a file \lstinline+f.ml+ with the following contents.
```ocaml
  type t = int
  let f x = x
```
  Which of the following are legal \lstinline+f.mli+ files?
  
1. \lstinline+f.mli+ is empty.
  
1. \lstinline+f.mli+:
```ocaml
  val f : 'a -> 'a
```
  
1. \lstinline+f.mli+:
```ocaml
  val f : ('a -> 'b) -> ('a -> 'b)
```
  
1. \lstinline+f.mli+:
```ocaml
  val f : t -> t
```
  
1. \lstinline+f.mli+:
```ocaml
  type t
  val f : t -> t
```
  
1. \lstinline+f.mli+:
```ocaml
  type s = int
  val f : s -> s
```
  
