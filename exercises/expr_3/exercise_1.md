  
## Exercise
  Suppose you are given the following definition of a list type.
  
```ocaml
  type 'a mylist = Nil | Cons of 'a * 'a mylist
```
  
1. Write a function `map : ('a -> 'b) -> 'a mylist -> 'b mylist`, where
  
  `map $f$ [$x_0$; $x_1$; $\cdots$; $x_n$] = [$f$ $x_0$; $f$ $x_1$; $\cdots$; $f$ $x_n$]`.
  
  
1. Write a function `append : 'a mylist -> 'a mylist -> 'a mylist`, where
  
  `append [$x_1$; $\cdots$; $x_n$] [$x_{n + 1}$; $\cdots$; $x_{n + m}$] = [$x_1$; $\cdots$; $x_{n + m}$]`.
  
