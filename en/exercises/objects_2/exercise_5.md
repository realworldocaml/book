  
## Exercise
  Consider the following definition of a stack of integers, implemented using the
  imperative lists of Exercise~\ref{exercise:classes-lists}.
  
```ocaml
  class int_stack =
  object
      val mutable items = new nil
      method add x = items <- new cons x items
      method take =
          let i = items#hd in
          items <- items#tl;
          i
  end
```
1.
  
  Define a class `int_queue` that implements a queue, by inheriting from
  the class `int_stack`, without overriding the method `take`.
  
  
1. Is it appropriate to say that a queue is-a stack?
