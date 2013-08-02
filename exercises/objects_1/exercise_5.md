  
## Exercise
  Let's reimplement the narrowing example from page~\pageref{page:narrowing-with-exceptions} in terms of
  polymorphic variants instead of exceptions.  The type definitions can be given as follows.
  
```ocaml
  type 'a animal = < actual : 'a; eat : unit >
  type 'a dog = < actual : 'a; eat : unit; bark : unit >
  type 'a cat = < actual : 'a; eat : unit; meow : unit >
  
  type 'a tag = [> `Dog of 'a tag dog | `Cat of 'a tag cat ] as 'a
```
1. Implement the rest of the example, including the function `chorus`.
1. What does the type variable `'a` represent?
1. What must be changed when a new type of animals is added, say `'a lizard`,
  for lizards that eat but don't vocalize?
  
