  
## Exercise
  Assume there is a class `name` that represents the
  name of a person.  We would normally say that a `person` is-a `human` and has-a `name`,
  
```ocaml
  class person (n : name) = object inherit human val name = n $\cdots$ end
```
  Suppose that instead, the class `person` inherits from both.
  
```ocaml
  class person (s : string) =
  object
     inherit human
     inherit name s
     $\cdots$
  end
```
  What is the difference?  Under what conditions would the different
  representations be preferred?
  
