1.
1. 
  
```ocaml
  let animal_model s =
     let rec self = function
        `L_eat -> printf "%s eats\n" s
     in
     self
```
  
1.
  
  A `model_dog` has all the methods of a `model_animal` so it can be used as an
  animal unchanged.  The coercion returns the dog without change.  To be more precise, consider the
  types.
  
```ocaml
  type model_dog = l:[`L_eat | `L_bark | `L_chase] -> $\cdots$
  type model_animal = l:[`L_eat] -> $\cdots$
```
  The relation `model_dog $\subtype$ model_animal` holds because
  `[`L_eat] $\subtype$ [`L_eat | `L_bark | `L_chase]`.
  
1. 
  
  A `model_dog` can be used as a model-`< chase : string -> unit>` without change.
  
1.
  
  A functional update becomes a record update.
  The new method `new_dog` is modeled as follows.
  
```ocaml
  let model_dog s =
     let rec self fields = function
        $\cdots$
      | `L_new_dog s ->
          self { fields with name = s }
     in
     self { name = s }
```
  
1.
  
  The pattern match is really just a table lookup, so it can be implemented in $O(\log n)$ time, where
  $n$ is the number of labels.  However, the number of labels in the program is fixed, so the pattern
  match can be implemented in constant time.
  
1.
  
  The advantage of hoisting the `vtable` is that it can be shared by multiple dog objects.
  The disadvantage is that it may be slightly more expensive because of the extra function call.

