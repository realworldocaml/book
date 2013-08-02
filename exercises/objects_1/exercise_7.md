  
## Exercise
  In OCaml, an object of type $t_1$ can be coerced to any supertype $t_2$, regardless of whether type $t_2$
  has a name.  This differs from some other languages.  For example, in C++, an object can safely be
  coerced to any of its superclasses, but arbitrary supertypes are not allowed.  This is mainly
  because objects in C++ are represented as a sequence of fields and methods (for space efficiency, methods
  are usually represented in a separate array called a \misspelled{\emph{vtable}}).  For instance, if class $C$ is
  a subclass of two independent classes $A$ and $B$, their representations are laid out in order.
  
  \begin{center}
  \includegraphics[scale=0.75]{c++-object}
  \end{center}
  The object $A$ is laid out first, followed by $B$, then any additional fields in $C$.  A pointer to
  a $C$ object is also a pointer to an $A$, so this coercion has no runtime cost.  The coercion from
  $C$ to $B$ is also allowed with a bit of pointer arithmetic.
  
  In OCaml, the situation is quite different.  The order of methods and fields in an object doesn't
  matter, coercions to arbitrary supertypes are allowed, and coercions never have a runtime cost.  To
  help understand how this works, let's build a model of objects using polymorphic variants.
  
  Abstractly, an object is just a thing that reacts to messages that are sent to it---in other words,
  it is a function on method names.  Given an object $o$ with method names $m_1, m_2, \ldots, m_n$, the
  names are given variant labels ``L_m1`, ``L_m2`, $\ldots$, ``L_mn`.  The
  object becomes a pattern match over method labels, and the fields become let-definitions.
  Here is a dog object together with the corresponding model.
  
  \begin{center}
  \begin{tabular}{c|c}
  Object & Model\\
  \hline
  \begin{minipage}[t]{2.2in}
```ocaml
  type dog =
     < eat : unit;
       bark : unit;
       chase : string -> unit
     >
  
  let dog s =
  object (self)
     val name = s
     method eat = printf "%s eats\n" name
     method bark = printf "%s barks\n" name
     method chase s =
        self#bark;
        printf "%s chases %s\n" name s
  end
```
  \end{minipage}
  &
  \begin{minipage}[t]{2.2in}
```ocaml
  ! type model_dog =
  !    l:[`L_eat | `L_bark | `L_chase] -> 
  !       (match l with
  !           `L_eat | `L_bark -> unit
  !         | `L_chase -> (string -> unit))
  
  let model_dog s =
     let name = s in
     let rec self = function
        `L_eat -> printf "%s eats\n" name
      | `L_bark -> printf "%s barks\n" name
      | `L_chase -> (fun s ->
            self `L_bark;
            printf "%s chases %s\n" name s)
     in
     self
```
  \end{minipage}
  \end{tabular}
  \end{center}
  The recursive function `self` represents the object.  It takes a method label, and returns
  the method value.  The type `model_dog` can't be defined in OCaml, because it is
  a \emph{dependent} type.  Informally it says that a `model_dog` is a function that takes a
  label `l`.  If `l` is ``L_eat` or ``L_bark`, then the result
  type is `unit`.  If the label is ``L_chase`, the result type
  is `string -> unit`.
  
1. Suppose an animal object is defined as follows.
  
```ocaml
  type animal = < eat : unit >
  let animal s = object method eat = printf "%s eats\n" s end
```
  Write the model `model_animal` for an animal object.
  
1.
  Given a `model_dog` $e$, how is a coercion `($e$ : model_dog :> model_animal)` implemented?
  
1.
  How is a coercion `($e$ : dog :> < chase : string -> unit >)` implemented in the model?
  Suppose that, instead of representing fields as individual let-definitions, the fields of an object
  are collected in a record, with the compiler inserting the appropriate projections.  For example,
  here is a revised `model_dog`.
  
```ocaml
  type dog_fields = { name : string }
  
  let model_dog s =
     let rec self fields = function
        `L_eat -> printf "%s eats\n" fields.name
      | `L_bark -> printf "%s barks\n" fields.name
      | `L_chase -> (fun s ->
            self fields `L_bark;
            printf "%s chases %s\n" fields.name s)
     in
     self { name = s }
```
1.[4.] In this revised version, how is a functional update implemented?
  Explain your answer by giving the model for a new method
  
```ocaml
  method new_dog s = {< name = s >}.
```
  
1.[5.]
  What is the complexity of method dispatch?  Meaning, given an arbitrary method label, how long does
  it take to perform the pattern match?
  Suppose the pattern match is hoisted out of the object into a separate function `vtable`.
  
```ocaml
  let model_dog s =
     let vtable = function
        `L_eat -> (fun self fields -> printf "%s eats\n" fields.name)
      | `L_bark -> (fun self fields -> printf "%s barks\n" fields.name)
      | `L_chase -> (fun self fields s ->
            self fields `L_bark;
            printf "%s chases %s\n" fields.name s)
     in
     let rec self fields label =
        vtable label self fields
     in
     self { name = s }
```
1.[6.]  What are the advantages of the separate `vtable`?  What are some disadvantages?
  
