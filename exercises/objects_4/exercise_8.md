  
  
## Exercise
  Consider the following class definitions.
  
  \begin{center}
  \begin{tabular}{cc}
  \begin{minipage}[t]{2.8in}
```ocaml
  class ['a] alt_animal_pair1 (p : 'a) =
    object (self : 'self)
      constraint 'a = ('b, 'b) #pair
      constraint 'b = #animal
      method sleep =
        let a1, a2 = p#value in
        a1#sleep; a2#sleep
    end;;
```
  \end{minipage}
  &
  \begin{minipage}[t]{2.2in}
```ocaml
  class ['a] alt_animal_pair2
    (a1 : 'b) (a2 : 'c) =
    object (self : 'self)
      inherit ['b, 'c] pair a1 a2
      constraint 'a = 'b * 'c
      constraint 'b = #animal
      constraint 'c = #animal
      method sleep =
        a1#sleep; a2#sleep
    end;;
```
  \end{minipage}
  \end{tabular}
  \end{center}
1.
  
  The type variable \lstinline$'b$ is not a type parameter
  of \lstinline$alt_animal_pair1$.  Why is the definition legal?
  
1.
  
  Is the type \lstinline$['a] alt_animal_pair1$ covariant, contravariant, or invariant in \lstinline$'a$?
  
1.
  
  Suppose we have a class \lstinline$cat$ that is a subtype of \lstinline$animal$.
  What is the type of the following expression?
  
```ocaml
  new alt_animal_pair2 (new dog "Spot") (new cat "Fifi");;
```
  
1.
  
  What happens if the line \lstinline$constraint 'a = 'b * 'c$ is left out
  of the class definition for \lstinline$alt_animal_pair2$?
  
1.
  
  What if the line is replaced with \lstinline$constraint 'a = 'b -> 'c$?
  
1.
  
  In principle, is it ever necessary for a class to have more than one
  type parameter?
  
