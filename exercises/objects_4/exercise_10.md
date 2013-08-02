  
  
## Exercise
  Consider the following class definition.
  
```ocaml
  # class type ['a] c1 = object method f : c2 -> 'a end
    and c2 = object method g : int c1 end;;
  @
  \begin{topoutput}
  class type ['a] c1 = object constraint 'a = int method f : c2 -> 'a end
  and c2 = object method g : int c1 end
  \end{topoutput}
  @
```
  Unfortunately, even though the class type \lstinline$['a] c1$ should
  be polymorphic in \lstinline$'a$, a type constraint is inferred
  that \lstinline$'a = int$.  The problem is that polymorphic
  type definitions are not polymorphic \emph{within} a recursive
  definition.
  
1.
  
  Suggest a solution to the problem, where class type \lstinline$c1$ is
  truly polymorphic.
  
1.
  
  The following definition is rejected.
  
```ocaml
  # class type ['a] c1 = object method f : c2 -> 'a end
    and c2 = object method g : 'a. 'a c1 -> 'a end;;
  @
  \begin{toperror}
  Characters 79-94:
  and c2 = object method g : 'a. 'a c1 -> 'a end;;
                             ^^^^^^^^^^^^^^^
  This type scheme cannot quantify 'a :
  it escapes this scope.
  \end{toperror}
  @
```
  The problem arises from the same issue---the class \lstinline$['a] c1$
  is not polymorphic within the recursive definition, so the
  type \lstinline$'a. 'a c1 -> 'a$ is rejected.
  
  Suggest a solution to this problem.
  
