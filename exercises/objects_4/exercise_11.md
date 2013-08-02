  
  
## Exercise
  As discussed in
  Section~\reflabelsection{comparing-objects-and-modules}, one problem
  with object-oriented implementations is that adding a new functionality
  to a class hierarchy might require modifying all the classes
  in the hierarchy.  \emph{Visitor design patterns} are one way in
  which this problem can be addressed.
  
  A \emph{visitor} is defined as an object with a method for each of the
  kinds of data.  For the type \lstinline$exp$, a visitor would have the
  following type.
  
```ocaml
  class type visitor =
    object ('self)
      method visit_int : int_exp -> unit
      method visit_var : var_exp -> unit
      method visit_add : add_exp -> unit
      method visit_if  : if_exp  -> unit
      method visit_let : let_exp -> unit
    end;;
```
  The class type \lstinline$exp$ is augmented with a
  method \lstinline$accept : visitor -> unit$ that guides the
  visitor through an expression, visiting every subexpression in turn.
  Here is a fragment of the code.
  
```ocaml
  class type exp =
    object ('self)
      method eval : int env -> int
      method accept : visitor -> unit
    end;;
  
  class int_exp (i : int) =
    object (self : 'self)
      method eval (_ : int env) = i
      method accept visitor = visitor#visit_int (self :> int_exp)
    end
  
  class add_exp (e1 : #exp) (e2 : #exp) =
    object (self : 'self)
      method eval env = e1#eval env + e2#eval env
      method accept visitor =
        visitor#visit (self :> add_exp);
        e1#accept visitor;
        e2#accept visitor
    end
  $\cdots$
```
  
1.[1.]
  
  One problem with this approach is the order of definitions.  For
  example, the class type \lstinline$visitor$ refers to the
  class \lstinline$add_exp$, which refers back to
  the \lstinline$visitor$ type in the definition of the
  method \lstinline$accept$.
  
1. We could simplify the types.  Would the following definition be acceptable?
  
```ocaml
  class type exp =
    object ('self)
      method eval : int env -> int
      method accept : visitor -> unit
    end
  
  and visitor =
    object ('self)
      method visit_int : exp -> unit
      method visit_var : exp -> unit
      $\cdots$
    end
```
  
1.
  
  What is a better way to solve this problem?
  
1.[2.]
  
  The class type \lstinline$visitor$ has one method for each specific
  kind of expression.  What must be done when a new kind of expression
  is added?
  As defined, the visitor pattern is not very useful because the classes
  do not provide any additional information about themselves.  Suppose
  we add a method \lstinline$explode$ that presents the contents
  of the object as a tuple.  Here is a fragment.
  
```ocaml
  class type exp = object $\cdots$ end
  and visitor = object $\cdots$ end
  
  and int_exp_type =
    object ('self)
      inherit exp
      method explode : int
    end
  
  and add_exp_type =
    object ('self)
      inherit exp
      method explode : exp * exp
    end
  $\cdots$
```
  
1.[3.]
  
  Since the method \lstinline$explode$ exposes the internal representation,
  it isn't really necessary for the \lstinline$accept$ methods to perform the
  recursive calls.  For example, we could make the following definition,
  and assume that the visitor will handle the recursive calls itself.
  
```ocaml
  class add_exp (e1 : #env) (e2 : #env) : add_exp_type =
    object (self : 'self)
      method eval env = e1#eval env + e2#eval env
      method accept visitor = visitor#visit_add (self :> add_exp_type)
      method explode = e1, e2
    end
```
  What are the advantages of this approach?  What are its disadvantages?
  
1.[4.]
  
  Another approach is, instead of passing the objects directly to the
  visitor, to pass the exploded values as arguments.  Here is the new visitor
  type definition.
  
```ocaml
  class type visitor =
    object ('self)
      method visit_int : int -> unit
      method visit_add : exp -> exp -> unit
      $\cdots$
    end
```
  What are the advantages of this approach?  What are its disadvantages?
  
1.[5.]
  
  Write a visitor to print out an expression.
  The visitors we have specified are imperative.  It is also possible to
  write pure visitors that compute without side-effects.  The visitor
  has a polymorphic class type parameterized over the type of values it
  computes.  As discussed in Exercise~\ref{exercise:monoclasses}, a recursive
  definition does not work, so we break apart the recursive definition.
  
```ocaml
  class type ['a, 'exp] pre_visitor =
    object ('self)
      method visit_int : int -> 'a
      method visit_var : string -> 'a
      method visit_add : 'exp -> 'exp -> 'a
      method visit_if  : 'exp -> 'exp -> 'exp -> 'a
      method visit_let : string -> 'exp -> 'exp -> 'a
    end;;
  
  class type exp =
    object ('self)
      method eval   : int env -> int
      method accept : 'a. ('a, exp) pre_visitor -> 'a
    end
  
  class type ['a] visitor = ['a, exp] pre_visitor
```
  
1.[6.]
  
  Rewrite the class definitions to implement the new \lstinline$accept$ methods.
  
1.[7.]
  
  Write an evaluator as a pure visitor \lstinline$eval_visitor$.
  The \lstinline$eval_visitor$ is not allowed to call the
  method \lstinline$eval$, and it is not allowed to use assignment or
  any other form of side-effect.
  
