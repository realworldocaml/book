  
  
## Exercise
  We also stated in
  Section~\reflabelsection{comparing-objects-and-modules} that one
  problem with the traditional functional representation is that it is hard to add a
  new case to a union, because each of the functions that operate on the
  data must also be updated.
  
  One way to address this is through the use of \emph{polymorphic
  variants}, discussed in Section~\reflabelsection{open-union-types}.
  Polymorphic variants can be defined as ``open'' types that can be
  later extended.  For the evaluator example, here is how we might
  define the initial type of expressions.
  
```ocaml
  type 'a exp1 = 'a constraint 'a =
   [> `Int of int
    | `Var of string
    | `Add of 'a * 'a
    | `If  of 'a * 'a * 'a
    | `Let of string * 'a * 'a ]
```
  The type \lstinline$'a exp$ is an open type that includes at least
  the cases specified in the type definition.
  The type of an evaluator is defined as follows, where the
  module \lstinline$Env$ is defined on
  page~\pageref{page:polyclasses-env}.
  
```ocaml
  type 'a evaluator = int Env.t -> 'a -> int
```
1.[1.]
  
  Write an evaluator (of type \lstinline$'a exp evaluator$).
  We can extend the type of expressions by adding an additional constraint
  that specifies the new kinds of expressions.  For example, this is how we might
  add products as a kind of expression.
  
```ocaml
  type 'a exp2 = 'a
     constraint 'a = 'a exp1
     constraint 'a = [> `Mul of 'a * 'a ]
```
  The next step is to define an evaluator of type
  \lstinline$'a exp2 evaluator$.
  However, we don't want to reimplement it
  completely---we would like to be able to re-use the previous
  implementation.  For this, we need a kind of ``open recursion.''
  Let's define a \emph{pre-evaluator} as a function of the following
  type.  That is, a pre-evaluator takes an evaluator as an argument
  for computing values of subterms.
  
```ocaml
  type 'a pre_evaluator = 'a evaluator -> 'a evaluator
  
  let pre_eval1 eval_subterm env = function
     `Add (e1, e2) -> eval_subterm env e1 + eval_subterm env e2
   | $\cdots$
```
  The function has type \lstinline$pre_eval1 : 'a exp1 pre_evaluator$.
  
1.[2.]
  
  Write the complete definition of \lstinline$pre_eval1$.
  
1.[3.]
  
  Write a function \lstinline$make_eval$ that turns a pre-evaluator into
  an evaluator.  Hint: this is a kind of ``fixpoint'' definition, explored
  in Exercise~\ref{exercise:tims-and-jasons-y-combinator}.
  
```ocaml
  val make_eval : 'a pre_evaluator -> 'a evaluator
```
  
1.[4.]
  
  The pre-evaluator \lstinline$pre_eval2 : 'a exp2 pre_evaluator$
  can be implemented as follows.
  
```ocaml
  let pre_eval2 eval_subterm env = function
     `Mul (e1, e2) -> eval_subterm env e1 * eval_subterm env e2
   | e -> pre_eval1 eval_subterm env e
```
  Implement the evaluator \lstinline$eval2 : 'a exp2 evaluator$
  in terms of \lstinline$pre_eval2$.
  
