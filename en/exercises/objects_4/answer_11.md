1.
1.
  
1.
  The simplified class type \lstinline$visitor$ is not acceptable because the
  visitor methods are called with a plain expression \lstinline$exp$, which
  isn't enough to do deconstruction.
  
1.
  
  A better way is to define class types for each of the kinds of
  expressions.  For example, an object of class \lstinline$add_exp$
  might have a class type definition \lstinline$add_exp_type$ that
  includes a method \lstinline$subterms$ to allow the visitor to
  decompose the sum.
  
```ocaml
  class type add_exp_type =
    object ('self)
      inherit exp
      method subterms : exp * exp
    end
  
  class add_exp (e1 : exp) (e2 : exp) =
    object (self : 'self)
      $\cdots$
      method subterms = e1, e2
    end
```
  
1.
  
  When a new kind of expression is added, the class type \lstinline$visitor$ must
  be extended with a new method definition, and all visitors must be updated
  to implement the new method.  This is the same issue that appears when adding
  a new expression to the union representation.
  
1. 
  
  The advantages of the approach is that the visitor can choose how to visit,
  and in what order.  For example, the visitor might choose to visit an
  expression from the bottom up, or from the top down.
  
  The disadvantages are that the burden of traversal is shifted onto the
  visitor, which means 1) each kind visitor must duplicate the traversal code,
  and 2) the traversal code may become out of date as the original definitions
  are modified.
  
  One intermediate approach is to define virtual classes that provide
  code for the common traversals.  Specific visitors would become
  subclasses of some version of a traversal class.
  
1. 
  
  Some advantages are that the new code may be slightly more efficient
  because the visitor doesn't have to call the
  method \lstinline$explode$ to decompose the object.  In many cases,
  the code may also be easier to write.  The principal disadvantage is
  the the visitor no longer has access to the original object, which may
  be required if, for example, the visitor wishes to modify the object
  in-place.
  
1.
  
  Here is an example printer, based on the exploded visitor definition from part 3.
  
```ocaml
  class print_visitor : visitor =
    object (self : 'self)
      method visit_int (e : int_exp_type) =
         print_int e#explode
      method visit_var (e : var_exp_type) =
         print_string e#explode
      method visit_add (e : add_exp_type) =
         let e1, e2 = e#explode in
         print_string "(";
         e1#accept (self :> visitor);
         print_string " + ";
         e2#accept (self :> visitor);
         print_string ")"
      method visit_if (e : if_exp_type) =
         let e1, e2, e3 = e#explode in
         print_string "(if ";
         e1#accept (self :> visitor);
         print_string " then ";
         e2#accept (self :> visitor);
         print_string " else ";
         e3#accept (self :> visitor);
         print_string ")"
      method visit_let (e : let_exp_type) =
         let v, e1, e2 = e#explode in
         printf "(let %s = " v;
         e1#accept (self :> visitor);
         print_string " in ";
         e2#accept (self :> visitor);
         print_string ")"
    end;;
```
  
1. 
  
  Here are the definitions of the expression classes.  We change
  the type of the method \lstinline$accept$ slightly so that it is
  possible to pass a subtype of a \lstinline$visitor$ without coercing.
  The method \lstinline$eval$ has been omitted (we'll define it
  as a visitor in the next part).
  
```ocaml
  class type ['a, 'exp] visitor =
    object ('self)
      method visit_int : int -> 'a
      method visit_var : string -> 'a
      method visit_add : 'exp -> 'exp -> 'a
      method visit_if  : 'exp -> 'exp -> 'exp -> 'a
      method visit_let : string -> 'exp -> 'exp -> 'a
    end;;
  
  class type exp =
    object ('self)
      method accept : 'a 'b. (('a, exp) #visitor as 'b) -> 'a
    end
  
  class int_exp (i : int) =
    object (self : 'self)
      method accept : 'a 'b. (('a, exp) #visitor as 'b) -> 'a =
        fun visitor -> visitor#visit_int i
    end
  
  class var_exp v =
    object (self : 'self)
      method accept : 'a 'b. (('a, exp) #visitor as 'b) -> 'a =
        fun visitor -> visitor#visit_var v
    end
  
  class add_exp (e1 : #exp) (e2 : #exp) =
    object (self : 'self)
      method accept : 'a 'b. (('a, exp) #visitor as 'b) -> 'a =
        fun visitor -> visitor#visit_add e1 e2
    end
  
  class if_exp (e1 : #exp) (e2 : #exp) (e3 : #exp) =
    object (self : 'self)
      method accept : 'a 'b. (('a, exp) #visitor as 'b) -> 'a =
        fun visitor -> visitor#visit_if e1 e2 e3
    end
  
  class let_exp (v : string) (e1 : #exp) (e2 : #exp) =
    object (self : 'self)
      method accept : 'a 'b. (('a, exp) #visitor as 'b) -> 'a =
        fun visitor -> visitor#visit_let v e1 e2
    end;;
```
  
1.
  
  The evaluator object needs to include the environment so that
  variables can be evaluated.  Here is a completely pure implementation.
  
```ocaml
  class eval_visitor : [int, exp] visitor =
    object (self : 'self)
      val env = new env
  
      method visit_int (i : int) =
         i
      method visit_var v =
         env#find v
      method visit_add (e1 : exp) (e2 : exp) =
         e1#accept self + e2#accept self
      method visit_if (e1 : exp) (e2 : exp) (e3 : exp) =
         if e1#accept self <> 0
         then e2#accept self
         else e3#accept self
      method visit_let v (e1 : exp) (e2 : exp) =
         e2#accept {< env = env#add v (e1#accept self) >}
    end;;
```
  Note that this code is nearly as concise as the version defined over
  the union representation of expressions.

