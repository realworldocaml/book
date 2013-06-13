1.
  The class definitions are pretty simple; they each implement an evaluator.
  
```ocaml
  class int_exp i =
  object (_ : #exp as 'self)
     method eval env = i
  end
  
  class binary_exp op (e1 : #exp) (e2 : #exp) =
  object
     method eval : 'a. (#env as 'a) -> int =
        (fun env -> op (e1#eval env) (e2#eval env))
  end
  
  class add_exp e1 e2 = binary_exp (+) e1 e2
  class sub_exp e1 e2 = binary_exp (-) e1 e2
  
  class var_exp v =
  object
     method eval : 'a. (#env as 'a) -> int = (fun env -> env#find v)
  end
```

1.
```ocaml
  class let_exp v (e1 : #exp) (e2 : #exp) =
  object
     method eval : 'a. (#env as 'a) -> int =
        (fun env -> e2#eval (env#add v (e1#eval env)))
  end
```

1.
  Unfortunately, all the classes need to be extended.
  
```ocaml
  class type exp2 =
  object
     inherit exp
     method closed : string list -> bool
  end
  
  class int_exp2 i =
  object (_ : #exp2 as 'self)
     inherit int_exp i
     method closed _ = true
  end
  
  class binary_exp2 op e1 e2 =
  object (_ : #exp2 as 'self)
     inherit binary_exp op e1 e2
     method closed defined_vars =
        e1#closed defined_vars && e2#closed defined_vars
  end
  
  class add_exp2 e1 e2 = binary_exp2 (+) e1 e2
  class sub_exp2 e1 e2 = binary_exp2 (-) e1 e2
  
  class let_exp2 v e1 e2 =
  object (_ : #exp2 as 'self)
     inherit let_exp v e1 e2
     method closed defined_vars =
        e1#closed defined_vars && e2#closed (v :: defined_vars)
  end
```

