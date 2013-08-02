1.
1.
  
  Here is a complete definition.  Since the type is open, there is a
  wildcard case for unknown expressions.
  
```ocaml
  let rec eval1 env = function
     `Int i -> i
   | `Var v -> Env.find env v
   | `Add (e1, e2) -> eval1 env e1 + eval1 env e2
   | `If (e1, e2, e3) ->
        if eval1 env e1 <> 0
        then eval1 env e2
        else eval1 env e3
   | `Let (v, e1, e2) ->
        let i = eval1 env e1 in
        let env' = Env.add env v i in
        eval1 env' e2
   | _ ->
        raise (Failure "eval")
```
  
1.
  
  The pre-evaluator \lstinline$pre_eval1$ is very similar
  to \lstinline$eval1$, but it is not directly recursive.
  
```ocaml
  let pre_eval1 eval_subterm env = function
     `Int i -> i
   | `Var v -> Env.find env v
   | `Add (e1, e2) ->
        eval_subterm env e1 + eval_subterm env e2
   | `If (e1, e2, e3) ->
        if eval_subterm env e1 <> 0
        then eval_subterm env e2
        else eval_subterm env e3
   | `Let (v, e1, e2) ->
        let i = eval_subterm env e1 in
        let env' = Env.add env v i in
        eval_subterm env' e2
   | _ ->
        raise (Failure "eval")
```
  
1.
  
  The function \lstinline$make_eval$ wraps the pre-evaluator in a
  fixpoint definition.
  
```ocaml
  let rec make_eval pre_eval env e =
     pre_eval (make_eval pre_eval) env e
```
  
1.
  
  The evaluator can be constructed using the function \lstinline$make_eval$.
  
```ocaml
  let eval2 env e = make_eval pre_eval2 env e
```

