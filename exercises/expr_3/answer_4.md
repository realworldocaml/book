1.
```ocaml
  let rec eval = function
     Constant i -> i
   | Unary (Neg, e) -> -(eval e)
   | Binary (e1, op, e2) ->
        let i = eval e1 in
        let j = eval e2 in
           match op with
              Add -> i + j
            | Sub -> i - j
            | Mul -> i * j
            | Div -> i / j
```

