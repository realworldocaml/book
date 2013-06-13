1.
  The implementations are as follows.
  
```ocaml
  let nil = Nil
  let cons h t = Cons (h, t)
  let lazy_cons h f = LazyCons (h, defer f)
  
  let is_nil = function
     Nil -> true
   | Cons _ | LazyCons _ -> false
  
  let head = function
     Nil -> raise (Invalid_argument "head")
   | Cons (h, _)
   | LazyCons (h, _) -> h
  
  let tail = function
     Nil -> raise (Invalid_argument "tail")
   | Cons (_, t) -> t
   | LazyCons (_, f) -> force f
  
  let rec (@@) l1 l2 =
     match l1, l2 with
        Nil, l | l, Nil -> l
      | _ -> lazy_cons (head l1) (fun () -> tail l1 @@ l2)
```

