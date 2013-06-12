1.
  The main task here is to separate the parts of the memoization.  We'll use a simple association list
  for the memo table.  The expression `fib $n$` is computed in quadratic time $O(n^2)$.  A
  more efficient implementation of the dictionary would reduct this to no more than $O(n \log n)$
  time.
  
```ocaml
  type ('a, 'b) memo = ('a * 'b) list
  
  let create_memo () = []
  
  let rec memo_find table x =
     match table with
        (x', y) :: _ when x' = x -> Some y
      | _ :: table -> memo_find table x
      | [] -> None
  
  let memo_fib table i =
     match memo_find table i with
        Some j -> j
      | None -> 
          match i with
             0 | 1 -> i
           | _ ->
              let j = memo_fib table (i - 1) + memo_fib table (i - 2) in
              memo_add table i j;
              j
```

