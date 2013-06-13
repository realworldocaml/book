  
## Exercise
  In the following program, the function `sum_entries` sums up the integer values associated with
  each name in the list `names`.  The `List.assoc` function finds the value associated with
  the name, raising the `Not_found` exception if the entry is not found.  For example, the
  expression `sum_entries 0 ["a"; "c"]` would evaluate to `35`, and the expression
  `sum_entries 0 ["a"; "d"]` would raise the `Not_found` exception.
  
  \begin{center}
```ocaml
  let table = [("a", 10); ("b", 20); ("c", 25)]
  let rec sum_entries total (names : string list) =
     match names with
        name :: names' ->
           sum_entries (total + List.assoc name table) names'
      | [] ->
           total
```
  \end{center}
  Suppose we wish to catch the exception, arbitrarily assigning a value of 0 to each unknown entry.
  What is the difference between the following two functions?  Which form is preferable?
  
1. 
  \begin{center}
```ocaml
  let table = [("a", 10); ("b", 20); ("c", 25)]
  let rec sum_entries total (names : string list) =
     match names with
        name :: names' ->
           (try sum_entries (total + List.assoc name table) names' with
               Not_found ->
                  sum_entries total names')
      | [] ->
           total
```
  \end{center}
  
1.
  \begin{center}
```ocaml
  let table = [("a", 10); ("b", 20); ("c", 25)]
  let rec sum_entries total (names : string list) =
     match names with
        name :: names' ->
           let i =
              try List.assoc name table with
                 Not_found  ->
                    1
           in
              sum_entries (total + i) names'
      | [] ->
           total
```
  \end{center}
  
