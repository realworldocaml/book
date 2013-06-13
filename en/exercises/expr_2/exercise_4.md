  
## Exercise
  Suppose you are implementing a relational employee database, where the database is
  a list of tuples `name * phone * salary`.
  
```ocaml
  let db =
     ["John", "x3456", 50.1;
      "Jane", "x1234", 107.3;
      "Joan", "unlisted", 12.7]
```
  
1. Write a function `find_salary : string -> float` that returns the salary
  of an employee, given the name.
  
  
  
1. Write a general function
```ocaml
  select : (string * string * float -> bool) -> (string * string * float) list
```
  that returns a list of all the tuples that match the predicate.  For example the expression
  `select (fun (_, _, salary) -> salary < 100.0)` would return the tuples for John
  and Joan.
  
