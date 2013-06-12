1.
```ocaml
  let find_salary name =
     let rec search = function
         (name', _, salary) :: rest when name' = name -> salary
       | _ :: rest -> search rest
       | [] -> raise (Invalid_argument "find_salary")
     in
        search db
```

1.
  We can implement the function `select` by examining each tuple in order, collecting the
  matching tuples in an accumulator `found`.
```ocaml
  let select pred =
     let rec search found = function
        tuple :: rest when pred tuple ->
            search (tuple :: found) rest
      | _ :: rest -> search found rest
      | [] -> found
     in
         search db
```

