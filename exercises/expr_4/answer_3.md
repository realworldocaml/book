1.
  We'll use association lists.  The function `insert` adds to the list, and `find`
  searches the list.  The function `new_dictionary` is used to form a dictionary from
  an association list.
  
```ocaml
  let empty =
     let rec find entries key =
        match entries with
           (key', value) :: _ when key' = key -> value
         | _ :: entries -> find entries key
         | [] -> raise Not_found
     in
     let rec new_dictionary entries =
        { insert = insert entries;
          find = find entries
        }
     and insert entries key value =
        new_dictionary ((key, value) :: entries)
     in
     new_dictionary []
```

