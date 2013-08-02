1.
  To ensure that the algorithm terminates, we need to keep track of which vertices have already been
  visited using a set `visited` of vertices that have already been visited.  The
  reachability test can be performed by a depth-first-search.
  
```ocaml
  let reachable graph v1 v2 =
     let rec search visited v =
        if v = v2 then
           true
        else if set_mem visited v then
           false
        else
           search_list (set_insert visited v) (dict_find graph v)
     and search_list visited = function
        v :: vl -> search visited v || search_list visited vl
      | [] -> false
     in
        search set_empty v1
```

