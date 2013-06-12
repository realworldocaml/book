1.
```ocaml
  type heap = Node of int * heap list
  
  let makeheap i = Node (i, [])
  
  let meld (Node (i1, c1) as h1) (Node (i2, c2) as h2) =
     if i1 < i2 then
        Node (i1, h2 :: c1)
     else
        Node (i2, h1 :: c2)
  
  let insert h i =
     meld h (makeheap i)
  
  let findmin (Node (i, _)) =
     i
  
  let deletemin = function
     Node (_, []) -> raise (Invalid_argument "deletemin")
   | Node (_, [h]) -> h
   | Node (_, h :: t) ->
       let rec meld_all h = function
          x :: t -> meld_all (meld h x) t
        | [] -> h
       in
          meld_all h t
```

1.
```ocaml
  let insert_list heap = function
     i :: elements -> insert_list (insert heap i) elements
   | [] -> heap
  
  let heap_sort = function
     [] -> []
   | i :: elements ->
       let rec loop sorted heap =
          match heap with
             Node (i, []) -> i :: sorted
           | _ -> loop (findmin heap :: sorted) (deletemin heap)
       in
          loop [] (insert_list (makeheap i) elements)
```

