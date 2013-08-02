1.
  The central difference between a set and a dictionary is that a node has both a `'key` and
  a `'value`.  The `add` function is similary to `insert`, and the
  `find` function is similar to `mem`.
  
```ocaml
  type ('key, 'value) dictionary =
     Node of 'key * 'value * ('key, 'value) dictionary * ('key, 'value) dictionary
   | Leaf
  
  let empty = Leaf
  
  let rec add dict key value =
     match dict with
        Leaf -> Node (key, value, Leaf, Leaf)
      | Node (key', value', left, right) ->
           if key < key' then
              Node (key', value', add left key value, right)
           else if key > key' then
              Node (key', value', left, add right key value)
           else (* key = key' *)
              Node (key, value, left, right)
        
  let rec find dict key =
     match dict with
        Leaf ->
           raise Not_found
      | Node (key', value, left, right) ->
           if key < key' then
              find left key
           else if key > key' then
              find right key
           else (* key = key' *)
              value
```

