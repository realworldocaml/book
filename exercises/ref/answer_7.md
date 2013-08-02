1.
  The function `graph_map` is definable, but it is difficult to implement it efficiently.
  The central issue is that, in order to preserve the structure of the graph, the function
  `graph_map` must be memoized using physical equality.  The following code gives a simple,
  but inefficient, implementation.
  
```ocaml
  let graph_map f graph =
     let memo = ref [] in
     let rec map u = function
        (u', v) :: table when u' == u -> v
      | _ :: table -> map u table
      | [] ->
         let (label, out_edges) = u in
         let new_out_edges = ref [] in
         let new_u = (f label, new_out_edges) in
         memo := (u, new_u) :: !memo;
         new_out_edges := map_list !out_edges
     and map_list edges =
        List.map (fun v -> map v !memo) edges
     in
     map_list graph
```
  The function `map` searches through the memo table in linear order, looking for an entry
  that matches with physical equality (`==`).  If so, the previous result is returned.
  Otherwise, a new vertex is created and added to the memo table.  Note that the new vertex
  `new_u` is added before following the out-edges recursively, preventing infinite looping
  on cyclic graphs.
  
  Given a graph with $n$ vertices and $m$ edges, the function `map` examines $n$ vertices worst case, so the total time complexity is of `graph_map` is $O(nm)$.
  
  An alternative representation that would support an efficient map is to give vertices unique names,
  and refer to them by name rather than referring to them directly.  The penalty is that edge
  traversal takes time $O(\log n)$ rather than constant time because of the dictionary lookup.
  
```ocaml
  (* A name for a vertex *)
  type name = int
  
  (* A vertex is (label, out-edges) *)
  type 'a vertex = 'a * name list ref
  
  (* A graph is (vertices, vertex dictionary) *)
  type 'a graph = name list * (name, 'a vertex) dictionary
  
  let graph_map f (vertices, dict) =
     vertices, dictionary_map f dict
```

