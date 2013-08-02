1.
  We'll keep a flag `cyclic` to indicate whether the graph is cyclic.  First, all the mark
  bits and DFS counters are cleared, then the function `search` is called to perform the DFS
  search.  It isn't necessary to keep an explicit stack of edges---in effect, the OCaml runtime stack
  is serving as the edge stack.
  
```ocaml
  let rec dfs graph =
     let cyclic = ref false in
     let dfs_counter = ref 0 in
  
     (* The main DFS search *)
     let rec search (Vertex (_, u_edges, u_mark, u_index)) =
        if not !u_mark then begin
           let c = !dfs_counter in
           u_index := Some c;
           dfs_counter := c + 1;
           List.iter (fun (Vertex (_, _, v_mark, v_index) as v) ->
              match !v_index with
                 Some index ->
                    if index < c && not !v_mark then
                       cyclic := true
               | None ->
                    search v) !u_edges;
           u_mark := true
        end
     in
  
     (* Reset the graph state *)
     List.iter (fun (Vertex (_, _, mark, index)) ->
        mark := false;
        index := None) graph;
  
     (* DFS over all the vertices in the graph *)
     List.iter search graph
```

