  
## Exercise
  One issue with graph data structures is that some familiar operations are hard to implement.  For
  example, consider the following representation (similar to the previous exercise) for a directed graph.
  
```ocaml
  (* Vertex (label, out-edges) *)
  type 'a vertex = Vertex of 'a * 'a vertex list ref
  type 'a directed_graph = 'a vertex list
```
  Suppose we want to define a polymorphic map function on graphs.
  
```ocaml
  val graph_map : ('a -> 'b) -> 'a directed_graph -> 'b directed_graph
```
  Given an arbitrary function `f : 'a -> 'b` and a graph `g`, the expression
  `graph_map f g` should produce a graph isomorphic to `g`, but where
  `f` has been applied to each label.  Is the function `graph_map` definable?  If
  so, describe the implementation.  If not, explain why not.  Is there another implementation of
  graphs where `graph_map` can be implemented efficiently?
  
