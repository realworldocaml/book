  
## Exercise
  A \emph{graph} $(V, E)$ has a set of \emph{vertices} $V$ and a set of \emph{edges} $E \subseteq
  V \times V$, where each edge $(v_1, v_2)$ is a pair of vertices.  In a \emph{directed} graph, each
  edge is an arrow from one vertex to another.  For example, for the graph below, the set of vertices
  is $V = \{ 1, 2, 3, 4 \}$, and the set of edges is $\{ (1, 2), (1, 3), (1, 4), (2, 4), (3, 4) \}$.
  
  \begin{center}
  \includegraphics[scale=0.5]{graph1}
  \end{center}
  One way to represent a graph is with a dictionary `(vertex, vertex list) dictionary` where
  each entry in the dictionary lists the outgoing edges from that vertex.
  Assume the following type definitions.
  
```ocaml
  type vertex = int
  type graph = (vertex, vertex list) dictionary
```
  Write a function `reachable : graph -> vertex -> vertex -> bool`, where
  `reachable graph v1 v2` is `true` iff vertex `v2` is
  reachable from `v1` by following edges only in the forward direction.  Your algorithm should
  terminate on all inputs.
  
