  
## Exercise
  \index{depth-first search}
  One way to represent a directed graph is with an adjacency list stored directly in each vertex.
  Each vertex has a label and a list of out-edges; we also include a ``mark'' flag and an integer to
  be used by a depth-first-search.
  
```ocaml
  type 'a vertex =
     (* Vertex (label, out-edges, dfs-mark, dfs-index) *)
     Vertex of 'a * 'a vertex list ref * bool ref * int option ref
  
  type 'a directed_graph = 'a vertex list
```
  Depth-first search and breadth-first search are two highly useful graph algorithms.
  A depth-first search (DFS) traverses the graph, assigning to each vertex a DFS index and
  marking a vertex $v$ when all out-edges of $v$ have been explored.  The DFS search is performed as follows.
  
  Choose an unmarked vertex $u$ in the graph, push out-edges $(u, v)$ onto a stack.  Assign $u$ DFS index 0, set
  the DFS counter $c$ to 1, and then repeat the following until the stack is empty.
  
1. Pop an edge $(u, v)$ from the stack, and classify it according to the following
  table.
  
  \begin{center}
  \begin{tabular}{l|l}
  Condition & Edge type for $(u, v)$\\
  \hline
  $v$ does not have a DFS index & tree edge\\
  $\ms{DFS}(u) < \ms{DFS}(v)$ & forward edge\\
  $\ms{DFS}(u) > \ms{DFS}(v)$ and $v$ not marked & back edge\\
  $\ms{DFS}(u) > \ms{DFS}(v)$ and $v$ marked & cross edge
  \end{tabular}
  \end{center}
1.
  If $(u, v)$ is a tree edge, assign $v$ the current DFS index $c$, increment $c$, and push all edges
  $(v, w)$ onto the stack.
1.
  When all edges $(u, v)$ have been considered, mark the vertex $u$.
  Repeat until all vertices have been marked.
  A graph is \emph{cyclic} iff the DFS search found any back-edges.
  
  Implement a DFS search.  You can assume that all vertices are initially unmarked and their DFS index
  is `None`.
  
