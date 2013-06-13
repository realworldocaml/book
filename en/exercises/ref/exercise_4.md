  
## Exercise
  \index{queue!functional}
  The FIFO queues described in Section~\ref{section:queues} are imperative; whenever a value is added to
  or taken from the queue, the queue is modified by side-effect.  Implement a \emph{persistent} queue
  with the following operations.
  
```ocaml
  val empty : 'a queue
  val add   : 'a queue -> 'a -> 'a queue
  val take  : 'a queue -> 'a * 'a queue
```
  The expression `add queue e` produces a new queue, without affecting the contents of the
  original queue `queue`.  The expression `take queue` returns an element of the
  queue `queue` and a new queue; again, the contents of the original queue `queue`
  are unaffected.
  
  $\star$ Can you implement the queue so that any sequence of $n$ `add` and `take`
  operations, in any order, take $O(n)$ time?  Hint: consider using lazy lists
  (Exercise~\ref{exercise:lazy-list}) to represent the queue, shifting the queue whenever the
  $\ms{front}$ is longer than the $\ms{back}$.  See Okasaki~\cite{Oka95}.
  
