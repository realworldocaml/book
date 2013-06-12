  
## Exercise
  A \emph{heap} of integers is a data structure supporting the following operations.
  
  \begin{itemize}
1. `makeheap : int -> heap`: create a heap containing a single element,
1. `insert : heap -> int -> heap`: add an element to a heap; duplicates are allowed,
1. `findmin : heap -> int`: return the smallest element of the heap.
1. `deletemin : heap -> heap`: return a new heap that is the same as the original, without the smallest element.
1. `meld : heap -> heap -> heap`: join two heaps into a new heap containing the elements of both.
  \end{itemize}
  A heap can be represented as a binary tree, where for any node $a$, if $b$ is a child node of $a$,
  then $\ms{label}(a) \le \ms{label}(b)$.  The order of children does not matter.  A \emph{pairing heap} is a
  particular implementation where the operations are performed as follows.
  
  \begin{itemize}
1. `makeheap i`: produce a single-node tree with `i` at the root.
1. `insert h i = meld h (makeheap i)`.
1. `findmin h`: return the root label.
1. `deletemin h`: remove the root, and `meld` the subtrees.
1. `meld h1 h2`: compare the roots, and make the heap with the larger element a subtree of the other.
  \end{itemize}
1.
  Define a type `heap` and implement the five operations.
  
  
1. A \emph{heap sort} is performed by inserting the elements to be sorted into a heap,
  then the values are extracted from smallest to largest.  Write a function
  `heap_sort : int list -> int list` that performs a heap sort,
  where the result is sorted from largest to smallest.
  
