  
## Exercise
  \index{insertion sort}
  
  \emph{Insertion sort}
  is a sorting algorithm that works by inserting elements one-by-one
  into an array of sorted elements.  Although the algorithm takes
  $O(n^2)$ time to sort an array of $n$ elements, it is simple, and it
  is also efficient when the array to be sorted is small.  The
  pseudo-code is as follows.
  
  \begin{ccode}
  insert(array a, int i)
      x <- a[i]
      j <- i - 1
      while j >= 0 and a[j] > x
          a[j] <- a[j - 1]
          j = j - 1
      a[j + 1] <- x
  
  insertion_sort(array a)
      i <- 1
      while i < length(a)
          insert(a, i)
          i <- i + 1
  \end{ccode}
  Write this program in OCaml.
  
