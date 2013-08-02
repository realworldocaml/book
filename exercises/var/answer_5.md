1.
  A simple solution is to search for the first positive value starting from 0,
  taking $O(n)$ iterations to find the answer.
  
```ocaml
  let search f n =
     let rec loop i =
        if f i >= 0 then
           i
        else
           loop (i + 1)
     in
        loop 0
```
  However, it is easy to write a more efficient implementation.  An algorithm to find the answer in
  $O(\log n)$ iterations using a binary search can be defined as follows.
  
```ocaml
  let search f n =
     let rec loop i j =
        if i < j - 1 then
           let k = (i + j) / 2 in
              if f k >= 0 then
                 loop i k
              else
                 loop k j
        else
           j
     in
        loop 0 n
```

