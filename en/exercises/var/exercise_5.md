  
## Exercise
  Suppose you have a function on integers `f : int -> int` that is monotonically increasing
  over some range of arguments from `0` up to `n`.  That is,
  \hbox{`f i < f (i + 1)`}
  for any `0 $\le$ i $<$ n`.  In addition `f 0 < 0` and `f n > 0`.
  Write a function `search f n` that finds the smallest argument `i` where
  \hbox{`f i $\ge$ 0`}.
  
