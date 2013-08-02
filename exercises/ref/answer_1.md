1.
  The variables `x` and `y` refer to the same reference cell, so the result is `2`.

1.
  The variables `x` and `y` refer to different reference cells, so the result is `1`.

1.
  Since `y` refers to `x`, assigning to `!y` is the same as assigning to `x`.
  The final value `!x` is `2`.

1.
  Both elements of the pair `(y, y)` refer to the same cell, so the assignment `fst x := 2`
  effects both parts; the value `!(snd x)` is `2`.

1.
  The variable `x` is incremented for each element of the list `y`, so the final
  value `!x` is `4`.

