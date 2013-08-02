1.
  Well typed.  The value is `-1`.

1.
  Well typed.  Subtraction is left-associative, so the value is
  `-4`.

1.
  Well typed.  The value is `3`.

1.
  Well typed.  The value is `0x15` (`21` in decimal).

1.
  Well typed.  On a 32-bit machine, `1073741823` is the maximum
  integer, so the value is `-1073741824`.  On a 64-bit machine, the addition does not
  overflow, so the result is `1073741824`.

1.
  Ill typed.  The operator `+` is for integer addition
  only.

1.
  Ill typed.  The operator `^` is string concatenation.

1.
  Ill typed.  The missing `else` branch has type
  `unit`, which is not compatible with `1`.

1.
  Well typed.  The result is `()`.

1.
  Well-typed.  On most machines, `0.3 -. 0.2` is very close to, but different from,
    `0.1`, so the result is `'b'`.

1.
  Well-typed.  The value is `true` (since disjunction
  `||` is a short-circuit operator).

1.
  Well typed, because `-` has higher precedence than `>`.
  The result is `false`.

1.
  Well typed.  The value is `'w'`.

1.
  Well typed, but the index `11` is out of bounds,
  so the expression does not evaluate to a value.

1.
  Well typed.  The value is `false`.

1.
  Well typed.  The ASCII character code for `'a'` is
  `97`.

1.
  Well typed.  The value is the unit `()`.

1.
  Well typed.  The value is `()`.

1.
  Well typed.  The value is `()`.

