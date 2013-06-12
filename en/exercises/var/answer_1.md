1.
  Legal.  The value is `1 : int`.

1.
  Legal.  The value is `1 : int`.

1.
  Illegal.  The variable `x` is undefined in the definition `y = x`.

1.
  Illegal.  The variable `x` is defined twice.

1.
  Legal.  The value is `1 : int`.

1.
  Legal.  The value is `2 : int`.

1.
  Illegal.  Identifiers may not begin with a single quote `'`.

1.
  Legal.  The value is `1 : int`.

1.
  Legal.  The identifier `x` represents the argument in the function body
    `x + 1` and the function itself in `x 2`.  The value is `3 : int`.

1.
  Legal.  The `rec` modifier has no effect here.  The value is `4 : int`.

1.
  Legal.  The identifier `++` represents function composition, so the value is
  `(1 * 2) + 1` (`3 : int`).

1.
  Legal.  The value is `3 - (2 - 1)` (`1`).

1.
  Legal.  Evaluation does not terminate because the operator `-` is defined in terms of
  itself.

1.
  Illegal.  Application has higher precedence
    than `+`, so the expression in the body is
    really `5 + (6 7)`, which is ill-typed.

1.
  Illegal.  All operators starting with a `+` sign are binary operators.

