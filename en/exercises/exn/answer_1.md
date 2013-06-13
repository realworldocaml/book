1.
1. `exception A` is legal.
1. `exception b` is not legal because the name `b` must be capitalized.
1. `exception C of string` is legal.
1. `exception D of exn` is legal, it adds a recursive definition.
1. `exception E of exn let x = E (E (E Not_found))` is also legal, the value `x` has type `exn`.
1. `let f () = exception F raise F` is not legal, exceptions can only be declared at the top-level, not
  within function bodies.

