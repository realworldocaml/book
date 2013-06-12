1.
1. \lstinline+exception A+ is legal.
1. \lstinline+exception b+ is not legal because the name \lstinline+b+ must be capitalized.
1. \lstinline+exception C of string+ is legal.
1. \lstinline+exception D of exn+ is legal, it adds a recursive definition.
1. \lstinline+exception E of exn let x = E (E (E Not_found))+ is also legal, the value \lstinline+x+ has type \lstinline+exn+.
1. \lstinline+let f () = exception F raise F+ is not legal, exceptions can only be declared at the top-level, not
  within function bodies.

