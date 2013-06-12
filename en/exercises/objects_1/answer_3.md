1.
    The object is initially created with a reference `self1` to itself, using the `self` parameter,
    and the value of `self` is checked (with pointer equality) before the withdraw operation
    is allowed.  The value for `self1` has to be set in an initializer (when the value of `self` is known).
    For simplicity, we use the empty object `object end` for the initial value of `self1`.
```ocaml
  let my_account =
  object (self : 'self)
      val mutable balance = 100
      val mutable self1 : < > = object end
      initializer self1 <- (self :> < >)
      method withdraw =
          if (self :> < >) != self1 then
             raise (Failure "object has been copied");
          if balance = 0 then
             raise (Failure "account is empty");
          balance <- balance - 1
  end
```

