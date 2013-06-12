1.
  The type is `[|[]|] : '_a list array`.

1.
  Mutable fields are not values, so the field `fst` is not polymorphic because of the value restriction.
  The type is `{ fst = []; snd = []  : ('_a list, 'b list) mpair`}.

1.
  During a functional update, it is legal for the types of polymorphic fields to change.
  The expression `{ fst = (); snd = 2 `} has type `(unit, int) mpair`,
  but the final value is `{ fst = 1; snd = 2 `} of type `(int, int) mpair`.

