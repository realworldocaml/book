# CHANGES

## 2020/11/20

* New module `DataFlow`, which performs a forward data flow analysis over a
  directed graph. (Such a computation could previously be performed by using
  the generic solver `Fix.Make`, but it was somewhat awkward to write, as it
  required access to predecessors. The new algorithm is easier to use and is
  more efficient.)

* In `Memoize`, new combinator `curried`, which can be used in combination
  with `fix` or `defensive_fix`. Thus, for instance, `curried fix` is a
  fixed point combinator that constructs a memoizing two-argument curried
  function.

## 2020/01/31

* In `Gensym`, new abstract type `generator`,
  with three functions `generator`, `fresh`, and `current`.

* In `Memoize`, new function `visibly_memoize`,
  which not only returns a memoized function,
  but also provides outside access to the memoization table.

* New signatures `ONGOING_NUMBERING` and `TWO_PHASE_NUMBERING`
  and new module `Numbering`,
  which provides facilities for numbering things.

* Breaking change: the module `Fix.Number`
  is renamed `Fix.GraphNumbering`.

## 2018/12/06

* New release, including new modules (`Gensym`, `Memoize`,
  `Tabulate`, `Number`, `HashCons`, `Prop`, `Glue`),
  new convenience functors (`Fix.ForHashedType`, etc.),
  and new demos.
  The least-fixed-point computation algorithm is unchanged.

## 2013/06/11

* Initial release of the package,
  containing just `Fix.Make`, the
  least-fixed-point computation algorithm.
