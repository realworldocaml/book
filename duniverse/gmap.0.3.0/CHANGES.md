## 0.3.0 (2019-04-20)

* S.union and S.merge could invalidate the invariant:
  foreach k \in m . m[k] = (key, value) /\ k = key
  which could lead to assertion fail in find
* The signatures uses semi-explicit polymorphism with a record type:
  * S.equal : { f : 'a key -> 'a -> 'a -> bool } -> t -> t -> bool
  * S.merge : { f : 'a key -> 'a option -> 'a option -> 'a option } -> t -> t -> t
  * S.union : { f : 'a key -> 'a -> 'a -> 'a option } -> t -> t -> t
  * new function S.map : { f : 'a key -> 'a -> 'a } -> t -> t
* Interface duplication for "bindings" and "value" were removed:
  S.findb, S.getb, S.addb, S.addb_unless_bound no longer exist,
  use S.find, S.get, S.add, S.add_unless_bound instead.
* The pretty-printer S.pp was removed, and K.pp is no longer required! S.pp is:
  let pp ppf = M.iter (fun (M.B (k, v)) -> Fmt.pf ppf (K.pp k) v)
* no more Fmt dependency
* added some initial tests

## 0.2.1 (2019-02-16)

* move build system to dune

## 0.2.0 (2018-06-24)

* New function `update`.
* New function `add_unless_bound` and `addb_unless_bound`.
* Replace `type v = V : 'a key * 'a -> v` by `type b = B : 'a key * 'a -> b`.
* Renamed functions ending with `v` to `b`

## 0.1.0 (2018-06-16)

* Initial release
