## 113.00.00

- Added `Fields.Direct.set_all_mutable_fields`, a function intended to
  guarantee when pooling records that one cannot forget to reinitialize some
  fields.

    Obviously one could achieve this through something like
    `Fields.Direct.iter`, but we want a more efficient version that
    doesn't force the call side to create closures.

## 109.19.00

- Made `with fields` generate the same functions in the `Fields` and
  `Fields_of_*` modules whether the type is called `t` or not.

## 109.14.00

- Made `with fields` expose first-class fields for private types while
  preserving privacy.

    There is now an additional phantom type in a first-class field that
    prevents building or modifying elements of a private type.

    One consequence of this change is that the `Field.t` type is now an
    abstract type -- it used to be exposed as a record type.  So, one
    must, e.g., change `field.Field.name` to `Field.name field`.

## 109.12.00

- Added back `Fields.fold` to `with fields` for `private` types.

    We had removed `Fields.fold` for `private` types, but this caused
    some pain.  So we're putting it back.  At some point, we'll patch
    `with fields` to prevent setting mutable fields on private types via
    the fields provided by `fold`.

## 109.11.00

- `with fields`, for a type `u` that isn't named `t`, creates module
  `Fields_of_u` rather than module `Fields`.  This allows one to us
  `with fields` on several types in the same structure.

## 109.10.00

- Changed `with fields` on `private` types to not expose mutators or
  creators.

