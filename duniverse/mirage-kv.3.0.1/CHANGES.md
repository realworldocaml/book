## v3.0.1 (2019-11-04)

* provide deprecated Mirage_kv_lwt for smooth transition (#21 @hannesm)

### v3.0.0 (2019-10-22)

* remove mirage-kv-lwt (#19 @hannesm)
* specialise mirage-kv on Lwt.t and value being string (#19 @hannesm)
* raise lower OCaml bound to 4.06.0 (#19 @hannesm)

### v2.0.0 (2019-02-24)

* Major revision of the `RO` signature:
 - values are of type `string`
 - keys are segments instead of a string
 - `read` is now named `get`, and does no longer take an offset and length
 - the new function `list` is provided
 - the new functions `last_modified` and `digest` are provided
* A module `Key` is provided with convenience functions to build keys
* An `RW` signature is provided, extending `RO` with
 - a function `set` to replace a value
 - a function `remove` to remove a key
 - `batch` to batch operations

### v1.1.1 (2017-06-29)

* Remove `open Result` statements (and drop support to 4.02)

### v1.1.0 (2017-05-26)

* Port to Jbuilder.

### v1.0.0 (2016-12-27)

* First release, import `V1.KV_RO` and `V1_LWT.KV_RO` from mirage-types.
