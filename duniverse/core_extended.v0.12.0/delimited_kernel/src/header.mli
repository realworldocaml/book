open Core_kernel

(** Control how headers are parsed

    - [`No]: File has no headers; columns can only be accessed by index.
    - [`Yes]: File has headers.
    - [`Require]: File has headers; assert that at least the given headers appear.
    - [`Replace]: File has headers, which are ignored; the given headers are used instead.
    - [`Add]: File has no headers. Use the given ones for header access.
    - [`Transform]: File has headers; each will be transformed with the given function.
    - [`Filter_map]: similar to [`Transform] but [None] headers are ignored.
*)
type t =
  [ `No
  | `Yes
  | `Require of string list
  | `Replace of string list
  | `Add of string list
  | `Transform of (string list -> string list) sexp_opaque
  | `Filter_map of (string list -> string option list) sexp_opaque ]
[@@deriving sexp_of]
