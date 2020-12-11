# Changes

## 2020/04/10

* New function `PPrint.utf8format`.

## 2020/03/16

* New functions `PPrint.OCaml.flowing_list` and `PPrint.OCaml.flowing_array`.

## 2020/02/26

* Change the behavior of `PPrint.ToFormatter` to use `Format.pp_print_text`
  internally. This means that a newline character causes a call to
  `Format.pp_force_newline`; a space character causes a call to
  `Format.pp_print_space`; and every other character is printed using
  `Format.pp_print_char`.

* Switch to `dune`.

* Avoid a few compilation warnings.

## 2018/05/23

* Add a `line` field to the `state` record, which can be read by the code
  that implements custom documents. Add a `range` combinator that allows
  retrieving the start and end points of a (sub)document in the output.
  (Suggested by Victor Gomes.)

## 2017/10/03

* Update the code and build options to use `-safe-string`. This means that
  the library now requires OCaml 4.02 or later, and is compatible with 4.06.

## 2015/03/16

* Moved to github and changed the license to LGPL with an exception.

## 2014/04/25

* Minor changes in the implementation of `string` and `substring`.
  Initially committed on 2014/03/24, but left out of the 20140424
  release due to a goof-up.

## 2014/04/11

* Changed the behavior of `align`, which was not consistent with its
  documentation. `align` now sets the indentation level to the current column.
  In particular, this means that `align (align d)` is equivalent to `align d`,
  which was not the case previously. Thanks to Dmitry Grebeniuk for reporting
  this issue.

## 2014/04/03

* The library is now extensible (in principle). A `custom` document
  constructor allows the user to define her own documents, as long as they fit
  the manner in which the current rendering engine works.

* The `compact` rendering engine is now tail-recursive too.

## 2014/03/21

* Minor optimisation in the smart constructor `group`.

## 2014/03/13

* New (simpler) pretty-printing engine. The representation of documents in
  memory is slightly larger; document construction is perhaps slightly slower,
  while rendering is significantly faster. (Construction dominates rendering.)
  The rendering speed is now guaranteed to be independent of the width
  parameter. The price to pay for this simplification is that the primitive
  document constructors `column` and `nesting` are no longer supported. The
  API is otherwise unchanged.

## 2013/01/31

* First official release of PPrint.
