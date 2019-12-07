How to link against these bindings
==================================

We export a library Re2 with one module Regex which binds the Google re2 regex
library.  Binaries which link to the OCaml Re2 library get the underlying
Google library and these bindings.

The underlying re2 sources updated 18 March 2015 (rev 3d5f1714e63f).
