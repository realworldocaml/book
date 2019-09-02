Standard Jane Street ppx rewriters
==================================

ppx\_jane is a [ppx_driver](https://github.com/janestreet/ppx_driver)
including all standard ppx rewriters.

Using ppx\_jane in the toplevel
-------------------------------

The default way is to do `#require "ppx_jane"` in the OCaml
toplevel. However this has been reported to be very slow. If this is
the case, try this instead:

```ocaml
#ppx "ppx-jane -as-ppx"
```

The main difference is that the first method will fire many different
executables per command, each one doing their own pass, while the
second will fire only one which efficiently combine all the
rewritings.
