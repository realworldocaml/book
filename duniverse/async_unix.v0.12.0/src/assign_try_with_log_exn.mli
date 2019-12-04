(*_ This module is internal to Async.  It is [include]d in [std.ml] so that any code that
  uses [Async_unix] does the top-level side effect in this module to assign
  [Async_kernel.Monitor0.try_with_log_exn]. *)
