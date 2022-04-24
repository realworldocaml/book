
(rule
 (alias ctypes)
 (deps
  (source_tree ./correct/ctypes))
 (action
  (system "dune build @all @runtest --root ./correct/ctypes")))

(alias
 (name runtest)
 (deps (alias ctypes)))

(rule
 (alias ffi_datetime)
 (deps
  (source_tree ./correct/ffi_datetime)
  (package core)
  (package async)
  (package ctypes))
 (action
  (system "dune build @all @runtest --root ./correct/ffi_datetime")))

(alias
 (name runtest)
 (deps (alias ffi_datetime)))

(rule
 (alias ffi_hello)
 (deps
  (source_tree ./correct/ffi_hello)
  (package core)
  (package async)
  (package ctypes))
 (action
  (system "dune build @all @runtest --root ./correct/ffi_hello")))

(alias
 (name runtest)
 (deps (alias ffi_hello)))

(rule
 (alias ffi_ncurses)
 (deps
  (source_tree ./correct/ffi_ncurses)
  (package core)
  (package async)
  (package ctypes))
 (action
  (system "dune build @all @runtest --root ./correct/ffi_ncurses")))

(alias
 (name runtest)
 (deps (alias ffi_ncurses)))

(rule
 (alias ffi_ncurses_nointf)
 (deps
  (source_tree ./correct/ffi_ncurses_nointf)
  (package core)
  (package async)
  (package ctypes))
 (action
  (system "dune build @all @runtest --root ./correct/ffi_ncurses_nointf")))

(alias
 (name runtest)
 (deps (alias ffi_ncurses_nointf)))

(rule
 (alias ffi_qsort)
 (deps
  (source_tree ./correct/ffi_qsort)
  (package core)
  (package async)
  (package ctypes))
 (action
  (system "dune build @all @runtest --root ./correct/ffi_qsort")))

(alias
 (name runtest)
 (deps (alias ffi_qsort)))
