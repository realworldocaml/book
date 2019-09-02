# Using local library

```ocaml require-package=example_lib
# #require "example_lib"
# Example_lib.hello ()
Hello world!
- : unit = ()
```

# Sub library

```ocaml require-package=example_lib
# #require "example_lib.b"
# Lib_b.hello ()
Hello world!
- : unit = ()
```

# Executables

```sh require-package=example_lib
$ example_lib.exe Hello
Hello world!
```
