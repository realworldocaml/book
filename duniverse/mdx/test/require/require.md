# Using local library

```ocaml
# #require "example_lib";;
# Example_lib.hello ()
Hello world!
- : unit = ()
```

# Sub library

```ocaml
# #require "example_lib.b";;
# Lib_b.hello ()
Hello world!
- : unit = ()
```
