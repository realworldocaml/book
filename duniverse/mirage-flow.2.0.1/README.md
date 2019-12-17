## mirag-flow -- Flow implementations and combinators for MirageOS

This repo contains generic operations over Mirage `FLOW` implementations.

Please consult [the API documentation](https://mirage.github.io/mirage-flow/index.html).

### Example usage

In a top-level like utop:
```ocaml
# #require "mirage-flow";;
# #require "mirage-clock-unix";;
# #require "lwt.syntax";;

# let a = Mirage_flow.Fun.(make ~input:(input_string "hellooooo") ());;
val a : Mirage_flow.Fun.flow = <abstr>

# let buffer = String.make 20 ' ';;
val buffer : bytes = "                    "
# let b = Mirage_flow.Fun.(make ~output:(output_string buffer) ());;
val b : Mirage_flow.Fun.flow = <abstr>

# lwt results = Mirage_flow.copy (module Clock) (module Mirage_flow.Fun) a (module Mirage_flow.Fun) b ();;
val results : [ `Error of [ `Msg of bytes ] | `Ok of Mirage_flow.CopyStats.t ] =  `Ok {Mirage_flow.CopyStats.read_bytes = 9L; read_ops = 1L; write_bytes = 9L; write_ops = 1L; duration = 6.9141387939453125e-06}
# buffer;;
- : bytes = "hellooooo           "
```
