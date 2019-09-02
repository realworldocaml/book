#require directives work as expected:

```ocaml
# #require "lwt"
# let x = Lwt.return 3
val x : int Lwt.t = <abstr>
```

Top-level `Lwt` values are automatically evaluated with `Lwt_main.run`:

```ocaml
# Lwt.return 4
- : int = 4
# Lwt_list.map_p Lwt.return [1;2;3]
- : int list = [1; 2; 3]
```
