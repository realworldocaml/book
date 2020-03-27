It is possible to use ellipsis (`...`) in the error blocks attached to OCaml blocks, here it is useful as the error message depends on the OCaml version:

```ocaml
module Counter: Irmin.Contents.S with type t = int64 = struct
  type t = int64
  let t = Irmin.Type.int64
```
```mdx-error
...
Error: Syntax error: 'end' expected
...
```

```ocaml
module Counter: Irmin.Contents.S with type t = int64 = struct
  type t = int64
  let t = Irmin.Type.int64
end
```
```mdx-error
...
Error: Unbound module Irmin
```
