Arbitrary padding is allowed, as long as it is consistent inside a code block.

```sh
   $ echo foo
   foo
```

```ocaml
     # let x = 3;;
     val x : int = 3
```

```sh
$ ocaml -warn-help | egrep '\b9 Missing\b'
  9 Missing fields in a record pattern.
```
