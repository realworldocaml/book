## Title

Only blocks that pass the filter 'Testing' should be printed

### Hello

```ocaml
let x = 42 in
Printf.printf "%d\n%!" x
```

```
raw contents
```

### Toto

```ocaml
let x = 2
```

### Testing cram

```sh
 $ for i in `seq 1 10`; do echo $i; done
 1
 2
 3
 4
 5
 6
 7
 8
 9
 10
```

### Not testing cram

```sh
$ ls
foo bar
```

```ocaml
let () = print_endline (string_of_int x)
```

## Toplevel

```ocaml
   # print_endline "42";;
   42
```

