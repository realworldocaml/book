## Title

### Prints to std.out are shown in output

```ocaml
let x = 1 in
Printf.printf "%d\n%!" x
```

```
assert false (* not an ocaml block, shouldn't be run *)
```

### Split definition and use

```ocaml
let x = "2"
```

With an intervening shell block:

```sh
 $ for i in `seq 1 10`; do echo $i; done
```


```ocaml
let () = print_endline x
```

## Top-level

```ocaml
   # print_endline "3";;
   3
```

