```ocaml


let x =

  5

```

```ocaml

# let x = 1

  in x
- : int = 1


# 3
- : int = 3


# Printf.printf "foo\n\nbar\n"
foo

bar
- : unit = ()
```

```sh
$ ocaml-mdx pp spaces.md
#2 "spaces.md"


let x =

  5

;;
#12 "spaces.md"
  let x = 1
  
  in x
#18 "spaces.md"
  3
#22 "spaces.md"
  Printf.printf "foo\n\nbar\n"
;;
```
