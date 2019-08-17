Mdx can also understand ocaml code blocks:

```ocaml version=4.02
# #require "fmt";;
# let x = 3;;
val x : int = 3
# x + "foo";;
Characters 4-9:
Error: This expression has type bytes but an expression was expected of type
         int
```

```ocaml version=4.06
# #require "fmt";;
# let x = 3;;
val x : int = 3
# x + "foo";;
Characters 4-9:
Error: This expression has type string but an expression was expected of type
         int
```

```ocaml version=4.07
# #require "fmt";;
# let x = 3;;
val x : int = 3
# x + "foo";;
Characters 4-9:
Error: This expression has type string but an expression was expected of type
         int
```

```ocaml version>=4.08
# #require "fmt";;
# let x = 3;;
val x : int = 3
# x + "foo";;
Line 1, characters 5-10:
Error: This expression has type string but an expression was expected of type
         int
```
