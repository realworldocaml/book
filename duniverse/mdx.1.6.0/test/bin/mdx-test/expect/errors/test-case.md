Errors should be well localized:

```ocaml version<4.08
# class ['a] stack init = object
    val mutable v = init

    method pop =
      match v with
      | hd :: tl ->
        v <- tl;
        Some hd
      | [] -> None

    method push hd =
      v <- hd :: v
  end;;
Characters 0-215:
Error: Some type variables are unbound in this type:
         class ['a] stack :
           'b list ->
           object
             val mutable v : 'b list
             method pop : 'b option
             method push : 'b -> unit
           end
       The method pop has type 'b option where 'b is unbound
```

Hi!


```ocaml version=4.02
# let x =
  1 + "42"
Characters 14-18:
Error: This expression has type bytes but an expression was expected of type
         int
```

```ocaml version=4.06
# let x =
  1 + "42"
Characters 14-18:
Error: This expression has type string but an expression was expected of type
         int
```

```ocaml version=4.07
# let x =
  1 + "42"
Characters 14-18:
Error: This expression has type string but an expression was expected of type
         int
```

```ocaml version>=4.08
# let x =
  1 + "42"
Line 2, characters 7-11:
Error: This expression has type string but an expression was expected of type
         int
```

```ocaml non-deterministic=output
# raise Not_found
Exception: Not_found.
```
