1.
  The operations are implemented with operations on records.
  
```ocaml
  let ref x = { contents = x }
  let (!) cell = cell.contents
  let (:=) cell x = cell.contents <- x
```

