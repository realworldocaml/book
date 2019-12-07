
```ocaml version>=4.08
let (let*) a f = f a
let test ma f =
  let* s = ma in
  f s
```
