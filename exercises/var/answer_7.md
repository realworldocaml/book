1.
  For efficiency, we should precompute as much as possible.
  
```ocaml
  let r b c =
     let minusb = -. b in
     let bsquared = b *. b in
     let fourc = -4.0 *. c in
        fun a -> (minusb +. sqrt (bsquared -. fourc *. a)) /. (2.0 *. a)
```

