1.
  The important part is that the forcing is memoized.  We can use a reference cell to save to computation.
  
```ocaml
  type 'a deferred_value =
     Deferred of (unit -> 'a)
   | Forced of 'a
  
  type 'a deferred = 'a deferred_value ref
  
  let defer f = ref (Deferred f)
  
  let force cell =
     match !cell with
        Deferred f ->
           let result = f () in
           cell := Forced result;
           result
      | Forced result ->
           result
```

