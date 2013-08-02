  
## Exercise
  What problem might arise with the following implementation of an array blit function?
  How can it be fixed?
  
```ocaml
  let blit src src_off dst dst_off len =
     for i = 0 to len - 1 do
        dst.(dst_off + i) <- src.(src_off + i)
     done
```
  
