1.
  There can be a problem if the `src` and `dst` arrays are the same,
  and the ranges to be copied overlap, and `dst_off > src_off`.
  
  For example, the following expression duplicates the first element of the array
  instead of copying a subrange.
  
```ocaml
  let data = [|1; 2; 3; 4; 5; 6; 7; 8; 9|];;
  @
  \begin{topoutput}
  val data : int array = [|1; 2; 3; 4; 5; 6; 7; 8; 9|]
  \end{topoutput}
  @
  # blit data 0 data 1 5;;
  @
  \begin{topoutput}
  - : unit = ()
  \end{topoutput}
  @
  # data;;
  @
  \begin{topoutput}
  - : int array = [|1; 1; 1; 1; 1; 1; 7; 8; 9|]
  \end{topoutput}
  @
```
  An easy solution is to copy in reverse direction when `dst_off > src_off`.
  
```ocaml
  let blit src src_off dst dst_off len =
     if dst_off < src_off then
        for i = 0 to len - 1 do
           dst.(dst_off + i) <- src.(src_off + i)
        done
     else
        for i = len - 1 downto 0 do
           dst.(dst_off + i) <- src.(src_off + i)
        done
```

