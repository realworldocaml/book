1.
```ocaml
  let printline (x, y, z) =
     printf "%-5s 0x%08x %3s\n" x y z
```
  
  Here are some examples.
```ocaml
  # printline ("A", 10, "B");;
  @
  \begin{topoutput}
  A     0x0000000a   B
  \end{topoutput}
  @
  # printline ("abcdefg", 255, "hijk");;
  @
  \begin{topoutput}
  abcdefg 0x000000ff hijk
  \end{topoutput}
  @
```

