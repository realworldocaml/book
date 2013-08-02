1.
  Answer
```ocaml
  let empty = fun key -> 0
  let add dict key v = fun key' -> if key' = key then v else dict key
  let find dict key = dict key
```

1.
  Answer
  \begin{center}
  \begin{tabular}{rcl}
  `dict1 "x"` & = & `1`\\
  `dict2 "x"` & = & `1`\\
  `dict3 "x"` & = & `3`\\
  `dict4 "x"` & = & `3`\\
  `dict1 "y"` & = & `0`\\
  `dict2 "y"` & = & `2`\\
  `dict3 "y"` & = & `2`\\
  `dict4 "y"` & = & `4`
  \end{tabular}
  \end{center}

