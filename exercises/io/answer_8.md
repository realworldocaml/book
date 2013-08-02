1.
  This seems like it would be straightforward
  with `printf`, we would just calculate the width of
  the first column, and produce the right format string.  For example,
  if the variable `words` contains the word list, and
  the width of the first column is computed to be 20 characters, we
  would use the following printer.
  
```ocaml
  List.iter (fun (w1, w2) ->
     printf "%-20s %s\n" w1 w2) words
```
  However, this doesn't work in general because the format string must
  be computed, and `printf` requires a literal string.
  
```ocaml
  # let fmt = sprintf "%%-%ds %%s\n" 20;;
  @
  \begin{topoutput}
  val fmt : string = "%-20s %s\n"
  \end{topoutput}
  @
  # List.iter (fun (w1, w2) ->
       printf fmt w1 w2) words;;
  @
  \begin{toperror}
  Characters 37-40:
       printf fmt w1 w2) words;;
  This expression has type string but is here used with type
    ('a -> 'b -> 'c, out_channel, unit) format
  \end{toperror}
  @
```
  Instead, we can define a ``padding'' function,
  and use it to justify the output, using the `%a`
  format specifier.
  
```ocaml
  let pad ochan i =
     for j = 1 to i do
        output_char ochan ' '
     done
  
  let print_cols l =
     let width =
        List.fold_left (fun width (s, _) ->
            max width (String.length s)) 0 l
     in
        List.iter (fun (s1, s2) ->
           printf "%s%a %s\n" s1
              print_pad (width - String.length s1) s2) l
```
  As it turns out, there \emph{is} a way to
  use `printf` directly.  If the width specifier is the
  character `*`, then `printf` expects
  the width specifier to be passed as an argument.
  
```ocaml
  let print_cols l =
     let width =
        List.fold_left (fun width (s, _) ->
            max width (String.length s)) 0 l
     in
        List.iter (fun (s1, s2) ->
           printf "%-*s %s\n" width s1 s2) l
```

