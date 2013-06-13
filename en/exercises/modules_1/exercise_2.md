  
## Exercise
  Consider the following two versions of a list reversal function.
  
  \begin{center}
  \begin{tabular}{ll}
  \begin{tabular}{l}
  rev.mli\\
  \hline
```ocaml
  val rev : 'a list -> 'a list
```
  \end{tabular}
  \\
  \\
  \begin{tabular}[t]{l}
  rev.ml (version 1)\\
  \hline
  \begin{minipage}{2in}
```ocaml
  let rev l =
     let rec rev_loop l1 l2 =
        match l2 with
           x :: l2 ->
              loop (x :: l1) l2
         | [] ->
              l1
     in
        rev_loop [] l
```
  \end{minipage}
  \end{tabular}
  &
  \begin{tabular}[t]{l}
  rev.ml (version 2)\\
  \hline
  \begin{minipage}{2in}
```ocaml
  let rec rev_loop l1 l2 =
     match l2 with
        x :: l2 ->
           loop (x :: l1) l2
      | [] ->
           l1
  
  let rev l = rev_loop [] l
```
  \end{minipage}
  \end{tabular}
  \end{tabular}
  \end{center}
1. Is there any reason to prefer one version over the other?
1.
  In the second version, what would happen if we defined
  the `rev` function as a partial application?
```ocaml
  (* let rev l = rev_loop [] l *)
  let rev = rev_loop []
```
  
