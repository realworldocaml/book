  
## Exercise
  You are given an input file `data.txt` containing
  lines that begin with a single digit `1`
  or `2`.  Write a function using
  the `Buffer` module to print the file, without
  leading digits, in de-interleaved form.
  
  \begin{center}
  \begin{tabular}{lcl}
  `data.txt` & $\longrightarrow$ & output\\
  \hline\\
  \begin{minipage}{2in}
```ocaml
  2Is
  1This
  2File
  1A
```
  \end{minipage}
  &
  &
  \begin{minipage}{2in}
```ocaml
  This
  Is
  A
  File
```
  \end{minipage}
  \end{tabular}
  \end{center}
  For example, given the input on the left, your program should produce
  the output on the right.
  
