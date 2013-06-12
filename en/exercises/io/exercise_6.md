  
## Exercise
  You are given an input file \lstinline+data.txt+ containing
  lines that begin with a single digit \lstinline+1+
  or \lstinline+2+.  Write a function using
  the \lstinline+Buffer+ module to print the file, without
  leading digits, in de-interleaved form.
  
  \begin{center}
  \begin{tabular}{lcl}
  \lstinline+data.txt+ & $\longrightarrow$ & output\\
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
  
