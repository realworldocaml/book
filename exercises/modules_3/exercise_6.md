  
## Exercise
  \index{pipelines}
  In Unix-style systems\footnote{UNIX\textregistered{} is a registered
  trademark of The Open Group.} a \emph{pipeline} is a series of
  processes `$p_1$ | $p_2$ | $\cdots$ | $p_n$` that
  interact through communication channels, where the input of process
  $p_{i + 1}$ is the output of process $p_i$.
  
  We can use a similar architecture within a program to connect modules,
  which we will call \emph{filters}, giving them the
  signature \lstinline$Filter$.  The pipeline itself is given the
  signature \lstinline$Pipeline$, where the type of elements
  passed into the pipeline have type \lstinline$Pipeline.t$.
  
```ocaml
  module type Pipeline = sig
     type t
     val f : t -> unit
  end
  
  module type Filter = functor (P : Pipeline) -> Pipeline
```
  For example, the following pipeline \lstinline$CatFile$
  prints the contents of a file to the terminal, one line at a time.
  
```ocaml
  module Print = struct
     type t = string
     let f s = print_string s; print_char '\n'
  end
  
  module Cat (Stdout : Pipeline with type t = string) =
  struct
     type t = string
  
     let f filename =
        let fin = open_in filename in
        try
           while true do Stdout.f (input_line fin) done
        with End_of_file -> close_in fin
  end
  
  module CatFile = Cat (Print)
```
  
1.
  
  Write a \lstinline$Uniq$ filter that, given a sequence of input
  lines, discards lines that are equal to their immediate predecessors.
  All other lines should be passed to the output.
  
1.
  
  Write a \lstinline$Grep$ filter that, given a regular
  expression and a sequence of input lines, outputs only those lines
  that match the regular expression.  For regular expression matching,
  you can use the \lstinline$Str$ library.  The function
  \lstinline$Str.regexp : string -> regexp$
  compiles a regular expression presented as a string; the
  expression \lstinline$Str.string_match r s 0$ tests whether a
  string $s$ matches a regular expression $r$.
  
1.
  
  Write a function \lstinline$grep : string -> string -> unit$,
  where the expression \lstinline$grep regex filename$ prints the
  lines of the file `filename` that match the pattern specified by
  the string `regex`, using the pipeline construction and the
  module \lstinline$Grep$ from the previous part.
  
1.
  
  Sometimes it is more convenient for filters to operate over individual
  characters instead of strings.  For example, the following filter
  translates a character stream to lowercase.
  
```ocaml
  module Lowercase (Stdout with type t = char) =
  struct
     type t = char
     let f c = Stdout.f (Char.lowercase c)
  end
```
  Write a filter \lstinline$StringOfChar$ that converts a
  character-based pipeline to a string-based pipeline.
```ocaml
  StringOfChar : functor (P : Pipeline with type t = char) ->
     Pipeline with type t = string
```
  
1.
  
  The pipeline signatures, as defined, seem to require that pipelines be
  constructed from the end toward the beginning, as a module expression
  of the form `P1 (P2 $\cdots$ (Pn) $\cdots$)`.  Write
  a functor \lstinline$Compose$ that takes two filters and
  produces a new one that passes the output of the first to the second.
  What is the signature of the \lstinline$Compose$ functor?
  (Hint: consider redefining the signature for filters.)
  
