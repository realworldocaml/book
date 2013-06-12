1.
1.
  
  For the \lstinline$Uniq$ filter, we can use a reference cell to
  keep track of lines as they are read.  We ``cheat'' in this
  solution---since we know the input never contains a newline, we
  initialize the \misspelled{refcell} to an impossible value.  A better solution
  would be for the cell to be initialized to \lstinline$None$
  (and thus be of type \lstinline$string option$).
  
```ocaml
  module Uniq (Stdout : Pipeline with type t = string)
   : Pipeline with type t = string =
  struct
     type t = string
     let last_line = ref "\n"
     let f s =
        if s <> !last_line then
           Stdout.f s;
        last_line := s
  end
```
  
1.
  
  The main problem in defining the \lstinline$Grep$ filter is
  that it takes a regular expression as an argument.  It is possible to
  pass the regular expression in its own module, but this will mean that
  the \lstinline$Grep$ module works for only one regular
  expression.  (The next part illustrates another solution to this
  problem.)
  
```ocaml
  module Grep
   (R : sig val regex : string end)
   (P : Pipeline with type t = string) =
  struct
     type t = string
     let regexp = Str.regexp R.regex
     let f s =
        if Str.string_match regexp s 0 then
           P.f s
  end
```
  
1.
  
  One easy way to define the \lstinline$grep$ function is to use
  a \lstinline$let module$ construction to define the pipeline
  within the function body.
  
```ocaml
  let grep regex filename =
     let module P = Cat (Grep (struct let regex = regex end) (Print)) in
     P.f filename
```
  
1.
  
  The \lstinline$StringOfChar$ module simply iterates through each
  character of the input string.
  
```ocaml
  module StringOfChar (P : Pipeline with type t = char)
   : Pipeline with type t = string =
  struct
     type t = string
     let f s = String.iter P.f s
  end
```
  
1.
  
  The \lstinline$Compose$ functor takes three arguments, the
  first filter \lstinline$F1$, the second
  filter \lstinline$F2$, and the rest of the
  pipeline \lstinline$P3$, where 
  \lstinline$Compose (F1) (F2) (P3) = F1 (F2 (P3))$.
  Thus, a partial application \lstinline$Compose (F1) (F2)$ will
  yield a filter.
  
  The main issue with constructing the filter is with the constraints
  about compatibility of the filters' types.  These sharing constraints
  are not obvious.  Suppose filter \lstinline$F1$ takes values of
  type $t_1$, filter \lstinline$F2$ takes values of type $t_2$,
  and the rest of the pipeline \lstinline$P3$ takes values of
  type $t_3$.  For illustration, we would have the following
  constraints.
  
```ocaml
  module Compose
   (F1 : (P : Pipeline with type t = $t_2$) -> Pipeline with type t = $t_1$)
   (F2 : (P : Pipeline with type t = $t_3$) -> Pipeline with type t = $t_2$)
   (P3 : Pipeline with type t = $t_3$) = F1 (F2 (P3))
```
  The proper types are then as follows $t_3
  = \hbox{`P3.t`}$, $t_2
  = \lstinline{F2(P3).t}$, and $t_1
  = \lstinline{F1(F2(P3)).t}$.  However, using these definitions
  directly is not possible because it would create forward references in
  the type definition.  For example, the signature
  for \lstinline$F1$ would refer forward to the
  modules \lstinline$F2$ and \lstinline$P3$.  We could
  reorder the arguments to eliminate the forward references, but then
  the partial application would not work as we wish.
  
  Arguably, the best solution is to redefine the signature for filters so
  that they specify the types for both input and output.
  
```ocaml
  module type Filter = sig
     type t_in
     type t_out
     module F : functor (P : Pipeline with type t = t_out) ->
        Pipeline with type t = t_in
  end
```
  The filters themselves are not much different.  For example, here is
  the filter \lstinline$StringOfChar$.
  
```ocaml
  module StringOfChar
   : Filter with type t_in = string and type t_out = char =
  struct
     type t_in = string
     type t_out = char
     module F (X : Pipeline with type t = char) = struct
        type t = string
        let f s = String.iter X.f s
     end
  end
```
  The sharing constraints for the \lstinline$Compose$ functor are
  now easy to express.
  
```ocaml
  module Compose
   (F1 : Filter)
   (F2 : Filter with type t_in = F1.t_out)
   : Filter with type t_in = F1.t_in and type t_out = F2.t_out =
  struct
     type t_in = F1.t_in
     type t_out = F2.t_out
     module F (P3 : Pipeline with type t = t_out) = struct
        module Pipe = F1.F (F2.F (P3))
        type t = t_in
        let f = Pipe.f
     end
  end
```
  The signature can be represented as follows.
  
```ocaml
  Compose : functor (F1 : Filter) ->
    functor (F2 : Filter with type t_in = F1.t_out) ->
    Filter with type t_in = F1.t_in and type t_out = F2.t_out
```

