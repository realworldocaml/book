1.
  There are a few potential solutions, including the use of recursive
  modules (discussed in Section~\ref{section:recursive-modules}) or
  objects (Chapter~\ref{chapter:objects}).  In addition here are a few
  alternate ways.
  
1.
  
  If the design can be partitioned into two separate message streams
  without any cycles, the design can be programmed directly.
  
  \begin{center}
  \includegraphics{network_stack2}
  \end{center}
  However, this approach may not be possible, and the changes required
  may be unnatural.
  
1.
  
  A simpler solution is to introduce a third file containing references
  that can be set to refer each of the functions.  This solution, while
  lacking elegance, is straightforward.
  
  \begin{center}
  \begin{tabular}{l}
  refs.mli\\
  \hline
  \begin{minipage}{5in}
```ocaml
  val net_recv_ref : (message -> unit) ref
  val net_send_ref : (message -> unit) ref
  val app_recv_ref : (message -> unit) ref
  val app_send_ref : (message -> unit) ref
```
  \end{minipage}
  \end{tabular}
  \end{center}
  The cells are initialized with dummy values.
```ocaml
  let net_recv_ref = ref (fun _ -> raise (Invalid_argument "not initialized"))
  ...
```
  At startup time, the layers set the references to refer to the
  appropriate functions.
  
  \begin{center}
  \begin{tabular}{l}
  protocol.ml\\
  \hline
  \begin{minipage}{3in}
```ocaml
  let net_recv msg = ...
  ...
  Refs.net_recv_ref := net_recv
```
  \end{minipage}
  \end{tabular}
  \end{center}
  
1.
  
  Yet another option is to define a third file,
  say `link.ml` that builds the recursive definition in
  a single file.  Each of the layer functions would take as arguments
  the functions that it expects to call.
  
```ocaml
  let rec net_recv msg =
     Protocol.net_recv net_send app_recv msg
  and app_send msg =
     Protocol.app_send net_send app_recv msg
  and net_send msg =
     Network.net_send net_recv msg
  and app_recv msg =
     Application.app_recv app_send msg
```
  This solution is more complicated and may be slightly less efficient
  than the solution using reference cells.
  

