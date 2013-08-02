  
## Exercise
  Object-oriented programming originated in the Simula, a language designed by Dahl
  and Nygaard~\cite{ND81} for the purpose of simulation.  In this exercise, we'll build a simple circuit simulator
  using objects in OCaml.
  
  A logic circuit is constructed from \emph{gates} and \emph{wires}.  A gate has one or more inputs and an
  output that is a computed as a Boolean function of the inputs.  A wire connects the output of a gate
  to one or more input \emph{terminals}, where a terminal has a method `set : bool -> unit` to
  set the value of the terminal.  Here are the definitions of the classes `terminal` and `wire`.
  
```ocaml
  type terminal = < set : bool -> unit >
  
  class wire =
  object
     val mutable terminals : terminal list = []
     val mutable value = false
     method add_terminal t = terminals <- t :: terminals
     method set x =
        if x <> value then (value <- x; List.iter (fun t -> t#set x) terminals)
  end
  
  let dummy_wire = new wire
```
  There are many kinds of gates, so we'll build an inheritance hierarchy.
  A generic gate has a single output, connected to a wire.  It also has
  a virtual method `compute_value` that defines the function
  computed by the gate.
  
```ocaml
  class virtual gate =
  object (self : 'self)
     val mutable output_wire = dummy_wire
     method connect_output wire = output_wire <- wire
     method private set_output =  output_wire#set self#compute_value
     method private virtual compute_value : unit -> bool
  end
```
  A `two_input_gate` is a gate that has two inputs.
  
```ocaml
  class virtual two_input_gate =
  object (self : 'self)
     inherit gate
     val mutable a = false
     val mutable b = false
     method private set_input_a x = a <- x; self#set_output
     method private set_input_b x = b <- x; self#set_output
     method connect_input_a wire = $\cdots$
     method connect_input_b wire = $\cdots$
  end
```
  With the boilerplate defined, we can build some standard gates.
  
  \begin{center}
  \begin{tabular}{ll}
  \includegraphics[scale=0.5]{nand2}
  &
  \includegraphics[scale=0.5]{nor2}
  \\
  \begin{minipage}[b]{2.5in}
```ocaml
  class nand2 =
  object
     inherit two_input_gate
     method compute_value = not (a && b)
  end
```
  \end{minipage}
  &
  \begin{minipage}[b]{2.5in}
```ocaml
  class nor2 =
  object
     inherit two_input_gate
     method compute_value = not (a || b)
  end
```
  \end{minipage}
  \end{tabular}
  \end{center}
1. 
  Fill in the definitions of the methods `connect_input_a` and `connect_input_b`.
  
  
1.
  Define a class `three_input_gate` (for gates with three inputs) by inheriting
  from `two_input_gate`.
     
1.
  Would the definition be simpler if the type `terminal` were a function instead of an
  object (where `type terminal = bool -> unit`)?
  
  
1.
  What is the purpose of the conditional `if x <> value then $\cdots$` in the class `wire`?
  
  
1.
  Write a program for the following circuit, called a \emph{SR latch}.
  \begin{center}
  \begin{tabular}{cc}
  \begin{tabular}[b]{cc|c}
  S & R & Action\\
  \hline
  0 & 0 & Keep state\\
  0 & 1 & $Q = 0$\\
  1 & 0 & $Q = 1$\\
  1 & 1 & $Q = 0, \overline{Q} = 0$\\
  \\
  \end{tabular}
  &
  \includegraphics[scale=0.5]{latch}
  \end{tabular}
  \end{center}
  
