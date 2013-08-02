     
## Exercise
  The simulator in Exercise~\ref{exercise:circuit-simulation}
  has a problem with some cyclic circuits.  For example, the following circuit,
  called a \emph{ring oscillator}, oscillates indefinitely, overflowing the stack during simulation.
  
  \begin{center}
  \includegraphics[scale=0.5]{ring}
  \end{center}
  The simulation can be executed in constant stack space by implementing an \emph{event-driven
  simulator}.  In the circuit context, an \emph{event} occurs whenever the value on a terminal is set.
  An event-driven simulator uses a scheduler to manage events.  When the value of a terminal is set,
  the terminal is scheduled, but not executed yet.  When scheduled, the terminal is removed from the
  scheduling queue and executed.
  
  Define an event driven simulator by implementing a scheduler.  You can use a scheduling policy of
  your choice, but it should be fair, meaning that if a terminal is scheduled, it will eventually be
  executed.  
  
  The scheduler should include a method `main : unit` that runs until there are no more
  events (perhaps forever).  The type `terminal` should be defined as follows.
  The method `set` schedules the terminal, and the method `execute` executes it.
  
```ocaml
  type terminal = < set : bool -> unit; execute : unit >
```
  
