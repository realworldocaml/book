  
## Exercise
  The strict-ordering requirement during linking can potentially have a
  major effect on the software design.  For example, suppose we were
  designing a bi-directional communication protocol, as shown in the
  following diagram.
  
  \begin{center}
  \includegraphics{network_stack}
  \end{center}
  With this design, the `Network` component
  calls `Protocol.net_recv` when a message arrives, and
  the `Protocol` component
  calls `Network.net_send` to send a message.  However,
  this is not possible if the implementations are in separate
  files `protocol.ml` and `network.ml`
  because that would introduce a cyclic dependency.
  
  Describe a method to circumvent this problem, without placing the code
  for the two components into a single file.
  
