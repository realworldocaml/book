1.
  The differences between the two include the following.
  \begin{itemize}
1.
  
  A `.mli` file contains only declarations and type
  definitions.  If the file contains a expression definition
  (like `let zero = 0`), then it must be placed in
  a `.ml` file.
  
1.
  
  Components with only a `.mli` file need not be
  specified during linking.  This is a slight benefit, but it can
  slightly simplify program construction in some cases.
  \end{itemize}

