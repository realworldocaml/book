1.
  The differences between the two include the following.
  \begin{itemize}
1.
  
  A \lstinline+.mli+ file contains only declarations and type
  definitions.  If the file contains a expression definition
  (like \lstinline+let zero = 0+), then it must be placed in
  a \lstinline+.ml+ file.
  
1.
  
  Components with only a \lstinline+.mli+ file need not be
  specified during linking.  This is a slight benefit, but it can
  slightly simplify program construction in some cases.
  \end{itemize}

