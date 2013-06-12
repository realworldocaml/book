  
## Exercise
  One issue we discussed was the need for duplicate type definitions.
  If a \lstinline+.mli+ provides a definition for a type \lstinline+t+, the the
  \lstinline+.ml+ file must specify exactly the same definition.  This can be
  annoying if the type definition is to be changed.
  
  One solution we discussed is to place the type definition in a
  separate file, like \lstinline+types.ml+, with no interface file
  \lstinline+types.mli+.  It is also legal to place the type definition in a
  file \lstinline+types.mli+ with no implementation \lstinline+types.ml+.
  
  Is it ever preferable to use the second form (where \lstinline+types.mli+
  exists, but \lstinline+types.ml+ doesn't)?
  
