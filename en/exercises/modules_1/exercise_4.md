  
## Exercise
  One issue we discussed was the need for duplicate type definitions.
  If a `.mli` provides a definition for a type `t`, the the
  `.ml` file must specify exactly the same definition.  This can be
  annoying if the type definition is to be changed.
  
  One solution we discussed is to place the type definition in a
  separate file, like `types.ml`, with no interface file
  `types.mli`.  It is also legal to place the type definition in a
  file `types.mli` with no implementation `types.ml`.
  
  Is it ever preferable to use the second form (where `types.mli`
  exists, but `types.ml` doesn't)?
  
