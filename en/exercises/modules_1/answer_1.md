1.
1.
  
  It is always legal for a `.mli` file to be empty.
  However, this hides all definitions in the `.ml`
  file, so it has limited usefulness.
  
1.
  
  The specification `val f : 'a -> 'a` is legal.
  
1.
  
  The specification `val f : ('a -> 'b) -> ('a -> 'b)`
  is also legal (it is just a refinement of the type
  `'a -> 'a`).
  
1. 
  
  The specification `val f : t -> t` is not legal
  because there is no definition for the type `t`.
  
1.
  
  The specification `type t val f : t -> t` is legal.
  
1.
  
  The specification `type s = int val f : s -> s` is
  not legal because the type `s` must also be defined
  in the `.ml` file.

