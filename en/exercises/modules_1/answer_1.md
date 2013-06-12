1.
1.
  
  It is always legal for a \lstinline+.mli+ file to be empty.
  However, this hides all definitions in the \lstinline+.ml+
  file, so it has limited usefulness.
  
1.
  
  The specification \lstinline+val f : 'a -> 'a+ is legal.
  
1.
  
  The specification \lstinline+val f : ('a -> 'b) -> ('a -> 'b)+
  is also legal (it is just a refinement of the type
  \lstinline+'a -> 'a+).
  
1. 
  
  The specification \lstinline+val f : t -> t+ is not legal
  because there is no definition for the type \lstinline+t+.
  
1.
  
  The specification \lstinline+type t val f : t -> t+ is legal.
  
1.
  
  The specification \lstinline+type s = int val f : s -> s+ is
  not legal because the type \lstinline+s+ must also be defined
  in the \lstinline+.ml+ file.

