1.
```ocaml
  module MakeTree (Compare : CompareSig)
    : sig val empty : Compare.t tree end =
  struct
     open Compare
     type key = Compare.t
     type t = key tree
  
     class node x (l : t) (r : t) =
       object (self : 'self)
         val label = x
         val left = l
         val right = r
         method mem x =
           match compare x label with
              Smaller -> left#mem x
            | Larger -> right#mem x
            | Equal -> true
         method add x =
           match compare x label with
              Smaller -> {< left = left#add x >}
            | Larger -> {< right = right#add x >}
            | Equal -> self
     end;;
  
     class leaf =
       object (self : 'self)
         method mem _ = false
         method add x = new node x (self :> t) (self :> t)
       end;;
  
     let empty = new leaf;;
  end;;
```

