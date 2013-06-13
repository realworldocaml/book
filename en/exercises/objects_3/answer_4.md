1.
  Inheriting from the `cell` twice would override the
  methods `get` and `set`, which is not what we
  want.  We need to \emph{rename} the methods first.  For this list, it
  is sufficient to rename the methods in just one of the classes.  Note
  that the cell value is hidden so that it is not overridden.
  
```ocaml
  module IntCell =
  struct
     module Cell = MakeCell (struct type t = int end);;
  
     class type cell_type =
     object
        method private get_int : int
        method private set_int : int -> unit
     end
  
     class cell i : cell_type =
     object (self)
        inherit Cell.cell i
        method private get_int = self#get
        method private set_int = self#set
     end
  end
  
  module ListCell = MakeCell (struct type t = int_list end);;
  
  class cons i : int_cons =
  object
      inherit IntCell.cell i as value
      inherit ListCell.cell None as link
  
      method hd = value#get_int
      method tl = link#get
      method set_hd = value#set_int
      method set_tl = link#set
  end
```

