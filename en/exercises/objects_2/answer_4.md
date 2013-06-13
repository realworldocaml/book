1.
  The class `nil` returns `true` for the method `is_nil`,
  and it raises an exception on all other operations.
  
```ocaml
  class nil : int_list =
  object (_ : #int_list as 'self)
     method is_nil = true
     method hd = raise (Invalid_argument "hd")
     method tl = raise (Invalid_argument "tl")
     method set_hd _ = raise (Invalid_argument "set_hd")
     method set_tl _ = raise (Invalid_argument "set_tl")
  end
```
  The class `cons` implements the mutable cons-cell.
  The constraint `_ : #int_list as 'self` is used to simplify
  the types.
  
```ocaml
  class cons hd tl =
  object (_ : #int_list as 'self)
     val mutable hd = hd
     val mutable tl = tl
     method is_nil = false
     method hd = hd
     method tl = tl
     method set_hd x = hd <- x
     method set_tl l = tl <- l
  end
```

1.
  No, it is not possible to generalize the type, at least not easily.
  The problem is that the class `cons` takes the `tl` as an argument.
  If we try to implement it, we get the error ``Self type cannot escape its class,''
  because the argument `tl` has the same type as the class being defined.
  
```ocaml
  class cons hd tl =
  object (_ : #gen_int_list as 'self)
  $\cdots$
  end
  @
  \begin{topoutput}
        This expression has type 'a but is here used with type
          < hd : int; is_nil : bool; set_hd : int -> unit; set_tl : 'b -> unit;
            tl : 'b; .. >
          as 'b
        Self type cannot escape its class
  \end{topoutput}
  @
```

1.
  Here are the complete class definitions.
```ocaml
  class nil =
  object (self : #int_list as 'self)
     method is_nil = true
     method hd = raise (Invalid_argument "hd")
     method tl = raise (Invalid_argument "tl")
     method set_hd (_ : int) = raise (Invalid_argument "set_hd")
     method set_tl (_ : int_list) = raise (Invalid_argument "set_tl")
     method iter f = ()
     method map f = (self :> int_list)
     method fold f x = x
  end
  
  class cons hd tl =
  object (self : #int_list as 'self)
     val mutable hd = hd
     val mutable tl = tl
     method is_nil = false
     method hd = hd
     method tl = tl
     method set_hd x = hd <- x
     method set_tl l = tl <- l
     method iter f = f hd; tl#iter f
     method map f = ({< hd = f hd; tl = tl#map f >} :> int_list)
     method fold f x = tl#fold f (f x hd)
  end
```

