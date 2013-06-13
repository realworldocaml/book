  
## Exercise
  You are given the following functor that defines a class `cell`
  containing a value of type `T.t`.
  
```ocaml
  module MakeCell (T : sig type t end) =
  struct
      class cell x =
      object
          val mutable x : T.t = x
          method private get = x
          method private set y = x <- y
      end
  end
```
  Define a singly-linked list of integers by inheriting from the class `cell`
  twice.  Your class should have the type `int_cons`.
  
```ocaml
  class type int_cons =
  object
     method hd : int
     method tl : int_cons option
     method set_hd : int -> unit
     method set_tl : int_cons option -> unit
  end
  
  type int_list = int_cons option
```
  
