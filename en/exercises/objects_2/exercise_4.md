  
## Exercise
  A mutable list of integers can be represented in object-oriented form with the following class type.
  
```ocaml
  class type int_list =
  object
      method is_nil : bool
      method hd : int
      method tl : int_list
      method set_hd : int -> unit
      method set_tl : int_list -> unit
  end
```
1.
  Define classes `nil` and `cons` that implement the usual list constructors.
  
```ocaml
  class nil : int_list
  class cons : int -> int_list -> int_list
```
  
  
1.
  
  The class type `int_list` is a recursive type.  Can it be generalized to the following type?
  
```ocaml
  class type gen_int_list =
  object ('self)
      method is_nil : bool
      method hd : int
      method tl : 'self
      method set_hd : int -> unit
      method set_tl : 'self -> unit
  end
```
  
  
1.
  
  The class type `int_list` should also include the usual list functions.
  
```ocaml
  class type int_list =
  object
      method is_nil : bool
      method hd : int
      method tl : int_list
      method set_hd : int -> unit
      method set_tl : int_list -> unit
      method iter : (int -> unit) -> unit
      method map  : (int -> int) -> int_list
      method fold : 'a. ('a -> int -> 'a) -> 'a -> 'a
  end
```
  Implement the methods `iter`, `map`, and `fold` for the
  classes `nil` and `cons`.
  
