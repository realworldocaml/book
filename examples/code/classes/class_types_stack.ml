module Stack = struct
  class ['a] stack init = object
    ...    
  end

  type 'a t = 'a stack

  let make init = new stack init
end


[@@@part "1"];;
module AbstractStack : sig
   type 'a t = < pop: 'a option; push: 'a -> unit >

   val make : unit -> 'a t
end = Stack


[@@@part "2"];;
module VisibleStack : sig
  
  type 'a t = < pop: 'a option; push: 'a -> unit >

  class ['a] stack : object
    val mutable v : 'a list
    method pop : 'a option
    method push : 'a -> unit
  end

  val make : unit -> 'a t
end = Stack
