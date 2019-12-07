class c = object
  val mutable x = 0
  method get_x = x
  method set_x x' = x <- x'
  method print = print_int x
  initializer print_endline "created"
end

let i = new c

class c' = object
  val mutable x = 0
  method get_x = x
  method set_x x' = print_endline "modified"; x <- x'
  method print = print_int x; print_newline ()
  initializer print_string "created"; print_newline ()
end

let i = new c

class virtual c'' = object
  method virtual get_x : int
  method set_x = ()
end

class p (v : int) = object
  method get_v = v
end

class p' = object
  inherit p 0
end
