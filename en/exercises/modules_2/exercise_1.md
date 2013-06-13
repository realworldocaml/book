  
  
## Exercise
  Which of the following are legal programs?  Explain your answers.
1.
  
```ocaml
  module A : sig
     val x : string
  end = struct
     let x = 1
     let x = "x"
  end
```
  
1.
  
```ocaml
  module A : sig
     val x : string
     val x : string
  end = struct
     let x = "x"
  end
```
  
1.
  
```ocaml
  module a = struct
     let x = 1
  end;;
```
  
1.
  
```ocaml
  module M : sig
     val f : int -> int
     val g : string -> string
  end = struct
     let g x = x
     let f x = g x
  end
```
  
1.
  
```ocaml
  let module X = struct let x = 1 end in X.x
```
  
1.
  
```ocaml
  module M = struct
     let g x = h x
     let f x = g x
     let h x = x + 1
  end
```
  
1.
  
```ocaml
  module rec M : sig
     val f : int -> int
     val h : int -> int
  end = struct
     open M
     let g x = h x
     let f x = g x
     let h x = x + 1
  end
```
  
1.
  
```ocaml
  module rec M : sig
     val f : int -> int
  end = struct
     let f = M.f
  end
```
  
1.
  
```ocaml
  type 'a t = { set : 'a -> unit; get : unit -> 'a }
  let f x =
     let cell = ref x in
     let module M = struct
        let s i = cell := i
        let g () = !cell
        let r = { set = s; get = g }
     end
     in
        M.r
```
  
1.
  
```ocaml
  let f x =
     let cell = ref x in
     let module M = struct
        type 'a t = { set : 'a -> unit; get : unit -> 'a }
        let s i = cell := i
        let g () = !cell
        let r = { set = s; get = g }
     end
     in
        M.r
```
  
1.
  
```ocaml
  module type ASig = sig type s  val f : int -> s end
  module type BSig = sig type t  val g : t -> int end
  module C : sig
     module A : ASig
     module B : BSig with type t = A.s
  end = struct
     type u = string
     module A = struct type s = u  let f = string_of_int end
     module B = struct type t = u  let g = int_of_string end
  end
  include C
  let i = B.g (A.f ())
```
  
1.
  
```ocaml
  module type ASig = sig type t end
  module type BSig = sig val x : int end
  module A : ASig with type t = int
     = struct type t = int end
  module B : BSig = struct let x = 1 end
  module C : sig
     include ASig
     val x : t
  end = struct
     include A
     include B
  end
```
  
