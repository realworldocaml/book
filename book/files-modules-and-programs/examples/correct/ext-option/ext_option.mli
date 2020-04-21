open Base

(* Include the interface of the option module from Base *)
include (module type of Option)

(* Signature of function we're adding *)
val apply : ('a -> 'b) t -> 'a -> 'b t
