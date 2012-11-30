# Imperative Programming

The OCaml programming language is _functional_, meaning that functions
are first-class values that can be passed around like any other.
However, this doesn't mean that OCaml programs are _pure_.  The
language includes assignment, mutable values like arrays and strings.
Evaluation order is strict and sequential.  In principle, you can port
many imperative programs directly to OCaml.  If you find yourself
doing this a lot, then OCaml may not be the right programming language
for your problem.  However, there are times when imperative
programming is both appropriate and efficient, and OCaml shines at
supporting programs with both functional and imperative aspects.

To illustrate imperative programming, let's start by implementing a
hash table.  Hash tables are an efficient way to implement imperative
_dictionaries_, and there are implementations Core as well as in the
OCaml standard library.  We'll consider _open hashing_, where there
are an array of buckets, each of which contain a linked list of
elements.  We'll use regular (pure) OCaml lists, and an array for the
buckets.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {.ocaml}
module Hashtbl : sig
  type ('a, 'b) t

  val create : unit -> ('a, 'b) t
  val add : ('a, 'b) t -> key:'a -> data:'b -> unit
  val find : ('a, 'b) t -> key:'a -> 'b
end = struct
  type ('a, 'b) t = ('a * 'b) list array
  
  let num_buckets = 17
  let hash_bucket key = (Hashtbl.hash key) mod num_buckets
  
  let create = Array.create num_buckets []

  let add table ~key ~data =
    let index = hash_bucket key in
	table.(index) <- (key, data) :: table.(index)
	
  let find table ~key =
	let rec find = function
	 | [] -> raise Not_found
	 | (k, d) :: _ when k = key -> d
	 | _ :: t -> find t
	in
	find buckets.(hash_bucket key)
end
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
