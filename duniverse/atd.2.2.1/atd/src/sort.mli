(**
   Topological sort that doesn't give up on cycles.

{v
     A --> B
     C --> D
     B --> C
     C --> B
     D --> E
     E --> E
v}

   gives the following ordering:

{v
     [A] [B C]* [D] [E]*
v}

   where a group marked with a star is cyclic, i.e any member of the group
   can be reached from any other member of that group.

   This is used by atdgen to sort type definitions by dependency order,
   creating recursive groups only when needed. This makes ocamlopt
   significantly faster in certain pathological situations.
   Also it improves the clarity of the generated code and can be used to
   report cycles in a context where they are not allowed.

   Feel free to reuse outside of atdgen. The algorithm is outlined in
   the ml file. The interface of this module may change without notice.
*)

module type Param =
sig
  type t
    (** Type of the nodes as specified by the user *)

  type id
    (** Node identifier that can be compared and hashed using
       the generic comparison and hashing functions of the standard library.
       Typically an int or a string.
    *)

  val id : t -> id
    (** User function to extract a node's unique identifier *)

  val to_string : id -> string
    (** User function to make a node identifier printable,
       used for debugging only. *)
end

module Make (P : Param) :
sig
  val sort : (P.t * P.id list) list -> (bool * P.t list) list
    (**
       Partition the nodes of a directed graph into groups and sort these
       groups such that all edges going from one group to another
       point to the right, and such that each group
       has a single element or is a cycle. A cyclic group is marked
       as [true] while non-cyclic singletons are marked as [false].

       A cycle is a set of nodes such that any node of the set
       can be reached from any other node of that set.

       All groups of more than one node are cyclic.
       Groups of one node may or may not be cyclic.
    *)

  (**/**)
  val debug : bool ref
end

(**/**)
val test : unit -> unit
