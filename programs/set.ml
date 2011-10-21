type 'a set = 'a list
let empty = []
let add x (s : 'a set) : 'a set = x :: s
let mem x s = List.mem x s

module type EqualSig = sig
   type t
   val equal : t -> t -> bool
end;;

module type SetSig = sig
   type t
   type elt
   val empty : t
   val mem : elt -> t -> bool
   val add : elt -> t -> t
   val find : elt -> t -> elt
end;;

module MakeSet (Equal : EqualSig)
: SetSig with type elt = Equal.t =
struct
   open Equal
   type elt = Equal.t
   type t = elt list
   let empty = []
   let rec mem x s =
      List.exists (equal x) s
   let add x l = x :: l
   let find x s =
      List.find (equal x) s
end

module StringCaseEqual = struct
   type t = string
   let equal s1 s2 =
      String.lowercase s1 = String.lowercase s2
end;;

module SSet = MakeSet (StringCaseEqual);;
