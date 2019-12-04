(* this lib should not depend on core *)
module List = struct
  include List
  let compare cmp a b =
    let rec loop a b =
      match a, b with
      | [], [] -> 0
      | [], _  -> -1
      | _ , [] -> 1
      | x :: xs, y :: ys ->
        let n = cmp x y in
        if n = 0 then loop xs ys
        else n
    in
    loop a b
end

module Uid : sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val next : string -> t
  val hash : t -> int
  val name : t -> string
  val static : t
end = struct
  type t = {
    code : int;
    name : string;
  }
  let compare a b = compare (a.code : int) b.code
  let equal a b = (a.code : int) = b.code
  let uid = ref 0
  let next name = let code = !uid in incr uid; {code; name}
  let hash a = Hashtbl.hash a.code
  let name a = a.name
  let static = next "static"
end

module Key = struct
  type t = {
    uid : Uid.t;
    params : t list;
  }
  let rec compare k1 k2 =
    if k1 == k2 then 0 else
    let cmp = Uid.compare k1.uid k2.uid in
    if cmp <> 0 then cmp else
      List.compare compare k1.params k2.params
  let equal a b = compare a b = 0
  let hash = (Hashtbl.hash : t -> int)
  let static = { uid = Uid.static ; params = [] }
end

type 'a t = Key.t
type 'a typename = 'a t

let key t = t
let uid t = t.Key.uid
let name t = Uid.name t.Key.uid
let static = Key.static

let create ?(name="Typename.create") () = { Key.uid = Uid.next name ; params = [] }

include struct
  (* The argument for Obj.magic here is the same as the one in core/type_equal *)

  let same (type a) (type b) (nm1 : a t) (nm2 : b t) = Key.compare nm1 nm2 = 0

  let same_witness (type a) (type b) (nm1 : a t) (nm2 : b t) =
    if Key.compare nm1 nm2 = 0
    then Some (Obj.magic Type_equal.refl : (a, b) Type_equal.t)
    else None

  let same_witness_exn (type a) (type b) (nm1 : a t) (nm2 : b t) =
    if Key.compare nm1 nm2 = 0
    then (Obj.magic Type_equal.refl : (a, b) Type_equal.t)
    else failwith "Typename.same_witness_exn"

end

module type S0 = sig
  type t
  val typename_of_t : t typename
end

module type S1 = sig
  type 'a t
  val typename_of_t : 'a typename -> 'a t typename
end

module type S2 = sig
  type ('a, 'b) t
  val typename_of_t :
    'a typename
    -> 'b typename
    -> ('a, 'b) t typename
end

module type S3 = sig
  type ('a, 'b, 'c) t
  val typename_of_t :
    'a typename
    -> 'b typename
    -> 'c typename
    -> ('a, 'b, 'c) t typename
end

module type S4 = sig
  type ('a, 'b, 'c, 'd) t
  val typename_of_t :
    'a typename
    -> 'b typename
    -> 'c typename
    -> 'd typename
    -> ('a, 'b, 'c, 'd) t typename
end

module type S5 = sig
  type ('a, 'b, 'c, 'd, 'e) t
  val typename_of_t :
    'a typename
    -> 'b typename
    -> 'c typename
    -> 'd typename
    -> 'e typename
    -> ('a, 'b, 'c, 'd, 'e) t typename
end

module Make0 (X : Named_intf.S0) = struct
  let uid = Uid.next X.name
  let typename_of_t = { Key.uid ; params = [] }
end

module Make1 (X : Named_intf.S1) = struct
  let uid = Uid.next X.name
  let typename_of_t a = { Key.uid ; params = [ a ] }
end

module Make2 (X : Named_intf.S2) = struct
  let uid = Uid.next X.name
  let typename_of_t a b = { Key.uid ; params = [ a ; b ] }
end

module Make3 (X : Named_intf.S3) = struct
  let uid = Uid.next X.name
  let typename_of_t a b c = { Key.uid ; params = [ a ; b ; c ] }
end

module Make4 (X : Named_intf.S4) = struct
  let uid = Uid.next X.name
  let typename_of_t a b c d = { Key.uid ; params = [ a ; b ; c ; d ] }
end

module Make5 (X : Named_intf.S5) = struct
  let uid = Uid.next X.name
  let typename_of_t a b c d e = { Key.uid ; params = [ a ; b ; c ; d ; e ] }
end

module Key_table = Hashtbl.Make (Key)

module Table (X : sig
  type 'a t
end) = struct

  type data = Data : 'a t * 'a X.t -> data
  type t = data Key_table.t

  let create int = Key_table.create int

  let mem table name = Key_table.mem table (key name)

  let set table name data =
    Key_table.replace table (key name) (Data (name, data))

  let find (type a) table (name : a typename) =
    let data =
      try Some (Key_table.find table (key name))
      with Base.Not_found_s _ | Caml.Not_found -> None
    in
    match data with
    | None -> None
    | Some (Data (name', data)) ->
      (fun (type b) (name' : b typename) (data : b X.t) ->
        let Type_equal.T = (same_witness_exn name' name : (b, a) Type_equal.t) in
        Some (data : a X.t)
      ) name' data
end

let fail uid_a uid_b =
  let msg =
    Printf.sprintf "Typename.Same_witness_exn %S %S" (Uid.name uid_a) (Uid.name uid_b)
  in
  failwith msg

module Same_witness_exn_1 (A : S1) (B : S1) = struct
  type t = { eq : 'a. ('a A.t, 'a B.t) Type_equal.t }

  let witness =
    let uid_a = uid (A.typename_of_t static) in
    let uid_b = uid (B.typename_of_t static) in
    if Uid.equal uid_a uid_b
    then { eq = Obj.magic Type_equal.refl }
    else fail uid_a uid_b
end

module Same_witness_exn_2 (A : S2) (B : S2) = struct
  type t = {
    eq : 'a 'b. ( ('a, 'b) A.t,
                  ('a, 'b) B.t ) Type_equal.t
  }

  let witness =
    let uid_a = uid (A.typename_of_t static static) in
    let uid_b = uid (B.typename_of_t static static) in
    if Uid.equal uid_a uid_b
    then { eq = Obj.magic Type_equal.refl }
    else fail uid_a uid_b
end

module Same_witness_exn_3 (A : S3) (B : S3) = struct
  type t = {
    eq : 'a 'b 'c. ( ('a, 'b, 'c) A.t,
                     ('a, 'b, 'c) B.t ) Type_equal.t
  }

  let witness =
    let uid_a = uid (A.typename_of_t static static static) in
    let uid_b = uid (B.typename_of_t static static static) in
    if Uid.equal uid_a uid_b
    then { eq = Obj.magic Type_equal.refl }
    else fail uid_a uid_b
end

module Same_witness_exn_4 (A : S4) (B : S4) = struct
  type t = {
    eq : 'a 'b 'c 'd. ( ('a, 'b, 'c, 'd) A.t,
                        ('a, 'b, 'c, 'd) B.t ) Type_equal.t
  }

  let witness =
    let uid_a = uid (A.typename_of_t static static static static) in
    let uid_b = uid (B.typename_of_t static static static static) in
    if Uid.equal uid_a uid_b
    then { eq = Obj.magic Type_equal.refl }
    else fail uid_a uid_b
end

module Same_witness_exn_5 (A : S5) (B : S5) = struct
  type t = {
    eq : 'a 'b 'c 'd 'e. ( ('a, 'b, 'c, 'd, 'e) A.t,
                           ('a, 'b, 'c, 'd, 'e) B.t ) Type_equal.t
  }

  let witness =
    let uid_a = uid (A.typename_of_t static static static static static) in
    let uid_b = uid (B.typename_of_t static static static static static) in
    if Uid.equal uid_a uid_b
    then { eq = Obj.magic Type_equal.refl }
    else fail uid_a uid_b
end
