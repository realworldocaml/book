open Ppx_hash_lib.Std
open Hash.Builtin

module Check_struct_items_match_sig_items = struct
  module M : sig
    type t0 [@@deriving hash]
    type _ t1 [@@deriving hash]
    type (_,_) t2 [@@deriving hash]
  end = struct
    type t0 = int [@@deriving hash]
    type 'a t1 = 'a * int [@@deriving hash]
    type ('a,'b) t2 = 'a * 'b * int [@@deriving hash]
  end
end

module Examples_from_design_doc = struct

  type a = int [@@deriving hash]
  type b = int [@@deriving hash]
  type c = int [@@deriving hash]

  type t1 = a * b * c [@@deriving hash]
  type t2 = {a : a; b : b; c : c;} [@@deriving hash]
  type t3 = Foo | Bar of a | Qaz of b * c [@@deriving hash]
  type t4 = [`Foo of a | `Bar] [@@deriving hash]
  type 'a t5 = ('a * 'a) list [@@deriving hash]
  type t6 = int * string list [@@deriving hash]

  type t7 = int lazy_t [@@deriving hash]

end

module String = struct
  include String
  let hash_fold_t = hash_fold_string
  let hash = hash_string
end

let hash = `Should_refer_to_Hashtbl_hash_explicitly

type 'a array_frozen = 'a array
type 'a ref_frozen = 'a ref

module M1s = struct type s = unit [@@deriving hash] end

module M2s = struct type s = int [@@deriving hash] end

module M1 = struct type t = unit [@@deriving hash] end

module M2 = struct type t = int [@@deriving hash] end

module M3 = struct type t = bool [@@deriving hash] end

module M4 = struct type t = int32 [@@deriving hash] end

module M5 = struct type t = nativeint [@@deriving hash] end

module M6 = struct type t = int64 [@@deriving hash] end

module M7 = struct type t = float [@@deriving hash] end

module M8 = struct type t = bool * float [@@deriving hash] end

module M9 = struct type t = bool * float * int [@@deriving hash] end

module M10 = struct type t = bool * float * int * string [@@deriving hash]  end

module M11 = struct type t = int ref_frozen [@@deriving hash] end

module M12 = struct type t = (float * float) option [@@deriving hash] end

module M13 = struct type t = float array_frozen [@@deriving hash] end

module M14 = struct type t = (int * int) array_frozen [@@deriving hash] end

module M15 = struct type t = float array_frozen array_frozen [@@deriving hash] end

module M16 = struct type t = int list [@@deriving hash] end

module M17 = struct type t = {
  s : string;
  b : float array_frozen list;
  mutable c : (int * int64 option); [@hash.ignore]
} [@@deriving hash]
end

module M18 = struct type t = {
  a : float;
  b : float;
  c : float;
} [@@deriving hash]
end

module M19 = struct type t = Foo [@@deriving hash] end

module M20 = struct type t = Foo of int [@@deriving hash] end

module M21 = struct type t = Foo of int * float [@@deriving hash]    end

module M22 = struct type t = Foo | Bar of int | Baz of string option [@@deriving hash] end

module M23 = struct type t = [`Foo | `Bar of string * string] [@@deriving hash] end

module M24 = struct type t = int * string * [`Foo | `Bar ] [@@deriving hash] end

module M25 = struct type t = String.t [@@deriving hash] end

module M26 = struct type 'a t = 'a array_frozen [@@deriving hash] end

module MyList = struct type 'a t = Nil | Node of 'a * 'a t [@@deriving hash] end

module M27 = struct
  type t = int [@@deriving hash]
  module Inner = struct
    type nonrec t = t list [@@deriving hash]
  end
end

module M28 = struct
  (* making sure that nobody is reversing the type parameters *)
  type ('a, 'b) t = ('a * 'b) list [@@deriving hash]
  let _ = [%hash_fold: (int,float) t] (Hash.create ()) [(1,nan)]
end

module Polyrec = struct
  type ('a, 'b) t = T of ('a option, 'b) t [@@deriving hash]

  type ('a, 'b) t1 = T of ('a option, 'b) t2
  and ('a, 'b) t2 = T1 of ('a list, 'b) t1 | T2 of ('a, 'b list) t2
  [@@deriving hash]
end

module type Variance_sig = sig
  type +'a t [@@deriving hash]
end

module Variance = struct
  type -'a t [@@deriving hash]
  type (-'a, +'b) u = 'a t * 'b [@@deriving hash]
end

module Simple_variant_inclusion = struct
  type poly_t =  [ `Foo of int | `Bar ] [@@deriving hash]
  type include_poly_t = [ poly_t | `Blah ] [@@deriving hash]
end

module Variant_inclusion = struct
  type 'a type1 = [ `T1 of 'a ] [@@deriving hash]
  type 'a type2 = [ 'a type1 | `T2 ] [@@deriving hash]
  type 'a type3 = [ `T3 | 'a type1 ] [@@deriving hash]
  type 'a type4 = [ 'a type2 | `T4 | 'a type3 ] [@@deriving hash]
end

module SigTU = struct
  module A : sig
    type t [@@deriving hash]
    type u [@@deriving hash]
  end = struct
    type t = int [@@deriving hash]
    type u = float [@@deriving hash]
  end
  type p = A.t * A.u [@@deriving hash]
end

module Gadt = struct
  type 'a t =
    | I : int -> int t
    | F : float -> float t
    | R : int t * string t -> bool t
  [@@deriving hash]
end

module Clash = struct
  (* Same name for type-var and type-name; must be careful when introducing rigid type names. *)
  type 'hey hey = Hey of 'hey [@@deriving hash]
  type 'hey rigid_hey = Hey of 'hey [@@deriving hash]
  type ('foo,'rigid_foo) foo = Foo of 'foo [@@deriving hash]
  type 'rigid_bar rigid_rigid_bar = Bar [@@deriving hash]
end

module Type_extension = struct
  let _ = ([%hash_fold: int list] : [%hash_fold: int list])
  let _ = ([%hash: int list] : [%hash: int list])
end

module Recursion_with_aliases = struct
  type a = A of c
  and b = a
  and c = b
  [@@deriving hash]
end

module Nested_tuples = struct
  type a = int * (string * bool) [@@deriving_inline hash]

  
let _ = fun (_ : a)  -> ()
  
  
let (hash_fold_a :
  Ppx_hash_lib.Std.Hash.state -> a -> Ppx_hash_lib.Std.Hash.state) =
  fun hsv  ->
    fun arg  ->
      let (e0,e1) = arg  in
      let hsv = hash_fold_int hsv e0  in
      let hsv =
        let (e0,e1) = e1  in
        let hsv = hash_fold_string hsv e0  in
        let hsv = hash_fold_bool hsv e1  in hsv  in
      hsv

let _ = hash_fold_a
let (hash_a : a -> Ppx_hash_lib.Std.Hash.hash_value) =
  let func arg =
    Ppx_hash_lib.Std.Hash.get_hash_value
      (let hsv = Ppx_hash_lib.Std.Hash.create ()  in hash_fold_a hsv arg)
     in
  fun x  -> func x
let _ = hash_a
[@@@deriving.end]
end
