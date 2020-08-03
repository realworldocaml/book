open Ppx_compare_lib.Builtin

let failwith = `Should_refer_to_runtime_lib
let ignore = `Should_refer_to_runtime_lib
let ( = ) = `Should_refer_to_runtime_lib
let ( <> ) = `Should_refer_to_runtime_lib
let ( == ) = `Should_refer_to_runtime_lib
let ( != ) = `Should_refer_to_runtime_lib
let ( > ) = `Should_refer_to_runtime_lib
let ( < ) = `Should_refer_to_runtime_lib
let ( >= ) = `Should_refer_to_runtime_lib
let ( <= ) = `Should_refer_to_runtime_lib
let ( max ) = `Should_refer_to_runtime_lib
let ( min ) = `Should_refer_to_runtime_lib
let ( equal ) = `Should_refer_to_runtime_lib
let ( compare ) = `Should_refer_to_runtime_lib

module M1 = struct type t = unit [@@deriving compare, equal] end

module M2 = struct type t = int [@@deriving compare, equal] end

module M3 = struct type t = bool [@@deriving compare, equal] end

module M4 = struct type t = int32 [@@deriving compare, equal] end

module M5 = struct type t = nativeint [@@deriving compare, equal] end

module M6 = struct type t = int64 [@@deriving compare, equal] end

module M7 = struct type t = float [@@deriving compare, equal] end

module M8 = struct type t = bool * float [@@deriving compare, equal] end

module M9 = struct type t = bool * float * int [@@deriving compare, equal] end

module M10 = struct type t = bool * float * int * string [@@deriving compare, equal]  end

module M11 = struct type t = int ref [@@deriving compare, equal] end

module M12 = struct type t = (float * float) option [@@deriving compare, equal] end

module M13 = struct type t = float array [@@deriving compare, equal] end

module M14 = struct type t = (int * int) array [@@deriving compare, equal] end

module M15 = struct type t = float array array [@@deriving compare, equal] end

module M16 = struct type t = int list [@@deriving compare, equal] end

module M17 = struct type t = {
  s : string;
  b : float array list;
  mutable c : (int * int64 option);
} [@@deriving compare, equal]
end

module M18 = struct type t = {
  a : float;
  b : float;
  c : float;
} [@@deriving compare, equal]
end

module M19 = struct type t = Foo [@@deriving compare, equal] end

module M20 = struct type t = Foo of int [@@deriving compare, equal] end

module M21 = struct type t = Foo of int * float [@@deriving compare, equal]    end

module M22 = struct type t = Foo | Bar of int | Baz of string option [@@deriving compare, equal] end

module M23 = struct type t = [`Foo | `Bar of string * string] [@@deriving compare, equal] end

module M24 = struct type t = int * string * [`Foo | `Bar ] [@@deriving compare, equal] end

module M25 = struct type t = String.t [@@deriving compare, equal] end

module M26 = struct type 'a t = 'a array [@@deriving compare, equal] end

module MyList = struct type 'a t = Nil | Node of 'a * 'a t [@@deriving compare, equal] end

module M27 = struct
  type t = int [@@deriving compare, equal]
  module Inner = struct
    type nonrec t = t list [@@deriving compare, equal]
    let _ = ((compare : int list -> int list -> int) : t -> t -> int)
  end
end

module M28 = struct
  (* making sure that nobody is reversing the type parameters *)
  type ('a, 'b) t = ('a * 'b) list [@@deriving compare, equal]
  let (_ : (int, float) t -> int) = [%compare: (int,float) t] [(1,nan)]
end

module M29 = struct
  type t = A of { a : float;
                  b : float;
                  c : float;
                }
         | B of float * float * float
  [@@deriving compare, equal]
end

module M30 = struct
  type ('a, 'b) t = A of { a : 'a;
                           b : 'b;
                           c : float;
                         }
                  | B of 'a * 'b
  [@@deriving compare, equal]
end


module Polyrec = struct
  type ('a, 'b) t = T of ('a option, 'b) t [@@deriving compare, equal]

  type ('a, 'b) t1 = T of ('a option, 'b) t2
  and ('a, 'b) t2 = T1 of ('a list, 'b) t1 | T2 of ('a, 'b list) t2
  [@@deriving compare, equal]
end

module type Variance_sig = sig
  type +'a t [@@deriving compare, equal]
end

module Variance = struct
  type -'a t [@@deriving compare, equal]
  type (-'a, +'b) u = 'a t * 'b [@@deriving compare, equal]
end

module Test = struct
  let (=) : int -> int -> bool = Base.Poly.(=)
  (* checking that for the types mentioned in the readme, we compare structurally  *)
  let%test _ = [%compare: unit option] None (Some ()) = Base.Poly.compare None (Some ())
  let%test _ = [%compare: unit list] [] [()] = Base.Poly.compare [] [()]
  let%test _ = [%compare: int array] [|0; 1|] [|1|] = Base.Poly.compare [|0; 1|] [|1|]
  let%test _ =
    Base.Poly.(=)
      (List.sort [%compare: int option] [Some 3; None; Some 2; Some 1])
      [None; Some 1; Some 2; Some 3]
end

module Variant_inclusion = struct
  type 'a type1 = [ `T1 of 'a ] [@@deriving compare, equal]
  type 'a type2 = [ 'a type1 | `T2 ] [@@deriving compare, equal]
  type 'a type3 = [ `T3 | 'a type1 ] [@@deriving compare, equal]
  type 'a type4 = [ 'a type2 | `T4 | 'a type3 ] [@@deriving compare, equal]
  type 'a id = 'a [@@deriving compare, equal]

  type ('a, 'b) u = [`u of 'a * 'b] [@@deriving compare, equal]
  type t = [ | (int, int) u ] [@@deriving compare, equal]
end

module Equal = struct
  let%test _ = [%compare.equal: int list] [7; 8; 9] [7; 8; 9]
  let%test _ = not ([%compare.equal: int list] [7; 8] [7; 8; 9])

  let%test _ =
    match [%compare: int * int] (1, 2) (1, 3) with
    | -1 -> true
    | _  -> false

  let%test _ =
    match [%compare: int * int] (1, 3) (1, 2) with
    | 1 -> true
    | _ -> false

  let%test _ = [%compare.equal: string option] None None
  let%test _ = not ([%compare.equal: string option] (Some "foo") None)

  let%test _ = [%compare.equal: string] "hello" "hello"
  let%test _ = not ([%compare.equal: string] "hello" "goodbye")
end

module Type_extensions : sig
  (* Making sure we don't generate [_ t -> _ t -> int], as
     that's too general. *)
  module type S = sig
    type 'a t
    val compare : [%compare: _ t]
    val equal : [%compare.equal: _ t]
  end
end = struct
  module type S = sig
    type 'a t
    val compare : 'a t -> 'a t -> int
    val equal : 'a t -> 'a t -> bool
  end
end

module Ignoring_field = struct

  type t =
    { a : int [@ignore]
    ; b : int
    ; c : int
    }
  [@@deriving_inline compare, equal]

  
let _ = fun (_ : t) -> ()
  
  
let compare =
  (fun a__589_ ->
     fun b__590_ ->
       if Ppx_compare_lib.phys_equal a__589_ b__590_
       then 0
       else
         (match compare_int a__589_.b b__590_.b with
          | 0 -> compare_int a__589_.c b__590_.c
          | n -> n) : t -> t -> int)
let _ = compare
let equal =
  (fun a__591_ ->
     fun b__592_ ->
       if Ppx_compare_lib.phys_equal a__591_ b__592_
       then true
       else
         Ppx_compare_lib.(&&) (equal_int a__591_.b b__592_.b)
           (equal_int a__591_.c b__592_.c) : t -> t -> bool)
let _ = equal
[@@@deriving.end]

  let equal = [%compare.equal: t]
end

module Ignoring_inline = struct
  type t = int * int * int

  let compare = [%compare: _ * (int [@ignore]) * int]

  let _ = compare

  let equal = [%compare.equal: t]

  let%test _ = equal (0, 1, 2) (9, 1, 2)
  let%test _ = equal (0, 1, 2) (0, 9, 2)
  let%test _ = not (equal (0, 1, 2) (0, 1, 9))
end


module Ignoring = struct
  type t = { a : (int [@ignore]) * string }
  [@@deriving_inline compare, equal]

  
let _ = fun (_ : t) -> ()
  
let compare =
  (fun a__609_ ->
     fun b__610_ ->
       if Ppx_compare_lib.phys_equal a__609_ b__610_
       then 0
       else
         (let (t__611_, t__612_) = a__609_.a in
          let (t__613_, t__614_) = b__610_.a in
          match let (_ : _) = t__611_
                and (_ : _) = t__613_ in 0 with
          | 0 -> compare_string t__612_ t__614_
          | n -> n) : t -> t -> int)
let _ = compare
let equal =
  (fun a__615_ ->
     fun b__616_ ->
       if Ppx_compare_lib.phys_equal a__615_ b__616_
       then true
       else
         (let (t__617_, t__618_) = a__615_.a in
          let (t__619_, t__620_) = b__616_.a in
          Ppx_compare_lib.(&&)
            (let (_ : _) = t__617_
             and (_ : _) = t__619_ in true) (equal_string t__618_ t__620_)) :
  t -> t -> bool)
let _ = equal
[@@@deriving.end]

  let%test _ = equal { a = (1, "hi") } { a = (2, "hi") }
  let%test _ = not (equal { a = (1, "hi") } { a = (1, "ho") })
end

module Ignoring_with_type = struct
  type t =
    { a : int
    ; b : (int[@compare.ignore])
    }
  [@@deriving_inline compare]
  
let _ = fun (_ : t) -> ()
  
let compare =
  (fun a__621_ ->
     fun b__622_ ->
       if Ppx_compare_lib.phys_equal a__621_ b__622_
       then 0
       else
         (match compare_int a__621_.a b__622_.a with
          | 0 -> let (_ : _) = a__621_.b
                 and (_ : _) = b__622_.b in 0
          | n -> n) : t -> t -> int)
let _ = compare
[@@@deriving.end]
end

module Enum_optim = struct

  type t = A | B | C
  [@@deriving_inline compare, equal]

  
let _ = fun (_ : t) -> ()
  
  
let compare = (Ppx_compare_lib.polymorphic_compare : t -> t -> int)
let _ = compare
let equal = (Ppx_compare_lib.polymorphic_equal : t -> t -> bool)
let _ = equal
[@@@deriving.end]
end


module Lazy_behavior = struct
  (* Test that the generated functions don't evaluate more than necessary *)
  type a = unit
  let equal_a () () = assert false
  let compare_a () () = assert false
  type b = int * a [@@deriving compare, equal]

  let%test _ = not (equal_b (0, ()) (1, ()))
  let%test _ = Base.Poly.(<) (compare_b (0, ()) (1, ())) 0
end

module Not_ieee_compliant = struct
  type t = float [@@deriving compare, equal]

  let%test _ = [%equal: t] nan nan
  let%test _ = Base.Poly.(=) ([%compare: t] nan nan) 0
end

module Wildcard : sig
  type _ transparent = int [@@deriving compare, equal]

  type _ opaque [@@deriving compare, equal]
end = struct
  type _ transparent = int [@@deriving compare, equal]

  let%test _ = [%equal: string transparent] 1 1
  let%test _ = not ([%equal: string transparent] 1 2)

  let%test _ = Base.Poly.(<) ([%compare: string transparent] 1 2) 0
  let%test _ = Base.Poly.(=) ([%compare: string transparent] 1 1) 0
  let%test _ = Base.Poly.(>) ([%compare: string transparent] 2 1) 0

  type 'a opaque = 'a option [@@deriving compare, equal]

  let%test _ = [%equal: int opaque] (Some 1) (Some 1)
  let%test _ = not ([%equal: int opaque] None (Some 1))
  let%test _ = not ([%equal: int opaque] (Some 1) (Some 2))

  let%test _ = Base.Poly.(<) ([%compare: int opaque] None (Some 1)) 0
  let%test _ = Base.Poly.(=) ([%compare: int opaque] (Some 1) (Some 1)) 0
  let%test _ = Base.Poly.(>) ([%compare: int opaque] (Some 2) (Some 1)) 0
end
