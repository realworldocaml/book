(* #183 *)

type 'a repr =
| Bytes of ('a -> string)
| Int of ('a -> int)
| Int32 of ('a -> int32)
| Int64 of ('a -> int64)
| Float of ('a -> float)

let bytes_of_repr = function
| Bytes b -> fun v -> b v
| Int i -> fun v -> R_byte_sort.of_int (i v)
| Int32 i -> fun v -> R_byte_sort.of_int32 (i v)
| Int64 i -> fun v -> R_byte_sort.of_int64 (i v)
| Float f -> fun v -> R_byte_sort.of_float (f v)

(* #265 *)

let _ = ( a
        ;
          b
        )

let _ = {
  a
  ;
  b
}

let f x =
  ( foo
  ;
    bar )

let _ = ( a
        ; (* foo *)
          b
        )

let _ = {
  a
  ; (* foo *)
  b
}

let f x =
  ( foo
  ; (* foo *)
    bar )

(* #224 *)
let () =
  begin [@attribute]
    print_endline "hello";
    print_endline "world";
  end

(* #188 *)
let f : t1 -> t2 -> t3 =
  fun x y z ->
  x + y + z

(* #257 *)
module M = struct
  type a = A of b [@@deriving compare]
  and b = B of a
end

(* #275 *)
let g x =
  (x * x
   [@ocaml.ppwarning "TODO: blabla"])

let h = "I am well indented"

let i x =
  x * x
  [@ocaml.ppwarning "TODO: blabla"]

let j = "I am NOT well indented"

(* #277 *)
module V = struct
  type t =
  | A of A.t [@blah "a"]
  | B of B.t [@blah "b"]
  | C of C.t [@blah "c"]
end

let foo =
  let f x =
    foo bar [@@bla] in
  zz
