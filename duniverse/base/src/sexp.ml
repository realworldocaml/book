open Hash.Builtin
open Ppx_compare_lib.Builtin
module List = List0
module String = String0
include (Sexplib0.Sexp : module type of Sexplib0.Sexp with type t := Sexplib0.Sexp.t)

(** Type of S-expressions *)
type t = Sexplib0.Sexp.t =
  | Atom of string
  | List of t list
[@@deriving_inline compare, hash]

let rec compare =
  (fun a__001_ b__002_ ->
     if Ppx_compare_lib.phys_equal a__001_ b__002_
     then 0
     else (
       match a__001_, b__002_ with
       | Atom _a__003_, Atom _b__004_ -> compare_string _a__003_ _b__004_
       | Atom _, _ -> -1
       | _, Atom _ -> 1
       | List _a__005_, List _b__006_ -> compare_list compare _a__005_ _b__006_)
       : t -> t -> int)
;;

let rec (hash_fold_t : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state) =
  (fun hsv arg ->
     match arg with
     | Atom _a0 ->
       let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 0 in
       let hsv = hsv in
       hash_fold_string hsv _a0
     | List _a0 ->
       let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 1 in
       let hsv = hsv in
       hash_fold_list hash_fold_t hsv _a0
       : Ppx_hash_lib.Std.Hash.state -> t -> Ppx_hash_lib.Std.Hash.state)

and (hash : t -> Ppx_hash_lib.Std.Hash.hash_value) =
  let func arg =
    Ppx_hash_lib.Std.Hash.get_hash_value
      (let hsv = Ppx_hash_lib.Std.Hash.create () in
       hash_fold_t hsv arg)
  in
  fun x -> func x
;;

[@@@end]

let of_string = ()
let invariant (_ : t) = ()
