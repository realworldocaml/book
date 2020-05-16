(* [List0] defines list functions that are primitives or can be simply defined in terms of
   [Caml.List].  [List0] is intended to completely express the part of [Caml.List] that
   [Base] uses -- no other file in Base other than list0.ml should use [Caml.List].
   [List0] has few dependencies, and so is available early in Base's build order.  All
   Base files that need to use lists and come before [Base.List] in build order should do
   [module List = List0].  Defining [module List = List0] is also necessary because it
   prevents ocamldep from mistakenly causing a file to depend on [Base.List]. *)

open! Import0

let hd_exn = Caml.List.hd
let length = Caml.List.length
let rev_append = Caml.List.rev_append
let tl_exn = Caml.List.tl
let unzip = Caml.List.split

(* These are eta expanded in order to permute parameter order to follow Base
   conventions. *)
let exists t ~f = Caml.List.exists t ~f
let exists2_ok l1 l2 ~f = Caml.List.exists2 l1 l2 ~f
let fold t ~init ~f = Caml.List.fold_left t ~f ~init
let fold2_ok l1 l2 ~init ~f = Caml.List.fold_left2 l1 l2 ~init ~f
let for_all t ~f = Caml.List.for_all t ~f
let for_all2_ok l1 l2 ~f = Caml.List.for_all2 l1 l2 ~f
let iter t ~f = Caml.List.iter t ~f
let iter2_ok l1 l2 ~f = Caml.List.iter2 l1 l2 ~f
let nontail_map t ~f = Caml.List.map t ~f
let nontail_mapi t ~f = Caml.List.mapi t ~f
let partition t ~f = Caml.List.partition t ~f
let rev_map t ~f = Caml.List.rev_map t ~f
let rev_map2_ok l1 l2 ~f = Caml.List.rev_map2 l1 l2 ~f
let sort l ~compare = Caml.List.sort l ~cmp:compare
let stable_sort l ~compare = Caml.List.stable_sort l ~cmp:compare

let rev = function
  | ([] | [ _ ]) as res -> res
  | x :: y :: rest -> rev_append rest [ y; x ]
;;
