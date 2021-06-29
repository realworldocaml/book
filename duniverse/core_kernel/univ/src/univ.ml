open! Core_kernel
open! Import
module Id = Type_equal.Id

module View = struct
  type t = T : 'a Id.t * 'a -> t
end

include View

let view = Fn.id
let create id value = T (id, value)
let type_id_name (T (id, _)) = Id.name id
let type_id_uid (T (id, _)) = Id.uid id
let sexp_of_t (T (id, value)) = Id.to_sexp id value
let does_match (T (id1, _)) id2 = Id.same id1 id2

let match_ (type a) (T (id1, value)) (id2 : a Id.t) =
  match Id.same_witness id1 id2 with
  | Some Type_equal.T -> Some (value : a)
  | None -> None
;;

let match_exn (type a) (T (id1, value) as t) (id2 : a Id.t) =
  match Id.same_witness id1 id2 with
  | Some Type_equal.T -> (value : a)
  | None ->
    failwiths
      ~here:[%here]
      "Univ.match_exn called with mismatched value and type id"
      (t, id2)
      [%sexp_of: t * _ Id.t]
;;
