module Stable = struct
  open Core_kernel.Core_kernel_stable

  module V1 = struct
    type ('key, 'a, 'cmp, 'enum) t = ('key, 'a, 'cmp) Map.V1.t

    module type Key = sig
      type t [@@deriving sexp, bin_io, compare, enumerate]
    end

    module type Key_with_witnesses = sig
      include Key
      include Comparator.V1.S with type t := t
      include Enumeration.S with type t := t
    end

    module type S = sig
      module Key : sig
        type t [@@deriving sexp, bin_io, compare, enumerate]
      end

      type comparator_witness
      type enumeration_witness

      type nonrec 'a t = (Key.t, 'a, comparator_witness, enumeration_witness) t
      [@@deriving bin_io, sexp, compare]
    end

    module Make_with_witnesses (Key : Key_with_witnesses) = struct
      module Key = struct
        include Key
        include Comparable.V1.Make (Key)
      end

      type comparator_witness = Key.comparator_witness
      type enumeration_witness = Key.enumeration_witness
      type nonrec 'a t = 'a Key.Map.t [@@deriving bin_io, sexp, compare]
    end
  end
end

open! Core_kernel
open! Import
module Enumeration = Enumeration

type ('key, 'a, 'cmp, 'enum) t = ('key, 'a, 'cmp, 'enum) Stable.V1.t

let to_map t = t

let key_not_in_enumeration t key =
  failwiths
    ~here:[%here]
    "Key was not provided in the enumeration given to [Total_map.Make]"
    key
    (Map.comparator t).sexp_of_t
;;

let change t k ~f =
  Map.update t k ~f:(function
    | Some x -> f x
    | None -> key_not_in_enumeration t k)
;;

let find t k =
  match Map.find t k with
  | Some x -> x
  | None -> key_not_in_enumeration t k
;;

let pair t1 t2 key = function
  | `Left _ -> key_not_in_enumeration t2 key
  | `Right _ -> key_not_in_enumeration t1 key
  | `Both (v1, v2) -> v1, v2
;;

let iter2 t1 t2 ~f =
  Map.iter2 t1 t2 ~f:(fun ~key ~data ->
    let v1, v2 = pair t1 t2 key data in
    f ~key v1 v2)
;;

let fold2 t1 t2 ~init ~f =
  Map.fold2 t1 t2 ~init ~f:(fun ~key ~data acc ->
    let v1, v2 = pair t1 t2 key data in
    f ~key v1 v2 acc)
;;

let map2 t1 t2 ~f =
  Map.merge t1 t2 ~f:(fun ~key v ->
    let v1, v2 = pair t1 t2 key v in
    Some (f v1 v2))
;;

let set t key data = Map.set t ~key ~data

module Sequence (A : Applicative) = struct
  let sequence t =
    List.fold
      (Map.to_alist t)
      ~init:(A.return (Map.Using_comparator.empty ~comparator:(Map.comparator t)))
      ~f:(fun acc (key, data) ->
        A.map2 acc data ~f:(fun acc data -> Map.set acc ~key ~data))
  ;;
end

include struct
  open Map

  let data = data
  let for_all = for_all
  let iter = iter
  let iter_keys = iter_keys
  let iteri = iteri
  let map = map
  let mapi = mapi
  let fold = fold
  let fold_right = fold_right
  let to_alist = to_alist
end

module type Key = sig
  type t [@@deriving sexp, bin_io, compare, enumerate]
end

module type Key_with_witnesses = sig
  include Key
  include Comparator.S with type t := t
  include Enumeration.S with type t := t
end

module type S = sig
  module Key : Key

  type comparator_witness
  type enumeration_witness

  type nonrec 'a t = (Key.t, 'a, comparator_witness, enumeration_witness) t
  [@@deriving sexp, bin_io, compare]

  include Applicative with type 'a t := 'a t

  val create : (Key.t -> 'a) -> 'a t
  val create_const : 'a -> 'a t
end

module Make_with_witnesses (Key : sig
    include Key
    include Comparator.S with type t := t
    include Enumeration.S with type t := t
  end) =
struct
  module Key = struct
    include Key
    include Comparable.Make_binable_using_comparator (Key)
  end

  type comparator_witness = Key.comparator_witness
  type enumeration_witness = Key.enumeration_witness
  type 'a t = 'a Key.Map.t [@@deriving sexp, compare]

  let all_set = Key.Set.of_list Key.all

  let validate_map_from_serialization map =
    let keys = Map.key_set map in
    let keys_minus_all = Set.diff keys all_set in
    let all_minus_keys = Set.diff all_set keys in
    Validate.maybe_raise
      (Validate.of_list
         [ (if Set.is_empty keys_minus_all
            then Validate.pass
            else
              Validate.fails
                "map from serialization has keys not provided in the enumeration"
                keys_minus_all
                [%sexp_of: Key.Set.t])
         ; (if Set.is_empty all_minus_keys
            then Validate.pass
            else
              Validate.fails
                "map from serialization doesn't have keys it should have"
                all_minus_keys
                [%sexp_of: Key.Set.t])
         ])
  ;;

  let t_of_sexp a_of_sexp sexp =
    let t = t_of_sexp a_of_sexp sexp in
    validate_map_from_serialization t;
    t
  ;;

  include Bin_prot.Utils.Make_binable1_without_uuid [@alert "-legacy"] (struct
      type nonrec 'a t = 'a t

      module Binable = Key.Map

      let to_binable x = x

      let of_binable x =
        validate_map_from_serialization x;
        x
      ;;
    end)

  let create f =
    List.fold Key.all ~init:Key.Map.empty ~f:(fun t key -> Map.set t ~key ~data:(f key))
  ;;

  let create_const x = create (fun _ -> x)

  include Applicative.Make (struct
      type nonrec 'a t = 'a t

      let return = create_const
      let apply t1 t2 = map2 t1 t2 ~f:(fun f x -> f x)
      let map = `Custom map
    end)
end

module Make (Key : Key) = Make_with_witnesses (struct
    include Key
    include Comparable.Make_binable (Key)
    include Enumeration.Make (Key)
  end)
