open! Import
open Std_internal
include Univ_map_intf
module Uid = Type_equal.Id.Uid

module Make1 (Data : sig
    type ('s, 'a) t [@@deriving sexp_of]
  end) =
struct
  type ('s, 'a) data = ('s, 'a) Data.t

  module Packed = struct
    type 's t = T : 'a Key.t * ('s, 'a) Data.t -> 's t

    let sexp_of_t sexp_of_a (T (key, data)) =
      Data.sexp_of_t sexp_of_a (Key.to_sexp key) data
    ;;

    let type_id_name (T (key, _)) = Key.name key
    let type_id_uid (T (key, _)) = Key.uid key
  end

  type 's t = 's Packed.t Uid.Map.t

  let sexp_of_t sexp_of_a t =
    Map.data t
    |> List.map ~f:(fun u -> Packed.type_id_name u, u)
    |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
    |> [%sexp_of: (string * a Packed.t) list]
  ;;

  let invariant (t : _ t) =
    Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
      Map.iteri t ~f:(fun ~key ~data ->
        assert (Uid.equal key (Packed.type_id_uid data))))
  ;;

  let set t key data = Map.set t ~key:(Key.uid key) ~data:(Packed.T (key, data))
  let mem_by_id t id = Map.mem t id
  let mem t key = mem_by_id t (Key.uid key)
  let remove_by_id t id = Map.remove t id
  let remove t key = remove_by_id t (Key.uid key)
  let empty = Uid.Map.empty
  let is_empty = Map.is_empty

  let find (type b) t (key : b Key.t) =
    match Map.find t (Key.uid key) with
    | None -> None
    | Some (Packed.T (key', value)) ->
      (* cannot raise -- see [invariant] *)
      let Type_equal.T = Key.same_witness_exn key key' in
      Some (value : (_, b) Data.t)
  ;;

  let find_exn t key =
    match find t key with
    | Some data -> data
    | None -> failwithf "Univ_map.find_exn on unknown key %s" (Key.name key) ()
  ;;

  let add t key data = if mem t key then `Duplicate else `Ok (set t key data)

  let add_exn t key data =
    match add t key data with
    | `Ok t -> t
    | `Duplicate -> failwithf "Univ_map.add_exn on existing key %s" (Key.name key) ()
  ;;

  let change_exn t key ~f:update =
    match find t key with
    | Some data -> set t key (update data)
    | None -> failwithf "Univ_map.change_exn on unknown key %s" (Key.name key) ()
  ;;

  let change t key ~f:update =
    let orig = find t key in
    let next = update orig in
    match next with
    | Some data -> set t key data
    | None -> if Option.is_none orig then t else remove t key
  ;;

  let update t key ~f = change t key ~f:(fun data -> Some (f data))
  let to_alist t = Map.data t

  let of_alist_exn t =
    Uid.Map.of_alist_exn (List.map t ~f:(fun p -> Packed.type_id_uid p, p))
  ;;
end

module Make (Data : sig
    type 'a t [@@deriving sexp_of]
  end) =
struct
  module M = Make1 (struct
      type (_, 'a) t = 'a Data.t [@@deriving sexp_of]
    end)

  type t = unit M.t [@@deriving sexp_of]
  type 'a data = 'a Data.t

  let invariant = M.invariant
  let empty = M.empty
  let is_empty = M.is_empty
  let set = M.set
  let mem = M.mem
  let mem_by_id = M.mem_by_id
  let find = M.find
  let find_exn = M.find_exn
  let add = M.add
  let add_exn = M.add_exn
  let change = M.change
  let change_exn = M.change_exn
  let update = M.update
  let remove = M.remove
  let remove_by_id = M.remove_by_id

  module Packed = struct
    type t = T : 'a Key.t * 'a Data.t -> t
  end

  let to_alist t =
    List.map (M.to_alist t) ~f:(function M.Packed.T (key, data) -> Packed.T (key, data))
  ;;

  let of_alist_exn t =
    M.of_alist_exn
      (List.map t ~f:(function Packed.T (key, data) -> M.Packed.T (key, data)))
  ;;
end

include Make (struct
    type 'a t = 'a [@@deriving sexp_of]
  end)

module With_default = struct
  module Key = struct
    type 'a t =
      { key : 'a Key.t
      ; default : 'a
      }

    let create ~default ~name sexp_of = { default; key = Key.create ~name sexp_of }
    let id t = t.key
  end

  let find t { Key.key; default } = Option.value ~default (find t key)
  let set t { Key.key; default = _ } v = set t key v
  let change t k ~f:update = set t k (update (find t k))
end

module With_fold = struct
  module Key = struct
    type ('a, 'b) t =
      { key : 'b With_default.Key.t
      ; f : 'b -> 'a -> 'b
      }

    let create ~init ~f ~name sexp_of =
      { f; key = With_default.Key.create ~default:init ~name sexp_of }
    ;;

    let id t = With_default.Key.id t.key
  end

  let find t { Key.key; f = _ } = With_default.find t key
  let set t { Key.key; f = _ } v = With_default.set t key v
  let change t { Key.key; f = _ } ~f:update = With_default.change t key ~f:update
  let add t { Key.key; f } v = With_default.change t key ~f:(fun acc -> f acc v)
end

module Multi = struct
  open With_fold

  module Key = struct
    type 'a t = ('a, 'a list) Key.t

    let create ~name sexp_of =
      Key.create ~init:[] ~f:(fun xs x -> x :: xs) ~name (List.sexp_of_t sexp_of)
    ;;

    let id = With_fold.Key.id
  end

  let set = set
  let find = find
  let add = add
  let change = change
end
