open! Import
include Hash_set_intf

let hashable_s = Hashtbl.hashable_s
let hashable = Hashtbl.Private.hashable
let poly_hashable = Hashtbl.Poly.hashable
let with_return = With_return.with_return

type 'a t = ('a, unit) Hashtbl.t
type 'a hash_set = 'a t
type 'a elt = 'a

module Accessors = struct
  let hashable = hashable
  let clear = Hashtbl.clear
  let length = Hashtbl.length
  let mem = Hashtbl.mem
  let is_empty t = Hashtbl.is_empty t

  let find_map t ~f =
    with_return (fun r ->
      Hashtbl.iter_keys t ~f:(fun elt ->
        match f elt with
        | None -> ()
        | Some _ as o -> r.return o);
      None)
  ;;

  let find t ~f = find_map t ~f:(fun a -> if f a then Some a else None)
  let add t k = Hashtbl.set t ~key:k ~data:()

  let strict_add t k =
    if mem t k
    then Or_error.error_string "element already exists"
    else (
      Hashtbl.set t ~key:k ~data:();
      Result.Ok ())
  ;;

  let strict_add_exn t k = Or_error.ok_exn (strict_add t k)
  let remove = Hashtbl.remove

  let strict_remove t k =
    if mem t k
    then (
      remove t k;
      Result.Ok ())
    else Or_error.error "element not in set" k (Hashtbl.sexp_of_key t)
  ;;

  let strict_remove_exn t k = Or_error.ok_exn (strict_remove t k)
  let fold t ~init ~f = Hashtbl.fold t ~init ~f:(fun ~key ~data:() acc -> f acc key)
  let iter t ~f = Hashtbl.iter_keys t ~f
  let count t ~f = Container.count ~fold t ~f
  let sum m t ~f = Container.sum ~fold m t ~f
  let min_elt t ~compare = Container.min_elt ~fold t ~compare
  let max_elt t ~compare = Container.max_elt ~fold t ~compare
  let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t
  let fold_until t ~init ~f = Container.fold_until ~fold ~init ~f t
  let to_list = Hashtbl.keys

  let sexp_of_t sexp_of_e t =
    sexp_of_list sexp_of_e (to_list t |> List.sort ~compare:(hashable t).compare)
  ;;

  let to_array t =
    let len = length t in
    let index = ref (len - 1) in
    fold t ~init:[||] ~f:(fun acc key ->
      if Array.length acc = 0
      then Array.create ~len key
      else (
        index := !index - 1;
        acc.(!index) <- key;
        acc))
  ;;

  let exists t ~f = Hashtbl.existsi t ~f:(fun ~key ~data:() -> f key)
  let for_all t ~f = not (Hashtbl.existsi t ~f:(fun ~key ~data:() -> not (f key)))
  let equal t1 t2 = Hashtbl.equal (fun () () -> true) t1 t2
  let copy t = Hashtbl.copy t
  let filter t ~f = Hashtbl.filteri t ~f:(fun ~key ~data:() -> f key)
  let union t1 t2 = Hashtbl.merge t1 t2 ~f:(fun ~key:_ _ -> Some ())
  let diff t1 t2 = filter t1 ~f:(fun key -> not (Hashtbl.mem t2 key))

  let inter t1 t2 =
    let smaller, larger = if length t1 > length t2 then t2, t1 else t1, t2 in
    Hashtbl.filteri smaller ~f:(fun ~key ~data:() -> Hashtbl.mem larger key)
  ;;

  let filter_inplace t ~f =
    let to_remove = fold t ~init:[] ~f:(fun ac x -> if f x then ac else x :: ac) in
    List.iter to_remove ~f:(fun x -> remove t x)
  ;;

  let of_hashtbl_keys hashtbl = Hashtbl.map hashtbl ~f:ignore
  let to_hashtbl t ~f = Hashtbl.mapi t ~f:(fun ~key ~data:() -> f key)
end

include Accessors

let create ?growth_allowed ?size m = Hashtbl.create ?growth_allowed ?size m

let of_list ?growth_allowed ?size m l =
  let size =
    match size with
    | Some x -> x
    | None -> List.length l
  in
  let t = Hashtbl.create ?growth_allowed ~size m in
  List.iter l ~f:(fun k -> add t k);
  t
;;

let t_of_sexp m e_of_sexp sexp =
  match sexp with
  | Sexp.Atom _ -> of_sexp_error "Hash_set.t_of_sexp requires a list" sexp
  | Sexp.List list ->
    let t = create m ~size:(List.length list) in
    List.iter list ~f:(fun sexp ->
      let e = e_of_sexp sexp in
      match strict_add t e with
      | Ok () -> ()
      | Error _ -> of_sexp_error "Hash_set.t_of_sexp got a duplicate element" sexp);
    t
;;

module Creators (Elt : sig
    type 'a t

    val hashable : 'a t Hashable.t
  end) : sig
  type 'a t_ = 'a Elt.t t

  val t_of_sexp : (Sexp.t -> 'a Elt.t) -> Sexp.t -> 'a t_

  include
    Creators_generic
    with type 'a t := 'a t_
    with type 'a elt := 'a Elt.t
    with type ('elt, 'z) create_options :=
      ('elt, 'z) create_options_without_first_class_module
end = struct
  type 'a t_ = 'a Elt.t t

  let create ?growth_allowed ?size () =
    create ?growth_allowed ?size (Hashable.to_key Elt.hashable)
  ;;

  let of_list ?growth_allowed ?size l =
    of_list ?growth_allowed ?size (Hashable.to_key Elt.hashable) l
  ;;

  let t_of_sexp e_of_sexp sexp = t_of_sexp (Hashable.to_key Elt.hashable) e_of_sexp sexp
end

module Poly = struct
  type 'a t = 'a hash_set
  type 'a elt = 'a

  let hashable = poly_hashable

  include Creators (struct
      type 'a t = 'a

      let hashable = hashable
    end)

  include Accessors

  let sexp_of_t = sexp_of_t
end

module M (Elt : T.T) = struct
  type nonrec t = Elt.t t
end

module type Sexp_of_m = sig
  type t [@@deriving_inline sexp_of]

  val sexp_of_t : t -> Ppx_sexp_conv_lib.Sexp.t

  [@@@end]
end

module type M_of_sexp = sig
  type t [@@deriving_inline of_sexp]

  val t_of_sexp : Ppx_sexp_conv_lib.Sexp.t -> t

  [@@@end]

  include Hashtbl_intf.Key.S with type t := t
end

let sexp_of_m__t (type elt) (module Elt : Sexp_of_m with type t = elt) t =
  sexp_of_t Elt.sexp_of_t t
;;

let m__t_of_sexp (type elt) (module Elt : M_of_sexp with type t = elt) sexp =
  t_of_sexp (module Elt) Elt.t_of_sexp sexp
;;

module Private = struct
  let hashable = Hashtbl.Private.hashable
end
