open! Import

(* WARNING:
   We use non-memory-safe things throughout the [Trusted] module.
   Most of it is only safe in combination with the type signature (e.g. exposing
   [val copy : 'a t -> 'b t] would be a big mistake). *)
module Trusted : sig
  type 'a t

  val empty : 'a t
  val unsafe_create_uninitialized : len:int -> 'a t
  val create_obj_array : len:int -> 'a t
  val create : len:int -> 'a -> 'a t
  val singleton : 'a -> 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val swap : _ t -> int -> int -> unit
  val unsafe_get : 'a t -> int -> 'a
  val unsafe_set : 'a t -> int -> 'a -> unit
  val unsafe_set_omit_phys_equal_check : 'a t -> int -> 'a -> unit
  val unsafe_set_int : 'a t -> int -> int -> unit
  val unsafe_set_int_assuming_currently_int : 'a t -> int -> int -> unit
  val unsafe_set_assuming_currently_int : 'a t -> int -> 'a -> unit
  val length : 'a t -> int
  val unsafe_blit : ('a t, 'a t) Blit.blit
  val copy : 'a t -> 'a t
  val unsafe_clear_if_pointer : _ t -> int -> unit
end = struct
  type 'a t = Obj_array.t

  let empty = Obj_array.empty
  let unsafe_create_uninitialized ~len = Obj_array.create_zero ~len
  let create_obj_array ~len = Obj_array.create_zero ~len
  let create ~len x = Obj_array.create ~len (Caml.Obj.repr x)
  let singleton x = Obj_array.singleton (Caml.Obj.repr x)
  let swap t i j = Obj_array.swap t i j
  let get arr i = Caml.Obj.obj (Obj_array.get arr i)
  let set arr i x = Obj_array.set arr i (Caml.Obj.repr x)
  let unsafe_get arr i = Caml.Obj.obj (Obj_array.unsafe_get arr i)
  let unsafe_set arr i x = Obj_array.unsafe_set arr i (Caml.Obj.repr x)
  let unsafe_set_int arr i x = Obj_array.unsafe_set_int arr i x

  let unsafe_set_int_assuming_currently_int arr i x =
    Obj_array.unsafe_set_int_assuming_currently_int arr i x
  ;;

  let unsafe_set_assuming_currently_int arr i x =
    Obj_array.unsafe_set_assuming_currently_int arr i (Caml.Obj.repr x)
  ;;

  let length = Obj_array.length
  let unsafe_blit = Obj_array.unsafe_blit
  let copy = Obj_array.copy

  let unsafe_set_omit_phys_equal_check t i x =
    Obj_array.unsafe_set_omit_phys_equal_check t i (Caml.Obj.repr x)
  ;;

  let unsafe_clear_if_pointer = Obj_array.unsafe_clear_if_pointer
end

include Trusted

let invariant t = assert (Caml.Obj.tag (Caml.Obj.repr t) <> Caml.Obj.double_array_tag)

let init l ~f =
  if l < 0
  then invalid_arg "Uniform_array.init"
  else (
    let res = unsafe_create_uninitialized ~len:l in
    for i = 0 to l - 1 do
      unsafe_set res i (f i)
    done;
    res)
;;

let of_array arr = init ~f:(Array.unsafe_get arr) (Array.length arr)
let map a ~f = init ~f:(fun i -> f (unsafe_get a i)) (length a)

let iter a ~f =
  for i = 0 to length a - 1 do
    f (unsafe_get a i)
  done
;;

let iteri a ~f =
  for i = 0 to length a - 1 do
    f i (unsafe_get a i)
  done
;;

let to_list t = List.init ~f:(get t) (length t)

let of_list l =
  let len = List.length l in
  let res = unsafe_create_uninitialized ~len in
  List.iteri l ~f:(fun i x -> set res i x);
  res
;;

(* It is not safe for [to_array] to be the identity function because we have code that
   relies on [float array]s being unboxed, for example in [bin_write_array]. *)
let to_array t = Array.init (length t) ~f:(fun i -> unsafe_get t i)

let exists t ~f =
  let rec loop t ~f i =
    if i < 0 then false else f (unsafe_get t i) || loop t ~f (i - 1)
  in
  loop t ~f (length t - 1)
;;

let map2_exn t1 t2 ~f =
  let len = length t1 in
  if length t2 <> len then invalid_arg "Array.map2_exn";
  init len ~f:(fun i -> f (unsafe_get t1 i) (unsafe_get t2 i))
;;

include Sexpable.Of_sexpable1
    (Array)
    (struct
      type nonrec 'a t = 'a t

      let to_sexpable = to_array
      let of_sexpable = of_array
    end)

include Blit.Make1 (struct
    type nonrec 'a t = 'a t

    let length = length

    let create_like ~len t =
      if len = 0
      then empty
      else (
        assert (length t > 0);
        create ~len (get t 0))
    ;;

    let unsafe_blit = unsafe_blit
  end)

let fold t ~init ~f =
  let r = ref init in
  for i = 0 to length t - 1 do
    r := f !r (unsafe_get t i)
  done;
  !r
;;

let min_elt t ~compare = Container.min_elt ~fold t ~compare
let max_elt t ~compare = Container.max_elt ~fold t ~compare
