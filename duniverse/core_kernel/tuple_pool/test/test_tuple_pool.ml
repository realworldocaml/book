open! Core_kernel
open! Int.Replace_polymorphic_compare

module Pool = Tuple_pool

let _make_sure_pool_pointer_is_an_int x = (x : _ Pool.Pointer.t :> int)

module Make (Pool : Pool.S) = struct
  module Pointer = Pool.Pointer

  let%test_unit "get_tuple with length = 1" =
    let pool = Pool.create Pool.Slots.t1 ~capacity:10 ~dummy:0 in
    let ptr = Pool.new1 pool 42 in
    let tuple = Pool.get_tuple pool ptr in
    assert (tuple = 42)
  ;;

  let%test_unit "get_tuple with length > 1" =
    let pool = Pool.create Pool.Slots.t3 ~capacity:10 ~dummy:(0, 0, 0) in
    let ptr = Pool.new3 pool 42 42 42 in
    let tuple = Pool.get_tuple pool ptr in
    assert (Poly.equal tuple (42, 42, 42))
  ;;

  module rec List_ : sig
    module Pool : sig
      type 'a t [@@deriving sexp_of]

      val create : capacity:int -> dummy:'a -> 'a t
      val length : _ t -> int
      val is_full : _ t -> bool
      val grow : ?capacity:int -> 'a t -> 'a t
    end

    type 'a t [@@deriving sexp_of]

    val nil : unit -> _ t
    val is_nil : _ t -> bool
    val create : 'a Pool.t -> 'a -> 'a t -> 'a t
    val free : 'a Pool.t -> 'a t -> unit
    val head : 'a Pool.t -> 'a t -> 'a
    val tail : 'a Pool.t -> 'a t -> 'a t
    val get : 'a Pool.t -> 'a t -> ('a * 'a t) option
    val equal : 'a t -> 'a t -> bool

    module Id : module type of Pointer.Id

    val id_of_pointer : 'a Pool.t -> 'a t -> Id.t
    val pointer_of_id_exn : 'a Pool.t -> Id.t -> 'a t
  end = struct
    type 'a ty = ('a, 'a ty Pointer.t) Pool.Slots.t2 [@@deriving sexp_of]
    type 'a t = 'a ty Pointer.t [@@deriving sexp_of]

    let create = Pool.new2
    let free = Pool.free
    let nil = Pointer.null
    let is_nil = Pointer.is_null
    let equal = Pointer.phys_equal
    let head p t = Pool.get p t Pool.Slot.t0
    let tail p t = Pool.get p t Pool.Slot.t1
    let get t p = if is_nil p then None else Some (Pool.get_tuple t p)

    module Id = Pointer.Id

    let id_of_pointer = Pool.id_of_pointer
    let pointer_of_id_exn = Pool.pointer_of_id_exn

    module Pool = struct
      type 'a t = 'a ty Pool.t [@@deriving sexp_of]

      let create ~capacity ~dummy =
        Pool.create Pool.Slots.t2 ~capacity ~dummy:(dummy, nil ())
      ;;

      let is_full = Pool.is_full
      let grow = Pool.grow
      let length = Pool.length
    end
  end

  open List_

  (* [create] with invalid capacity *)
  let%test_unit _ =
    List.iter [ -1 ] ~f:(fun capacity ->
      assert (
        Result.is_error
          (Result.try_with (fun () -> ignore (Pool.create ~capacity ~dummy:())))))
  ;;

  (* [length] *)
  let%test_unit _ =
    let t = Pool.create ~capacity:3 ~dummy:13 in
    assert (Pool.length t = 0);
    let l1 = create t 13 (nil ()) in
    assert (Pool.length t = 1);
    let l2 = create t 13 (nil ()) in
    assert (Pool.length t = 2);
    free t l1;
    assert (Pool.length t = 1);
    free t l2;
    assert (Pool.length t = 0)
  ;;

  let rec grow_loop p num_left =
    if num_left > 0 then grow_loop (Pool.grow p) (num_left - 1)
  ;;

  (* [grow] an empty pool *)
  let%test_unit _ = grow_loop (Pool.create ~capacity:1 ~dummy:()) 10

  (* [grow] a non-empty pool *)
  let%test_unit _ =
    let p = Pool.create ~capacity:1 ~dummy:0 in
    ignore (create p 13 (nil ()));
    grow_loop p 10
  ;;

  (* [grow] a non-empty pool, while adding each time *)
  let%test_unit _ =
    let rec loop p num_left =
      if num_left > 0
      then (
        ignore (create p 13 (nil ()));
        loop (Pool.grow p) (num_left - 1))
    in
    loop (Pool.create ~capacity:1 ~dummy:0) 10
  ;;

  let rec fold p list ~init ~f =
    if is_nil list then init else fold p (tail p list) ~init:(f init (head p list)) ~f
  ;;

  let to_list p list = List.rev (fold p list ~init:[] ~f:(fun l a -> a :: l))

  (* [grow] on demand *)
  let%test_unit (_[@tags "no-js"]) =
    let total_length = 3_000 in
    let rec loop i p list =
      let i = i - 1 in
      if i < 0
      then assert (Poly.equal (to_list p list) (List.init total_length ~f:Fn.id))
      else (
        let p = if not (Pool.is_full p) then p else Pool.grow p in
        loop i p (create p i list))
    in
    loop total_length (Pool.create ~capacity:0 ~dummy:0) (nil ())
  ;;

  (* [free] *)
  let%test_unit _ =
    let n = 10 in
    let p = Pool.create ~capacity:n ~dummy:0 in
    for _ = 1 to 4 do
      let ls = List.init n ~f:(fun i -> create p i (nil ())) in
      assert (Pool.is_full p);
      List.iter ls ~f:(fun l -> free p l)
    done
  ;;

  (* [free] *)
  let%test_unit _ =
    let rec loop p num_iters_left num_to_alloc_this_iter live =
      if num_iters_left = 0
      then List.iter live ~f:(fun l -> free p l)
      else (
        let p, live =
          List.fold
            (List.init num_to_alloc_this_iter ~f:Fn.id)
            ~init:(p, live)
            ~f:(fun (p, live) i ->
              let p = if Pool.is_full p then Pool.grow p else p in
              p, create p i (nil ()) :: live)
        in
        let to_free, live =
          let r = ref true in
          List.partition_map live ~f:(fun a ->
            r := not !r;
            if !r then First a else Second a)
        in
        List.iter to_free ~f:(fun l -> free p l);
        loop p (num_iters_left - 1) (num_to_alloc_this_iter * 2) live)
    in
    loop (Pool.create ~capacity:1 ~dummy:0) 10 1 []
  ;;

  (* [get_tuple] *)
  let%test_unit _ =
    let p = Pool.create ~capacity:10 ~dummy:0 in
    let l = create p 13 (nil ()) in
    let z1 = Option.value_exn (get p l) in
    let z2 = 13, nil () in
    assert (Poly.equal z1 z2)
  ;;

  (* [get] *)
  let%test_unit _ =
    let p = Pool.create ~capacity:10 ~dummy:0 in
    try
      let l = create p 13 (nil ()) in
      assert (not (is_nil l));
      assert (head p l = 13);
      assert (is_nil (tail p l));
      free p l
    with
    | exn -> failwiths ~here:[%here] "failure" (exn, p) [%sexp_of: exn * _ Pool.t]
  ;;

  (* [sexp_of] *)
  let%test_unit _ =
    let sexp_of = [%sexp_of: int t] in
    ignore (sexp_of (nil ()));
    let p = Pool.create ~capacity:10 ~dummy:0 in
    try
      let l = create p 13 (nil ()) in
      ignore (sexp_of l)
    with
    | exn -> failwiths ~here:[%here] "failure" (exn, p) [%sexp_of: exn * _ Pool.t]
  ;;

  (* [id_of_pointer], [pointer_of_id_exn], [Id.to_int63], [Id.of_int63] *)
  let%test_unit _ =
    let capacity = 10 in
    let p = Pool.create ~capacity ~dummy:0 in
    let id_of_pointer_via_int63 l = Id.of_int63 (Id.to_int63 (id_of_pointer p l)) in
    let does_round_trip l =
      let id = id_of_pointer_via_int63 l in
      equal l (pointer_of_id_exn p id)
    in
    assert (does_round_trip (nil ()));
    let alloc_all () = Array.init capacity ~f:(fun _ -> create p 13 (nil ())) in
    let ls = alloc_all () in
    assert (Array.for_all ls ~f:does_round_trip);
    Array.iter ls ~f:(fun l -> free p l);
    let all_ls_fail () =
      Array.for_all ls ~f:(fun l ->
        Exn.does_raise (fun () -> pointer_of_id_exn p (id_of_pointer_via_int63 l)))
    in
    assert (all_ls_fail ());
    let _ls' = alloc_all () in
    assert (all_ls_fail ())
  ;;
end

let%test_module _ = (module Make (Pool))

open! Pool

let%expect_test "use a pool that has been [grow]n" =
  let t = create Slots.t1 ~capacity:1 ~dummy:13 in
  let pointer = new1 t 15 in
  let show_pointer () =
    print_s [%sexp (pointer_is_valid t pointer : bool), (get t pointer Slot.t0 : int)]
  in
  show_pointer ();
  [%expect {|
    (true 15) |}];
  ignore (grow t);
  show_pointer ();
  [%expect {|
    (false 13) |}]
;;

let%test_module _ =
  (module Make (struct
       include Pool.Unsafe

       let create (type tuple) (slots : (tuple, _) Slots.t) ~capacity ~dummy:(_ : tuple) =
         create slots ~capacity
       ;;
     end))
;;

let%test_module "Debug without messages" =
  (module Make (struct
       include Pool.Debug (Pool)

       let () = show_messages := false

       (* or it prints too much *)
     end))
;;

module Error_checked_pool = Pool.Error_check (Pool)

let%test_module _ = (module Make (Error_checked_pool))

open Error_checked_pool

let%expect_test "use a pool that has been [grow]n" =
  let t = create Slots.t1 ~capacity:1 ~dummy:13 in
  let pointer = new1 t 15 in
  let show_pointer () =
    print_s [%sexp (pointer_is_valid t pointer : bool), (get t pointer Slot.t0 : int)]
  in
  show_pointer ();
  [%expect {|
    (true 15) |}];
  ignore (grow t);
  show_pointer ();
  [%expect {|
    (false 13) |}]
;;

open Unsafe

let%expect_test "use a pool that has been [grow]n" =
  let t = create Slots.t1 ~capacity:1 in
  let pointer = new1 t 15 in
  let show_pointer () =
    print_s [%sexp (pointer_is_valid t pointer : bool), (get t pointer Slot.t0 : int)]
  in
  show_pointer ();
  [%expect {|
    (true 15) |}];
  ignore (grow t);
  show_pointer ();
  [%expect {|
    (false 0) |}]
;;
