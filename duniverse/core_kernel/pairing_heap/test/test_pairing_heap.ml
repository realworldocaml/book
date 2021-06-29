open! Core_kernel
open Poly
open Expect_test_helpers_core
module Heap = Pairing_heap

let%expect_test "Heap.sexp_of_t" =
  let test list =
    let heap = Heap.of_list list ~cmp:Int.compare in
    print_s [%sexp (heap : int Heap.t)];
    (* test for stability of element order in sexps, and make sure [sexp_of_t] does not
       accidentally mutate [t] *)
    while not (Heap.is_empty heap) do
      ignore (Heap.pop_exn heap : int);
      print_s [%sexp (heap : int Heap.t)]
    done
  in
  test [];
  [%expect {| () |}];
  test [ 3 ];
  [%expect {|
    (3)
    () |}];
  test [ 3; 1; 4 ];
  [%expect {|
    (1 3 4)
    (3 4)
    (4)
    () |}];
  test [ 3; 1; 4; 1; 5; 9; 2; 6; 5 ];
  [%expect
    {|
    (1 1 2 3 4 5 5 6 9)
    (1 2 3 4 5 5 6 9)
    (2 3 4 5 5 6 9)
    (3 4 5 5 6 9)
    (4 5 5 6 9)
    (5 5 6 9)
    (5 6 9)
    (6 9)
    (9)
    () |}]
;;

let%expect_test "Heap.sexp_of_t with removes" =
  let test list =
    let heap = Heap.create ~cmp:Int.compare () in
    let elts = List.map list ~f:(Heap.add_removable heap) in
    print_s [%sexp (heap : int Heap.t)];
    (* test for stability of element order in sexps, and make sure [sexp_of_t] does not
       accidentally mutate [t] *)
    List.iter elts ~f:(fun elt ->
      Heap.remove heap elt;
      print_s [%sexp (heap : int Heap.t)])
  in
  test [];
  [%expect {|
    () |}];
  test [ 3 ];
  [%expect {|
    (3)
    () |}];
  test [ 3; 1; 4 ];
  [%expect {|
    (1 3 4)
    (1 4)
    (4)
    () |}];
  test [ 3; 1; 4; 1; 5; 9; 2; 6; 5 ];
  [%expect
    {|
    (1 1 2 3 4 5 5 6 9)
    (1 1 2 4 5 5 6 9)
    (1 2 4 5 5 6 9)
    (1 2 5 5 6 9)
    (2 5 5 6 9)
    (2 5 6 9)
    (2 5 6)
    (5 6)
    (5)
    () |}]
;;

open! Heap

let%test_module _ =
  (module struct
    let data = [ 0; 1; 2; 3; 4; 5; 6; 7 ]
    let t = of_list data ~cmp:Int.compare
    let () = invariant Fn.ignore t

    (* pop the zero at the top to force some heap structuring.  This does not touch the
       sum. *)
    let (_ : int option) = pop t
    let () = invariant Fn.ignore t
    let list_sum = List.fold data ~init:0 ~f:(fun sum v -> sum + v)
    let heap_fold_sum = fold t ~init:0 ~f:(fun sum v -> sum + v)

    let heap_iter_sum =
      let r = ref 0 in
      iter t ~f:(fun v -> r := !r + v);
      !r
    ;;

    let%test _ = Int.( = ) list_sum heap_fold_sum
    let%test _ = Int.( = ) list_sum heap_iter_sum
  end)
;;

let%test_module _ =
  (module struct
    module type Heap_intf = sig
      type 'a t [@@deriving sexp_of]

      include Invariant.S1 with type 'a t := 'a t

      val create : cmp:('a -> 'a -> int) -> 'a t
      val add : 'a t -> 'a -> unit
      val pop : 'a t -> 'a option
      val length : 'a t -> int
      val top : 'a t -> 'a option
      val remove_top : 'a t -> unit
      val to_list : 'a t -> 'a list
    end

    module That_heap : Heap_intf = struct
      type 'a t =
        { cmp : 'a -> 'a -> int
        ; mutable heap : 'a list
        }

      let sexp_of_t sexp_of_v t = List.sexp_of_t sexp_of_v t.heap
      let create ~cmp = { cmp; heap = [] }
      let add t v = t.heap <- List.sort ~compare:t.cmp (v :: t.heap)

      let pop t =
        match t.heap with
        | [] -> None
        | x :: xs ->
          t.heap <- xs;
          Some x
      ;;

      let length t = List.length t.heap
      let top t = List.hd t.heap

      let remove_top t =
        match t.heap with
        | [] -> ()
        | _ :: xs -> t.heap <- xs
      ;;

      let to_list t = t.heap
      let invariant _ = Fn.ignore
    end

    module This_heap : Heap_intf = struct
      type nonrec 'a t = 'a t [@@deriving sexp_of]

      let create ~cmp = create ~cmp ()
      let add = add
      let pop = pop
      let length = length
      let top = top
      let remove_top = remove_top
      let to_list = to_list
      let invariant = invariant
    end

    let this_to_string this = Sexp.to_string (This_heap.sexp_of_t Int.sexp_of_t this)
    let that_to_string that = Sexp.to_string (That_heap.sexp_of_t Int.sexp_of_t that)

    let length_check (t_a, t_b) =
      let this_len = This_heap.length t_a in
      let that_len = That_heap.length t_b in
      if this_len <> that_len
      then
        failwithf
          "error in length: %i (for %s) <> %i (for %s)"
          this_len
          (this_to_string t_a)
          that_len
          (that_to_string t_b)
          ()
    ;;

    let create () =
      let cmp = Int.compare in
      This_heap.create ~cmp, That_heap.create ~cmp
    ;;

    let add (this_t, that_t) v =
      This_heap.add this_t v;
      That_heap.add that_t v;
      length_check (this_t, that_t)
    ;;

    let pop (this_t, that_t) =
      let res1 = This_heap.pop this_t in
      let res2 = That_heap.pop that_t in
      if res1 <> res2
      then
        failwithf
          "pop results differ (%s, %s)"
          (Option.value ~default:"None" (Option.map ~f:Int.to_string res1))
          (Option.value ~default:"None" (Option.map ~f:Int.to_string res2))
          ()
    ;;

    let top (this_t, that_t) =
      let res1 = This_heap.top this_t in
      let res2 = That_heap.top that_t in
      if res1 <> res2
      then
        failwithf
          "top results differ (%s, %s)"
          (Option.value ~default:"None" (Option.map ~f:Int.to_string res1))
          (Option.value ~default:"None" (Option.map ~f:Int.to_string res2))
          ()
    ;;

    let remove_top (this_t, that_t) =
      This_heap.remove_top this_t;
      That_heap.remove_top that_t;
      length_check (this_t, that_t)
    ;;

    let internal_check (this_t, that_t) =
      let this_list = List.sort ~compare:Int.compare (This_heap.to_list this_t) in
      let that_list = List.sort ~compare:Int.compare (That_heap.to_list that_t) in
      assert (this_list = that_list);
      This_heap.invariant Fn.ignore this_t;
      That_heap.invariant Fn.ignore that_t
    ;;

    let test_dual_ops () =
      let t = create () in
      let rec loop ops =
        if ops = 0
        then ()
        else (
          let r = Random.int 100 in
          if r < 40
          then add t (Random.int 100_000)
          else if r < 70
          then pop t
          else if r < 80
          then top t
          else if r < 90
          then remove_top t
          else internal_check t;
          loop (ops - 1))
      in
      loop 1_000
    ;;

    let%test_unit _ = test_dual_ops ()
  end)
;;

let test_copy ~add_removable ~remove =
  let sum t = fold t ~init:0 ~f:(fun acc i -> acc + i) in
  let t = create ~cmp:Int.compare () in
  for i = 1 to 99 do
    add t i;
    if i % 10 = 0
    (* We need to pop from time to time to trigger the amortized tree reorganizations.  If
       we don't do this the resulting structure is just a linked list and the copy
       function is not flexed as completely as it should be. *)
    then (
      ignore (pop t);
      add t i)
  done;
  let token = add_removable t 100 in
  invariant Fn.ignore t;
  let t' = copy t in
  invariant Fn.ignore t';
  assert (sum t = sum t');
  assert (to_list t = to_list t');
  remove t token;
  assert (sum t = sum t' - 100)
;;

let%test_unit _ = test_copy ~add_removable ~remove
let%test_unit _ = test_copy ~add_removable:Unsafe.add_removable ~remove:Unsafe.remove

let test_removal ~add_removable ~remove ~elt_value_exn =
  let t = create ~cmp:Int.compare () in
  let tokens = ref [] in
  for i = 1 to 10_000 do
    tokens := add_removable t i :: !tokens
  done;
  invariant Fn.ignore t;
  List.iter !tokens ~f:(fun token ->
    if elt_value_exn token t % 2 <> 0 then remove t token);
  invariant Fn.ignore t;
  let rec loop count =
    if count % 1000 = 0 then invariant Fn.ignore t;
    match pop t with
    | None -> assert (count = 10_000 / 2)
    | Some v ->
      assert ((1 + count) * 2 = v);
      loop (count + 1)
  in
  loop 0
;;

let%test_unit _ =
  test_removal ~add_removable ~remove ~elt_value_exn:(fun token _ -> Elt.value_exn token)
;;

let%test_unit _ =
  test_removal
    ~add_removable:Unsafe.add_removable
    ~remove:Unsafe.remove
    ~elt_value_exn:Unsafe.Elt.value
;;

let test_ordering () =
  let t = create ~cmp:Int.compare () in
  for _ = 1 to 10_000 do
    add t (Random.int 100_000)
  done;
  let rec loop last count =
    if count % 1_000 = 0 then invariant Fn.ignore t;
    match pop t with
    | None -> ()
    | Some v ->
      assert (v >= last);
      loop v (count + 1)
  in
  loop (-1) 0
;;

let%test_unit _ = test_ordering ()
let%test_unit _ = ignore (of_array [||] ~cmp:Int.compare)

let%test_unit "operations on removed elements" =
  let h = create ~cmp:Int.compare () in
  let elt = add_removable h 1 in
  [%test_eq: string] (Sexp.to_string (Elt.sexp_of_t sexp_of_int elt)) "(1)";
  ignore (pop_exn h : int);
  assert (Result.is_error (Result.try_with (fun () -> Elt.value_exn elt)));
  [%test_eq: string] (Sexp.to_string (Elt.sexp_of_t sexp_of_int elt)) "()"
;;
