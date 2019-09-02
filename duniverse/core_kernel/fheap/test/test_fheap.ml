open! Core_kernel
open! Fheap

let%test_module _ =
  (module struct
    module type Heap_intf = sig
      type 'a t [@@deriving sexp_of]

      val create : cmp:('a -> 'a -> int) -> 'a t
      val add : 'a t -> 'a -> 'a t
      val pop : 'a t -> ('a * 'a t) option
      val length : 'a t -> int
      val top : 'a t -> 'a option
      val remove_top : 'a t -> 'a t option
      val of_list : 'a list -> cmp:('a -> 'a -> int) -> 'a t
      val to_list : 'a t -> 'a list

      val sum
        :  (module Container.Summable with type t = 'sum)
        -> 'a t
        -> f:('a -> 'sum)
        -> 'sum
    end

    module That_heap : Heap_intf = struct
      type 'a t =
        { cmp : 'a -> 'a -> int
        ; heap : 'a list
        }

      let sexp_of_t sexp_of_v t = List.sexp_of_t sexp_of_v t.heap
      let create ~cmp = { cmp; heap = [] }
      let add t v = { cmp = t.cmp; heap = List.sort ~compare:t.cmp (v :: t.heap) }

      let pop t =
        match t.heap with
        | [] -> None
        | x :: xs -> Some (x, { cmp = t.cmp; heap = xs })
      ;;

      let length t = List.length t.heap
      let top t = List.hd t.heap

      let remove_top t =
        match t.heap with
        | [] -> None
        | _ :: xs -> Some { cmp = t.cmp; heap = xs }
      ;;

      let of_list l ~cmp = { cmp; heap = List.sort ~compare:cmp l }
      let to_list t = t.heap
      let sum m t ~f = List.sum m (to_list t) ~f
    end

    module This_heap : Heap_intf = struct
      type nonrec 'a t = 'a t [@@deriving sexp_of]

      let create ~cmp = create ~cmp
      let add = add
      let pop = pop
      let length = length
      let top = top
      let remove_top = remove_top
      let of_list = of_list
      let to_list = to_list
      let sum = sum
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
      else t_a, t_b
    ;;

    let create () =
      let cmp = Int.compare in
      This_heap.create ~cmp, That_heap.create ~cmp
    ;;

    let add (this_t, that_t) v =
      let this_t = This_heap.add this_t v in
      let that_t = That_heap.add that_t v in
      length_check (this_t, that_t)
    ;;

    let pop (this_t, that_t) =
      let res1 = This_heap.pop this_t in
      let res2 = That_heap.pop that_t in
      let f r default =
        match r with
        | None -> None, default
        | Some (r, t) -> Some r, t
      in
      let defaults = create () in
      let res1, this_t = f res1 (fst defaults) in
      let res2, that_t = f res2 (snd defaults) in
      if Poly.( <> ) res1 res2
      then
        failwithf
          "pop results differ (%s, %s)"
          (Option.value_map ~default:"None" ~f:Int.to_string res1)
          (Option.value_map ~default:"None" ~f:Int.to_string res2)
          ()
      else this_t, that_t
    ;;

    let top (this_t, that_t) =
      let res1 = This_heap.top this_t in
      let res2 = That_heap.top that_t in
      if Poly.( <> ) res1 res2
      then
        failwithf
          "top results differ (%s, %s)"
          (Option.value_map ~default:"None" ~f:Int.to_string res1)
          (Option.value_map ~default:"None" ~f:Int.to_string res2)
          ()
      else this_t, that_t
    ;;

    let remove_top (this_t, that_t) =
      let this_t = This_heap.remove_top this_t in
      let that_t = That_heap.remove_top that_t in
      let cmp = Int.compare in
      let this_default = This_heap.create ~cmp in
      let that_default = That_heap.create ~cmp in
      let this_t = Option.value ~default:this_default this_t in
      let that_t = Option.value ~default:that_default that_t in
      length_check (this_t, that_t)
    ;;

    let of_list l ~cmp =
      let this_t = This_heap.of_list l ~cmp in
      let that_t = That_heap.of_list l ~cmp in
      length_check (this_t, that_t)
    ;;

    let check (this_t, that_t) =
      let this_list = List.sort ~compare:Int.compare (This_heap.to_list this_t) in
      let that_list = List.sort ~compare:Int.compare (That_heap.to_list that_t) in
      [%test_eq: int list] this_list that_list
    ;;

    let check_sum (this_t, that_t) =
      let this_sum = This_heap.sum (module Int) ~f:Fn.id this_t in
      let that_sum = That_heap.sum (module Int) ~f:Fn.id that_t in
      [%test_eq: int] this_sum that_sum;
      this_sum
    ;;

    let%test_unit _ =
      let t = create () in
      let random = Random.State.make [| 4 |] in
      let rec loop ops dual =
        if ops = 0
        then ()
        else (
          let r = Random.State.int random 100 in
          let new_dual =
            if r < 30
            then add dual (Random.State.int random 100_000)
            else if r < 70
            then pop dual
            else if r < 80
            then top dual
            else if r < 90
            then remove_top dual
            else (
              check dual;
              dual)
          in
          loop (ops - 1) new_dual)
      in
      loop 10_000 t
    ;;

    let%test_unit _ =
      let l = List.init 10_000 ~f:(fun _ -> Random.int 100_000) in
      let dual = of_list ~cmp:Int.compare l in
      check dual;
      let sum0 = check_sum dual in
      let dual = add dual (-100) in
      let sum1 = check_sum dual in
      [%test_eq: int] (sum0 - 100) sum1
    ;;
  end)
;;

let%test_unit _ =
  let data = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
  let h = of_list data ~cmp:Int.compare in
  let top_value, t = pop_exn h in
  [%test_result: int] ~expect:0 top_value;
  let list_sum = List.sum (module Int) data ~f:Fn.id in
  let heap_fold_sum = fold t ~init:0 ~f:(fun sum v -> sum + v) in
  let heap_iter_sum =
    let r = ref 0 in
    iter t ~f:(fun v -> r := !r + v);
    !r
  in
  [%test_eq: int] list_sum heap_fold_sum;
  [%test_eq: int] list_sum heap_iter_sum
;;

let%test_unit _ =
  let data = [ 0; 1; 2; 3; 4; 5; 6; 7 ] in
  let t = of_list data ~cmp:Int.compare in
  let s = sum (module Int) t ~f:Fn.id in
  [%test_result: int] ~expect:28 s;
  let t = add t 8 in
  let top_value = top_exn t in
  [%test_result: int] ~expect:0 top_value;
  let top_value, t = pop_exn t in
  [%test_result: int] ~expect:0 top_value;
  [%test_result: int] ~expect:1 (top_exn t);
  let len = length t in
  [%test_result: int] ~expect:8 len
;;
