open! Base
open! Container

module Test_generic (Elt : sig
    type 'a t

    val of_int : int -> int t
    val to_int : int t -> int
  end) (Container : sig
          type 'a t [@@deriving sexp]

          include Generic with type 'a t := 'a t with type 'a elt := 'a Elt.t

          val mem : 'a t -> 'a Elt.t -> equal:('a Elt.t -> 'a Elt.t -> bool) -> bool
          val of_list : 'a Elt.t list -> [ `Ok of 'a t | `Skip_test ]
        end) : sig
  type 'a t [@@deriving sexp]

  include Generic with type 'a t := 'a t

  val mem : 'a t -> 'a Elt.t -> equal:('a Elt.t -> 'a Elt.t -> bool) -> bool
end
with type 'a t := 'a Container.t
with type 'a elt := 'a Elt.t =
(* This signature constraint reminds us to add unit tests when functions are added to
   [Generic]. *)
struct
  open Container

  let find = find
  let find_map = find_map
  let fold = fold
  let is_empty = is_empty
  let iter = iter
  let length = length
  let mem = mem
  let sexp_of_t = sexp_of_t
  let t_of_sexp = t_of_sexp
  let to_array = to_array
  let to_list = to_list
  let fold_result = fold_result
  let fold_until = fold_until

  let%test_unit _ =
    let ( = ) = Poly.equal in
    let compare = Poly.compare in
    List.iter [ 0; 1; 2; 3; 4; 8; 1024 ] ~f:(fun n ->
      let list = List.init n ~f:Elt.of_int in
      match Container.of_list list with
      | `Skip_test -> ()
      | `Ok c ->
        let sort l = List.sort l ~compare in
        let sorts_are_equal l1 l2 = sort l1 = sort l2 in
        assert (n = Container.length c);
        assert (n = 0 = Container.is_empty c);
        assert (
          sorts_are_equal list (Container.fold c ~init:[] ~f:(fun ac e -> e :: ac)));
        assert (sorts_are_equal list (Container.to_list c));
        assert (sorts_are_equal list (Array.to_list (Container.to_array c)));
        assert (
          n > 0 = Option.is_some (Container.find c ~f:(fun e -> Elt.to_int e = 0)));
        assert (
          n > 0 = Option.is_some (Container.find c ~f:(fun e -> Elt.to_int e = n - 1))
        );
        assert (Option.is_none (Container.find c ~f:(fun e -> Elt.to_int e = n)));
        assert (n > 0 = Container.mem c (Elt.of_int 0) ~equal:( = ));
        assert (n > 0 = Container.mem c (Elt.of_int (n - 1)) ~equal:( = ));
        assert (not (Container.mem c (Elt.of_int n) ~equal:( = )));
        assert (
          n
          > 0
          = Option.is_some
              (Container.find_map c ~f:(fun e ->
                 if Elt.to_int e = 0 then Some () else None)));
        assert (
          n
          > 0
          = Option.is_some
              (Container.find_map c ~f:(fun e ->
                 if Elt.to_int e = n - 1 then Some () else None)));
        assert (
          Option.is_none
            (Container.find_map c ~f:(fun e ->
               if Elt.to_int e = n then Some () else None)));
        let r = ref 0 in
        Container.iter c ~f:(fun e -> r := !r + Elt.to_int e);
        assert (!r = List.fold list ~init:0 ~f:(fun n e -> n + Elt.to_int e));
        assert (!r = sum (module Int) c ~f:Elt.to_int);
        let c2 = [%of_sexp: int Container.t] ([%sexp_of: int Container.t] c) in
        assert (sorts_are_equal list (Container.to_list c2));
        let compare_elt a b = Int.compare (Elt.to_int a) (Elt.to_int b) in
        if n = 0
        then (
          assert (!r = 0);
          assert (min_elt ~compare:compare_elt c = None);
          assert (max_elt ~compare:compare_elt c = None))
        else (
          assert (!r = n * (n - 1) / 2);
          assert (Option.map ~f:Elt.to_int (min_elt ~compare:compare_elt c) = Some 0);
          assert (
            Option.map ~f:Elt.to_int (max_elt ~compare:compare_elt c)
            = Some (Int.pred n)));
        let mid = Container.length c / 2 in
        (match
           Container.fold_result c ~init:0 ~f:(fun count _elt ->
             if count = mid then Error count else Ok (count + 1))
         with
         | Ok 0 -> assert (Container.length c = 0)
         | Ok _ -> failwith "Expected fold to stop early"
         | Error x -> assert (mid = x)))
  ;;

  let min_elt = min_elt
  let max_elt = max_elt
  let count = count
  let sum = sum
  let exists = exists
  let for_all = for_all

  let%test_unit _ =
    List.iter
      [ []
      ; [ true ]
      ; [ false ]
      ; [ false; false ]
      ; [ true; false ]
      ; [ false; true ]
      ; [ true; true ]
      ]
      ~f:(fun bools ->
        let count_should_be =
          List.fold bools ~init:0 ~f:(fun n b -> if b then n + 1 else n)
        in
        let forall_should_be = List.fold bools ~init:true ~f:(fun ac b -> b && ac) in
        let exists_should_be = List.fold bools ~init:false ~f:(fun ac b -> b || ac) in
        match
          Container.of_list
            (List.map bools ~f:(fun b -> Elt.of_int (if b then 1 else 0)))
        with
        | `Skip_test -> ()
        | `Ok container ->
          let is_one e = Elt.to_int e = 1 in
          let ( = ) = Poly.equal in
          assert (forall_should_be = Container.for_all container ~f:is_one);
          assert (exists_should_be = Container.exists container ~f:is_one);
          assert (count_should_be = Container.count container ~f:is_one))
  ;;
end

module Test_S1_allow_skipping_tests (Container : sig
    type 'a t [@@deriving sexp]

    include Container.S1 with type 'a t := 'a t

    val of_list : 'a list -> [ `Ok of 'a t | `Skip_test ]
  end) =
struct
  include Test_generic
      (struct
        type 'a t = 'a

        let of_int = Fn.id
        let to_int = Fn.id
      end)
      (Container)
end

module Test_S1 (Container : sig
    type 'a t [@@deriving sexp]

    include Container.S1 with type 'a t := 'a t

    val of_list : 'a list -> 'a t
  end) =
  Test_S1_allow_skipping_tests (struct
    include Container

    let of_list l = `Ok (of_list l)
  end)
