open! Base
open! Stack

module Debug (Stack : S) : S with type 'a t = 'a Stack.t = struct
  open Stack

  type nonrec 'a t = 'a t

  let invariant = invariant

  let check_and_return t =
    invariant ignore t;
    t
  ;;

  let debug t f =
    let result = Result.try_with f in
    invariant ignore t;
    Result.ok_exn result
  ;;

  (* The return-type annotations are to prevent an error where we don't supply all the
     arguments to the function, and thus wouldn't be checking the invariant after fully
     applying the function. *)
  let clear t : unit = debug t (fun () -> clear t)
  let copy t : _ t = check_and_return (debug t (fun () -> copy t))
  let count t ~f : int = debug t (fun () -> count t ~f)
  let sum m t ~f = debug t (fun () -> sum m t ~f)
  let create () : _ t = check_and_return (create ())
  let exists t ~f : bool = debug t (fun () -> exists t ~f)
  let find t ~f : _ option = debug t (fun () -> find t ~f)
  let find_map t ~f : _ option = debug t (fun () -> find_map t ~f)
  let fold (type a) t ~init ~f : a = debug t (fun () -> fold t ~init ~f)
  let for_all t ~f : bool = debug t (fun () -> for_all t ~f)
  let is_empty t : bool = debug t (fun () -> is_empty t)
  let iter t ~f : unit = debug t (fun () -> iter t ~f)
  let length t : int = debug t (fun () -> length t)
  let mem t a ~equal : bool = debug t (fun () -> mem t a ~equal)
  let of_list l : _ t = check_and_return (of_list l)
  let pop t : _ option = debug t (fun () -> pop t)
  let pop_exn (type a) t : a = debug t (fun () -> pop_exn t)
  let push t a : unit = debug t (fun () -> push t a)
  let sexp_of_t sexp_of_a t : Sexp.t = debug t (fun () -> [%sexp_of: a t] t)
  let singleton x : _ t = check_and_return (singleton x)
  let t_of_sexp a_of_sexp sexp : _ t = check_and_return ([%of_sexp: a t] sexp)
  let to_array t : _ array = debug t (fun () -> to_array t)
  let to_list t : _ list = debug t (fun () -> to_list t)
  let top t : _ option = debug t (fun () -> top t)
  let top_exn (type a) t : a = debug t (fun () -> top_exn t)
  let until_empty t f : unit = debug t (fun () -> until_empty t f)
  let min_elt t ~compare : _ option = debug t (fun () -> min_elt t ~compare)
  let max_elt t ~compare : _ option = debug t (fun () -> max_elt t ~compare)
  let fold_result t ~init ~f = debug t (fun () -> fold_result t ~init ~f)
  let fold_until t ~init ~f = debug t (fun () -> fold_until t ~init ~f)
end

module Test (Stack : S) : S with type 'a t = 'a Stack.t =
(* This signature is here to remind us to add a unit test whenever we add something to
   the stack interface. *)
struct
  open Stack

  type nonrec 'a t = 'a t

  include Test_container.Test_S1 (Stack)

  let invariant = invariant
  let create = create
  let is_empty = is_empty
  let top_exn = top_exn
  let pop_exn = pop_exn
  let pop = pop
  let top = top
  let singleton = singleton

  let%test_unit _ =
    let empty = create () in
    invariant ignore empty;
    invariant (fun b -> assert b) (of_list [ true ]);
    assert (is_empty empty);
    let t = create () in
    push t 0;
    assert (not (is_empty t));
    assert (Exn.does_raise (fun () -> top_exn empty));
    let t = create () in
    push t 0;
    [%test_result: int] (top_exn t) ~expect:0;
    assert (Exn.does_raise (fun () -> pop_exn empty));
    let t = create () in
    push t 0;
    [%test_result: int] (pop_exn t) ~expect:0;
    assert (Option.is_none (pop empty));
    assert (Option.is_some (pop (of_list [ 0 ])));
    assert (Option.is_none (top empty));
    assert (Option.is_some (top (of_list [ 0 ])));
    assert (Option.is_some (top (singleton 0)));
    assert (Option.is_some (pop (singleton 0)));
    assert (
      let t = singleton 0 in
      ignore (pop_exn t : int);
      Option.is_none (top t))
  ;;

  let min_elt = min_elt
  let max_elt = max_elt

  let%test_unit _ =
    let empty = create () in
    [%test_result: _ option] (min_elt ~compare:Int.compare empty) ~expect:None;
    [%test_result: _ option] (max_elt ~compare:Int.compare empty) ~expect:None;
    [%test_result: int] (sum (module Int) ~f:Fn.id empty) ~expect:0
  ;;

  let push = push
  let copy = copy
  let until_empty = until_empty

  let%test_unit _ =
    let t =
      let t = create () in
      push t 0;
      push t 1;
      push t 2;
      t
    in
    [%test_result: bool] (is_empty t) ~expect:false;
    [%test_result: int] (length t) ~expect:3;
    [%test_result: int option] (top t) ~expect:(Some 2);
    [%test_result: int] (top_exn t) ~expect:2;
    [%test_result: int option] (min_elt ~compare:Int.compare t) ~expect:(Some 0);
    [%test_result: int option] (max_elt ~compare:Int.compare t) ~expect:(Some 2);
    [%test_result: int] (sum (module Int) ~f:Fn.id t) ~expect:3;
    let t' = copy t in
    [%test_result: int] (pop_exn t') ~expect:2;
    [%test_result: int] (pop_exn t') ~expect:1;
    [%test_result: int] (pop_exn t') ~expect:0;
    [%test_result: int] (length t') ~expect:0;
    [%test_result: bool] (is_empty t') ~expect:true;
    let t' = copy t in
    [%test_result: int option] (pop t') ~expect:(Some 2);
    [%test_result: int option] (pop t') ~expect:(Some 1);
    [%test_result: int option] (pop t') ~expect:(Some 0);
    [%test_result: int] (length t') ~expect:0;
    [%test_result: bool] (is_empty t') ~expect:true;
    (* test that t was not modified by pops applied to copies *)
    [%test_result: int] (length t) ~expect:3;
    [%test_result: int] (top_exn t) ~expect:2;
    [%test_result: int list] (to_list t) ~expect:[ 2; 1; 0 ];
    [%test_result: int array] (to_array t) ~expect:[| 2; 1; 0 |];
    [%test_result: int] (length t) ~expect:3;
    [%test_result: int] (top_exn t) ~expect:2;
    let t' = copy t in
    let n = ref 0 in
    until_empty t' (fun x -> n := !n + x);
    [%test_result: int] !n ~expect:3;
    [%test_result: bool] (is_empty t') ~expect:true;
    [%test_result: int] (length t') ~expect:0
  ;;

  let%test_unit _ =
    let t = create () in
    [%test_result: bool] (is_empty t) ~expect:true;
    [%test_result: int] (length t) ~expect:0;
    [%test_result: _ list] (to_list t) ~expect:[];
    [%test_result: _ option] (pop t) ~expect:None;
    push t 13;
    [%test_result: bool] (is_empty t) ~expect:false;
    [%test_result: int] (length t) ~expect:1;
    [%test_result: int option] (min_elt ~compare:Int.compare t) ~expect:(Some 13);
    [%test_result: int option] (max_elt ~compare:Int.compare t) ~expect:(Some 13);
    [%test_result: int] (sum (module Int) ~f:Fn.id t) ~expect:13;
    [%test_result: int] (pop_exn t) ~expect:13;
    [%test_result: bool] (is_empty t) ~expect:true;
    [%test_result: int] (length t) ~expect:0;
    push t 13;
    push t 14;
    [%test_result: bool] (is_empty t) ~expect:false;
    [%test_result: int] (length t) ~expect:2;
    [%test_result: int list] (to_list t) ~expect:[ 14; 13 ];
    [%test_result: int option] (min_elt ~compare:Int.compare t) ~expect:(Some 13);
    [%test_result: int option] (max_elt ~compare:Int.compare t) ~expect:(Some 14);
    [%test_result: int] (sum (module Int) ~f:Fn.id t) ~expect:27;
    [%test_result: bool] (Option.is_some (pop t)) ~expect:true;
    [%test_result: bool] (Option.is_some (pop t)) ~expect:true
  ;;

  let of_list = of_list

  let%test_unit _ =
    for n = 0 to 5 do
      let l = List.init n ~f:Fn.id in
      [%test_result: int list] (to_list (of_list l)) ~expect:l
    done
  ;;

  let clear = clear

  let%test_unit _ =
    for n = 0 to 5 do
      let t = of_list (List.init n ~f:Fn.id) in
      clear t;
      assert (is_empty t);
      push t 13;
      [%test_result: int] (length t) ~expect:1
    done
  ;;

  let%test_unit "float test" =
    let s = create () in
    push s 1.0;
    push s 2.0;
    push s 3.0
  ;;
end
