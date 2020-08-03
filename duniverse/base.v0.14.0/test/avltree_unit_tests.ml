open! Import

let%test_module _ =
  (module (
   struct
     open Avltree

     type ('k, 'v) t = ('k, 'v) Avltree.t = private
       | Empty
       | Node of
           { mutable left : ('k, 'v) t
           ; key : 'k
           ; mutable value : 'v
           ; mutable height : int
           ; mutable right : ('k, 'v) t
           }
       | Leaf of
           { key : 'k
           ; mutable value : 'v
           }

     module For_quickcheck = struct
       module Key = struct
         include Int

         type t = int [@@deriving quickcheck]

         let quickcheck_generator =
           Base_quickcheck.Generator.small_positive_or_zero_int
         ;;
       end

       module Data = struct
         include String

         type t = string [@@deriving quickcheck]

         let quickcheck_generator =
           Base_quickcheck.Generator.string_of
             Base_quickcheck.Generator.char_lowercase
         ;;
       end

       let compare = Key.compare

       module Constructor = struct
         type t =
           | Add of Key.t * Data.t
           | Replace of Key.t * Data.t
           | Remove of Key.t
         [@@deriving quickcheck, sexp_of]

         let apply_to_tree t tree =
           match t with
           | Add (key, data) ->
             add tree ~key ~data ~compare ~added:(ref false) ~replace:false
           | Replace (key, data) ->
             add tree ~key ~data ~compare ~added:(ref false) ~replace:true
           | Remove key -> remove tree key ~compare ~removed:(ref false)
         ;;

         let apply_to_map t map =
           match t with
           | Add (key, data) ->
             if Map.mem map key then map else Map.set map ~key ~data
           | Replace (key, data) -> Map.set map ~key ~data
           | Remove key -> Map.remove map key
         ;;
       end

       module Constructors = struct
         type t = Constructor.t list [@@deriving quickcheck, sexp_of]
       end

       let reify constructors =
         List.fold
           constructors
           ~init:(empty, Map.empty (module Key))
           ~f:(fun (t, map) constructor ->
             ( Constructor.apply_to_tree constructor t
             , Constructor.apply_to_map constructor map ))
       ;;

       let merge map1 map2 =
         Map.merge map1 map2 ~f:(fun ~key variant ->
           match variant with
           | `Left data | `Right data -> Some data
           | `Both (data1, data2) ->
             Error.raise_s
               [%message
                 "duplicate data for key"
                   (key : Key.t)
                   (data1 : Data.t)
                   (data2 : Data.t)])
       ;;

       let rec to_map = function
         | Empty -> Map.empty (module Key)
         | Leaf { key; value = data } -> Map.singleton (module Key) key data
         | Node { left; key; value = data; height = _; right } ->
           merge
             (Map.singleton (module Key) key data)
             (merge (to_map left) (to_map right))
       ;;
     end

     open For_quickcheck

     let empty = empty

     let%test_unit _ =
       match empty with
       | Empty -> ()
       | _ -> assert false
     ;;

     let is_empty = is_empty

     let%test _ = is_empty empty

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module Constructors)
         ~f:(fun constructors ->
           let t, map = reify constructors in
           [%test_result: bool] (is_empty t) ~expect:(Map.is_empty map))
     ;;

     let invariant = invariant

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module Constructors)
         ~f:(fun constructors ->
           let t, map = reify constructors in
           invariant t ~compare;
           [%test_result: Data.t Map.M(Key).t] (to_map t) ~expect:map)
     ;;

     let add = add

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module struct
           type t = Constructor.t list * Key.t * Data.t * bool
           [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun (constructors, key, data, replace) ->
           let t, map = reify constructors in
           (* test [added], other aspects of [add] are tested via [reify] in the
              [invariant] test above *)
           let added = ref false in
           let (_ : (Key.t, Data.t) t) =
             add t ~key ~data ~compare ~added ~replace
           in
           [%test_result: bool] !added ~expect:(not (Map.mem map key)))
     ;;

     let remove = remove

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module struct
           type t = Constructors.t * Key.t [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun (constructors, key) ->
           let t, map = reify constructors in
           (* test [removed], other aspects of [remove] are tested via [reify] in the
              [invariant] test above *)
           let removed = ref false in
           let (_ : (Key.t, Data.t) t) = remove t key ~compare ~removed in
           [%test_result: bool] !removed ~expect:(Map.mem map key))
     ;;

     let find = find

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module struct
           type t = Constructors.t * Key.t [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun (constructors, key) ->
           let t, map = reify constructors in
           [%test_result: Data.t option]
             (find t key ~compare)
             ~expect:(Map.find map key))
     ;;

     let mem = mem

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module struct
           type t = Constructors.t * Key.t [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun (constructors, key) ->
           let t, map = reify constructors in
           [%test_result: bool] (mem t key ~compare) ~expect:(Map.mem map key))
     ;;

     let first = first

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module Constructors)
         ~f:(fun constructors ->
           let t, map = reify constructors in
           [%test_result: (Key.t * Data.t) option]
             (first t)
             ~expect:(Map.min_elt map))
     ;;

     let last = last

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module Constructors)
         ~f:(fun constructors ->
           let t, map = reify constructors in
           [%test_result: (Key.t * Data.t) option]
             (last t)
             ~expect:(Map.max_elt map))
     ;;

     let find_and_call = find_and_call

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module struct
           type t = Constructors.t * Key.t [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun (constructors, key) ->
           let t, map = reify constructors in
           [%test_result: [ `Found of Data.t | `Not_found of Key.t ]]
             (find_and_call
                t
                key
                ~compare
                ~if_found:(fun data -> `Found data)
                ~if_not_found:(fun key -> `Not_found key))
             ~expect:
               (match Map.find map key with
                | None -> `Not_found key
                | Some data -> `Found data))
     ;;

     let findi_and_call = findi_and_call

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module struct
           type t = Constructors.t * Key.t [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun (constructors, key) ->
           let t, map = reify constructors in
           [%test_result: [ `Found of Key.t * Data.t | `Not_found of Key.t ]]
             (findi_and_call
                t
                key
                ~compare
                ~if_found:(fun ~key ~data -> `Found (key, data))
                ~if_not_found:(fun key -> `Not_found key))
             ~expect:
               (match Map.find map key with
                | None -> `Not_found key
                | Some data -> `Found (key, data)))
     ;;

     let find_and_call1 = find_and_call1

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module struct
           type t = Constructors.t * Key.t * int
           [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun (constructors, key, a) ->
           let t, map = reify constructors in
           [%test_result:
             [ `Found of Data.t * int | `Not_found of Key.t * int ]]
             (find_and_call1
                t
                key
                ~compare
                ~a
                ~if_found:(fun data a -> `Found (data, a))
                ~if_not_found:(fun key a -> `Not_found (key, a)))
             ~expect:
               (match Map.find map key with
                | None -> `Not_found (key, a)
                | Some data -> `Found (data, a)))
     ;;

     let findi_and_call1 = findi_and_call1

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module struct
           type t = Constructors.t * Key.t * int
           [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun (constructors, key, a) ->
           let t, map = reify constructors in
           [%test_result:
             [ `Found of Key.t * Data.t * int | `Not_found of Key.t * int ]]
             (findi_and_call1
                t
                key
                ~compare
                ~a
                ~if_found:(fun ~key ~data a -> `Found (key, data, a))
                ~if_not_found:(fun key a -> `Not_found (key, a)))
             ~expect:
               (match Map.find map key with
                | None -> `Not_found (key, a)
                | Some data -> `Found (key, data, a)))
     ;;

     let find_and_call2 = find_and_call2

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module struct
           type t = Constructors.t * Key.t * int * string
           [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun (constructors, key, a, b) ->
           let t, map = reify constructors in
           [%test_result:
             [ `Found of Data.t * int * string
             | `Not_found of Key.t * int * string
             ]]
             (find_and_call2
                t
                key
                ~compare
                ~a
                ~b
                ~if_found:(fun data a b -> `Found (data, a, b))
                ~if_not_found:(fun key a b -> `Not_found (key, a, b)))
             ~expect:
               (match Map.find map key with
                | None -> `Not_found (key, a, b)
                | Some data -> `Found (data, a, b)))
     ;;

     let findi_and_call2 = findi_and_call2

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module struct
           type t = Constructors.t * Key.t * int * string
           [@@deriving quickcheck, sexp_of]
         end)
         ~f:(fun (constructors, key, a, b) ->
           let t, map = reify constructors in
           [%test_result:
             [ `Found of Key.t * Data.t * int * string
             | `Not_found of Key.t * int * string
             ]]
             (findi_and_call2
                t
                key
                ~compare
                ~a
                ~b
                ~if_found:(fun ~key ~data a b -> `Found (key, data, a, b))
                ~if_not_found:(fun key a b -> `Not_found (key, a, b)))
             ~expect:
               (match Map.find map key with
                | None -> `Not_found (key, a, b)
                | Some data -> `Found (key, data, a, b)))
     ;;

     let iter = iter

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module Constructors)
         ~f:(fun constructors ->
           let t, map = reify constructors in
           [%test_result: (Key.t * Data.t) list]
             (let q = Queue.create () in
              iter t ~f:(fun ~key ~data -> Queue.enqueue q (key, data));
              Queue.to_list q)
             ~expect:(Map.to_alist map))
     ;;

     let mapi_inplace = mapi_inplace

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module Constructors)
         ~f:(fun constructors ->
           let t, map = reify constructors in
           [%test_result: (Key.t * Data.t) list]
             (mapi_inplace t ~f:(fun ~key:_ ~data -> data ^ data);
              fold t ~init:[] ~f:(fun ~key ~data acc -> (key, data) :: acc))
             ~expect:
               (Map.map map ~f:(fun data -> data ^ data)
                |> Map.to_alist
                |> List.rev))
     ;;

     let fold = fold

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module Constructors)
         ~f:(fun constructors ->
           let t, map = reify constructors in
           [%test_result: (Key.t * Data.t) list]
             (fold t ~init:[] ~f:(fun ~key ~data acc -> (key, data) :: acc))
             ~expect:(Map.to_alist map |> List.rev))
     ;;

     let choose_exn = choose_exn

     let%test_unit _ =
       Base_quickcheck.Test.run_exn
         (module Constructors)
         ~f:(fun constructors ->
           let t, map = reify constructors in
           [%test_result: bool]
             (is_some (Option.try_with (fun () -> choose_exn t)))
             ~expect:(not (Map.is_empty map)))
     ;;
   end :
     module type of Avltree))
;;
