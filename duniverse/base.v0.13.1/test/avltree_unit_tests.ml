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
         include Quickcheck.Int

         let quickcheck_generator = Quickcheck.Generator.small_non_negative_int
       end

       module Data = struct
         include Quickcheck.String

         let quickcheck_generator = gen' Quickcheck.Char.gen_lowercase
       end

       let compare = Key.compare

       open Quickcheck
       open Generator

       module Constructor = struct
         type t =
           | Add of Key.t * Data.t
           | Replace of Key.t * Data.t
           | Remove of Key.t
         [@@deriving sexp_of]

         let add_gen =
           Key.quickcheck_generator
           >>= fun key ->
           Data.quickcheck_generator >>| fun data -> Add (key, data)
         ;;

         let replace_gen =
           Key.quickcheck_generator
           >>= fun key ->
           Data.quickcheck_generator >>| fun data -> Replace (key, data)
         ;;

         let remove_gen = Key.quickcheck_generator >>| fun key -> Remove key
         let quickcheck_generator = union [ add_gen; replace_gen; remove_gen ]

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

       let constructors_gen =
         List.quickcheck_generator Constructor.quickcheck_generator
       ;;

       let reify constructors =
         List.fold
           constructors
           ~init:(empty, Key.Map.empty)
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
         | Empty -> Key.Map.empty
         | Leaf { key; value = data } -> Key.Map.singleton key data
         | Node { left; key; value = data; height = _; right } ->
           merge
             (Key.Map.singleton key data)
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
       Quickcheck.test
         constructors_gen
         ~sexp_of:[%sexp_of: Constructor.t list]
         ~f:(fun constructors ->
           let t, map = reify constructors in
           [%test_result: bool] (is_empty t) ~expect:(Map.is_empty map))
     ;;

     let invariant = invariant

     let%test_unit _ =
       Quickcheck.test
         constructors_gen
         ~sexp_of:[%sexp_of: Constructor.t list]
         ~f:(fun constructors ->
           let t, map = reify constructors in
           invariant t ~compare;
           [%test_result: Data.t Key.Map.t] (to_map t) ~expect:map)
     ;;

     let add = add

     let%test_unit _ =
       Quickcheck.test
         (Quickcheck.Generator.tuple4
            constructors_gen
            Key.quickcheck_generator
            Data.quickcheck_generator
            Quickcheck.Bool.quickcheck_generator)
         ~sexp_of:[%sexp_of: Constructor.t list * Key.t * Data.t * bool]
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
       Quickcheck.test
         (Quickcheck.Generator.tuple2 constructors_gen Key.quickcheck_generator)
         ~sexp_of:[%sexp_of: Constructor.t list * Key.t]
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
       Quickcheck.test
         (Quickcheck.Generator.tuple2 constructors_gen Key.quickcheck_generator)
         ~sexp_of:[%sexp_of: Constructor.t list * Key.t]
         ~f:(fun (constructors, key) ->
           let t, map = reify constructors in
           [%test_result: Data.t option]
             (find t key ~compare)
             ~expect:(Map.find map key))
     ;;

     let mem = mem

     let%test_unit _ =
       Quickcheck.test
         (Quickcheck.Generator.tuple2 constructors_gen Key.quickcheck_generator)
         ~sexp_of:[%sexp_of: Constructor.t list * Key.t]
         ~f:(fun (constructors, key) ->
           let t, map = reify constructors in
           [%test_result: bool] (mem t key ~compare) ~expect:(Map.mem map key))
     ;;

     let first = first

     let%test_unit _ =
       Quickcheck.test
         constructors_gen
         ~sexp_of:[%sexp_of: Constructor.t list]
         ~f:(fun constructors ->
           let t, map = reify constructors in
           [%test_result: (Key.t * Data.t) option]
             (first t)
             ~expect:(Map.min_elt map))
     ;;

     let last = last

     let%test_unit _ =
       Quickcheck.test
         constructors_gen
         ~sexp_of:[%sexp_of: Constructor.t list]
         ~f:(fun constructors ->
           let t, map = reify constructors in
           [%test_result: (Key.t * Data.t) option]
             (last t)
             ~expect:(Map.max_elt map))
     ;;

     let find_and_call = find_and_call

     let%test_unit _ =
       Quickcheck.test
         (Quickcheck.Generator.tuple2 constructors_gen Key.quickcheck_generator)
         ~sexp_of:[%sexp_of: Constructor.t list * Key.t]
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
       Quickcheck.test
         (Quickcheck.Generator.tuple2 constructors_gen Key.quickcheck_generator)
         ~sexp_of:[%sexp_of: Constructor.t list * Key.t]
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
       Quickcheck.test
         (Quickcheck.Generator.tuple3
            constructors_gen
            Key.quickcheck_generator
            Base_quickcheck.Generator.int)
         ~sexp_of:[%sexp_of: Constructor.t list * Key.t * int]
         ~f:(fun (constructors, key, arg) ->
           let t, map = reify constructors in
           [%test_result:
             [ `Found of Data.t * int | `Not_found of Key.t * int ]]
             (find_and_call1
                t
                key
                ~compare
                arg
                ~if_found:(fun data arg -> `Found (data, arg))
                ~if_not_found:(fun key arg -> `Not_found (key, arg)))
             ~expect:
               (match Map.find map key with
                | None -> `Not_found (key, arg)
                | Some data -> `Found (data, arg)))
     ;;

     let iter = iter

     let%test_unit _ =
       Quickcheck.test
         constructors_gen
         ~sexp_of:[%sexp_of: Constructor.t list]
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
       Quickcheck.test
         constructors_gen
         ~sexp_of:[%sexp_of: Constructor.t list]
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
       Quickcheck.test
         constructors_gen
         ~sexp_of:[%sexp_of: Constructor.t list]
         ~f:(fun constructors ->
           let t, map = reify constructors in
           [%test_result: (Key.t * Data.t) list]
             (fold t ~init:[] ~f:(fun ~key ~data acc -> (key, data) :: acc))
             ~expect:(Map.to_alist map |> List.rev))
     ;;

     let choose_exn = choose_exn

     let%test_unit _ =
       Quickcheck.test
         constructors_gen
         ~sexp_of:[%sexp_of: Constructor.t list]
         ~f:(fun constructors ->
           let t, map = reify constructors in
           [%test_result: bool]
             (is_some (Option.try_with (fun () -> choose_exn t)))
             ~expect:(not (Map.is_empty map)))
     ;;
   end :
     module type of Avltree))
;;
