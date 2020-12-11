open! Core_kernel
open Base_test_helpers

let%test_module _ =
  (module (
   struct
     open Queue

     module type S = S

     let does_raise = Exn.does_raise

     type nonrec 'a t = 'a t [@@deriving bin_io, sexp]

     let capacity = capacity
     let set_capacity = set_capacity

     let%test_unit _ =
       let t = create () in
       [%test_result: int] (capacity t) ~expect:1;
       enqueue t 1;
       [%test_result: int] (capacity t) ~expect:1;
       enqueue t 2;
       [%test_result: int] (capacity t) ~expect:2;
       enqueue t 3;
       [%test_result: int] (capacity t) ~expect:4;
       set_capacity t 0;
       [%test_result: int] (capacity t) ~expect:4;
       set_capacity t 3;
       [%test_result: int] (capacity t) ~expect:4;
       set_capacity t 100;
       [%test_result: int] (capacity t) ~expect:128;
       enqueue t 4;
       enqueue t 5;
       set_capacity t 0;
       [%test_result: int] (capacity t) ~expect:8;
       set_capacity t (-1);
       [%test_result: int] (capacity t) ~expect:8
     ;;


     let round_trip_sexp t =
       let sexp = sexp_of_t Int.sexp_of_t t in
       let t' = t_of_sexp Int.t_of_sexp sexp in
       [%test_result: int list] ~expect:(to_list t) (to_list t')
     ;;

     let%test_unit _ = round_trip_sexp (of_list [ 1; 2; 3; 4 ])
     let%test_unit _ = round_trip_sexp (create ())
     let%test_unit _ = round_trip_sexp (of_list [])

     let invariant = invariant
     let create = create

     let%test_unit _ =
       let t = create () in
       [%test_result: int] (length t) ~expect:0;
       [%test_result: int] (capacity t) ~expect:1
     ;;

     let%test_unit _ =
       let t = create ~capacity:0 () in
       [%test_result: int] (length t) ~expect:0;
       [%test_result: int] (capacity t) ~expect:1
     ;;

     let%test_unit _ =
       let t = create ~capacity:6 () in
       [%test_result: int] (length t) ~expect:0;
       [%test_result: int] (capacity t) ~expect:8
     ;;

     let%test_unit _ =
       assert (does_raise (fun () -> (create ~capacity:(-1) () : _ Queue.t)))
     ;;

     let singleton = singleton

     let%test_unit _ =
       let t = singleton 7 in
       [%test_result: int] (length t) ~expect:1;
       [%test_result: int] (capacity t) ~expect:1;
       [%test_result: int option] (dequeue t) ~expect:(Some 7);
       [%test_result: int option] (dequeue t) ~expect:None
     ;;

     let init = init

     let%test_unit _ =
       let t = init 0 ~f:(fun _ -> assert false) in
       [%test_result: int] (length t) ~expect:0;
       [%test_result: int] (capacity t) ~expect:1;
       [%test_result: int option] (dequeue t) ~expect:None
     ;;

     let%test_unit _ =
       let t = init 3 ~f:(fun i -> i * 2) in
       [%test_result: int] (length t) ~expect:3;
       [%test_result: int] (capacity t) ~expect:4;
       [%test_result: int option] (dequeue t) ~expect:(Some 0);
       [%test_result: int option] (dequeue t) ~expect:(Some 2);
       [%test_result: int option] (dequeue t) ~expect:(Some 4);
       [%test_result: int option] (dequeue t) ~expect:None
     ;;

     let%test_unit _ =
       assert (
         does_raise (fun () -> (init (-1) ~f:(fun _ -> ()) : unit Queue.t)))
     ;;

     let get = get
     let set = set

     let%test_unit _ =
       let t = create () in
       let get_opt t i = Option.try_with (fun () -> get t i) in
       [%test_result: int option] (get_opt t 0) ~expect:None;
       [%test_result: int option] (get_opt t (-1)) ~expect:None;
       [%test_result: int option] (get_opt t 10) ~expect:None;
       List.iter [ -1; 0; 1 ] ~f:(fun i ->
         assert (does_raise (fun () -> set t i 0)));
       enqueue t 0;
       enqueue t 1;
       enqueue t 2;
       [%test_result: int option] (get_opt t 0) ~expect:(Some 0);
       [%test_result: int option] (get_opt t 1) ~expect:(Some 1);
       [%test_result: int option] (get_opt t 2) ~expect:(Some 2);
       [%test_result: int option] (get_opt t 3) ~expect:None;
       ignore (dequeue_exn t : int);
       [%test_result: int option] (get_opt t 0) ~expect:(Some 1);
       [%test_result: int option] (get_opt t 1) ~expect:(Some 2);
       [%test_result: int option] (get_opt t 2) ~expect:None;
       set t 0 3;
       [%test_result: int option] (get_opt t 0) ~expect:(Some 3);
       [%test_result: int option] (get_opt t 1) ~expect:(Some 2);
       List.iter [ -1; 2 ] ~f:(fun i ->
         assert (does_raise (fun () -> set t i 0)))
     ;;

     let map = map

     let%test_unit _ =
       for i = 0 to 5 do
         let l = List.init i ~f:Fn.id in
         let t = of_list l in
         let f x = x * 2 in
         let t' = map t ~f in
         [%test_result: int list] (to_list t') ~expect:(List.map l ~f)
       done
     ;;

     let%test_unit _ =
       let t = create () in
       let t' = map t ~f:(fun x -> x * 2) in
       [%test_result: int] (length t') ~expect:(length t);
       [%test_result: int] (length t') ~expect:0;
       [%test_result: int list] (to_list t') ~expect:[]
     ;;

     let mapi = mapi

     let%test_unit _ =
       for i = 0 to 5 do
         let l = List.init i ~f:Fn.id in
         let t = of_list l in
         let f i x = i, x * 2 in
         let t' = mapi t ~f in
         [%test_result: (int * int) list] (to_list t') ~expect:(List.mapi l ~f)
       done
     ;;

     let%test_unit _ =
       let t = create () in
       let t' = mapi t ~f:(fun i x -> i, x * 2) in
       [%test_result: int] (length t') ~expect:(length t);
       [%test_result: int] (length t') ~expect:0;
       [%test_result: (int * int) list] (to_list t') ~expect:[]
     ;;

     include Test_container.Test_S1 (Queue)

     let dequeue_exn = dequeue_exn
     let enqueue = enqueue
     let peek = peek
     let peek_exn = peek_exn
     let last = last
     let last_exn = last_exn

     let%test_unit _ =
       let t = create () in
       [%test_result: int option] (peek t) ~expect:None;
       [%test_result: int option] (last t) ~expect:None;
       enqueue t 1;
       enqueue t 2;
       [%test_result: int option] (peek t) ~expect:(Some 1);
       [%test_result: int] (peek_exn t) ~expect:1;
       [%test_result: int option] (last t) ~expect:(Some 2);
       [%test_result: int] (last_exn t) ~expect:2;
       [%test_result: int] (dequeue_exn t) ~expect:1;
       [%test_result: int] (dequeue_exn t) ~expect:2;
       assert (does_raise (fun () -> dequeue_exn t));
       assert (does_raise (fun () -> peek_exn t));
       assert (does_raise (fun () -> last_exn t))
     ;;

     let enqueue_all = enqueue_all

     let%test_unit _ =
       let t = create () in
       enqueue_all t [ 1; 2; 3 ];
       [%test_result: int] (dequeue_exn t) ~expect:1;
       [%test_result: int] (dequeue_exn t) ~expect:2;
       [%test_result: int option] (last t) ~expect:(Some 3);
       enqueue_all t [ 4; 5 ];
       [%test_result: int option] (last t) ~expect:(Some 5);
       [%test_result: int] (dequeue_exn t) ~expect:3;
       [%test_result: int] (dequeue_exn t) ~expect:4;
       [%test_result: int] (dequeue_exn t) ~expect:5;
       assert (does_raise (fun () -> dequeue_exn t));
       enqueue_all t [];
       assert (does_raise (fun () -> dequeue_exn t))
     ;;

     let of_list = of_list
     let to_list = to_list

     let%test_unit _ =
       for i = 0 to 4 do
         let list = List.init i ~f:Fn.id in
         [%test_result: int list] (to_list (of_list list)) ~expect:list
       done
     ;;

     let%test _ =
       let t = create () in
       for i = 1 to 5 do
         enqueue t i
       done;
       [%equal: int list] (to_list t) [ 1; 2; 3; 4; 5 ]
     ;;

     let of_array = of_array
     let to_array = to_array

     let%test_unit _ =
       for len = 0 to 4 do
         let array = Array.init len ~f:Fn.id in
         [%test_result: int array] (to_array (of_array array)) ~expect:array
       done
     ;;

     let compare = compare
     let equal = equal

     let%test_module "comparisons" =
       (module struct
         let sign x = if x < 0 then ~-1 else if x > 0 then 1 else 0

         let test t1 t2 =
           [%test_result: bool]
             (equal Int.equal t1 t2)
             ~expect:(List.equal Int.equal (to_list t1) (to_list t2));
           [%test_result: int]
             (sign (compare Int.compare t1 t2))
             ~expect:
               (sign (List.compare Int.compare (to_list t1) (to_list t2)))
         ;;

         let lists =
           [ []
           ; [ 1 ]
           ; [ 2 ]
           ; [ 1; 1 ]
           ; [ 1; 2 ]
           ; [ 2; 1 ]
           ; [ 1; 1; 1 ]
           ; [ 1; 2; 3 ]
           ; [ 1; 2; 4 ]
           ; [ 1; 2; 4; 8 ]
           ; [ 1; 2; 3; 4; 5 ]
           ]
         ;;

         let%test_unit _ =
           (* [phys_equal] inputs *)
           List.iter lists ~f:(fun list ->
             let t = of_list list in
             test t t)
         ;;

         let%test_unit _ =
           List.iter lists ~f:(fun list1 ->
             List.iter lists ~f:(fun list2 ->
               test (of_list list1) (of_list list2)))
         ;;
       end)
     ;;

     let clear = clear
     let blit_transfer = blit_transfer

     let%test_unit _ =
       let q_list = [ 1; 2; 3; 4 ] in
       let q = of_list q_list in
       let q' = create () in
       blit_transfer ~src:q ~dst:q' ();
       [%test_result: int list] (to_list q') ~expect:q_list;
       [%test_result: int list] (to_list q) ~expect:[]
     ;;

     let%test_unit _ =
       let q = of_list [ 1; 2; 3; 4 ] in
       let q' = create () in
       blit_transfer ~src:q ~dst:q' ~len:2 ();
       [%test_result: int list] (to_list q') ~expect:[ 1; 2 ];
       [%test_result: int list] (to_list q) ~expect:[ 3; 4 ]
     ;;

     let%test_unit "blit_transfer on wrapped queues" =
       let list = [ 1; 2; 3; 4 ] in
       let q = of_list list in
       let q' = copy q in
       ignore (dequeue_exn q : int);
       ignore (dequeue_exn q : int);
       ignore (dequeue_exn q' : int);
       ignore (dequeue_exn q' : int);
       ignore (dequeue_exn q' : int);
       enqueue q 5;
       enqueue q 6;
       blit_transfer ~src:q ~dst:q' ~len:3 ();
       [%test_result: int list] (to_list q') ~expect:[ 4; 3; 4; 5 ];
       [%test_result: int list] (to_list q) ~expect:[ 6 ]
     ;;

     let copy = copy
     let dequeue = dequeue
     let filter = filter
     let filteri = filteri
     let filter_inplace = filter_inplace
     let filteri_inplace = filteri_inplace
     let concat_map = concat_map
     let concat_mapi = concat_mapi
     let filter_map = filter_map
     let filter_mapi = filter_mapi
     let counti = counti
     let existsi = existsi
     let for_alli = for_alli
     let iter = iter
     let iteri = iteri
     let foldi = foldi
     let findi = findi
     let find_mapi = find_mapi

     let%test_module "Linked_queue bisimulation" =
       (module struct
         module type Queue_intf = sig
           type 'a t [@@deriving sexp_of]

           val create : unit -> 'a t
           val enqueue : 'a t -> 'a -> unit
           val dequeue : 'a t -> 'a option
           val to_array : 'a t -> 'a array
           val fold : 'a t -> init:'b -> f:('b -> 'a -> 'b) -> 'b
           val foldi : 'a t -> init:'b -> f:(int -> 'b -> 'a -> 'b) -> 'b
           val iter : 'a t -> f:('a -> unit) -> unit
           val iteri : 'a t -> f:(int -> 'a -> unit) -> unit
           val length : 'a t -> int
           val clear : 'a t -> unit
           val concat_map : 'a t -> f:('a -> 'b list) -> 'b t
           val concat_mapi : 'a t -> f:(int -> 'a -> 'b list) -> 'b t
           val filter_map : 'a t -> f:('a -> 'b option) -> 'b t
           val filter_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b t
           val filter : 'a t -> f:('a -> bool) -> 'a t
           val filteri : 'a t -> f:(int -> 'a -> bool) -> 'a t
           val filter_inplace : 'a t -> f:('a -> bool) -> unit
           val filteri_inplace : 'a t -> f:(int -> 'a -> bool) -> unit
           val map : 'a t -> f:('a -> 'b) -> 'b t
           val mapi : 'a t -> f:(int -> 'a -> 'b) -> 'b t
           val counti : 'a t -> f:(int -> 'a -> bool) -> int
           val existsi : 'a t -> f:(int -> 'a -> bool) -> bool
           val for_alli : 'a t -> f:(int -> 'a -> bool) -> bool
           val findi : 'a t -> f:(int -> 'a -> bool) -> (int * 'a) option
           val find_mapi : 'a t -> f:(int -> 'a -> 'b option) -> 'b option
           val transfer : src:'a t -> dst:'a t -> unit
           val copy : 'a t -> 'a t
         end

         module That_queue : Queue_intf = Linked_queue

         module This_queue : Queue_intf = struct
           include Queue

           let create () = create ()
           let transfer ~src ~dst = blit_transfer ~src ~dst ()
         end

         let this_to_string this_t =
           Sexp.to_string (this_t |> [%sexp_of: int This_queue.t])
         ;;

         let that_to_string that_t =
           Sexp.to_string (that_t |> [%sexp_of: int That_queue.t])
         ;;

         let array_string arr = Sexp.to_string (arr |> [%sexp_of: int array])
         let create () = This_queue.create (), That_queue.create ()


         let enqueue (t_a, t_b) v =
           let start_a = This_queue.to_array t_a in
           let start_b = That_queue.to_array t_b in
           This_queue.enqueue t_a v;
           That_queue.enqueue t_b v;
           let end_a = This_queue.to_array t_a in
           let end_b = That_queue.to_array t_b in
           if not ([%equal: int array] end_a end_b)
           then
             failwithf
               "enqueue transition failure of: %s -> %s vs. %s -> %s"
               (array_string start_a)
               (array_string end_a)
               (array_string start_b)
               (array_string end_b)
               ()
         ;;

         let iter (t_a, t_b) =
           let r_a, r_b = ref 0, ref 0 in
           This_queue.iter t_a ~f:(fun x -> r_a := !r_a + x);
           That_queue.iter t_b ~f:(fun x -> r_b := !r_b + x);
           if !r_a <> !r_b
           then
             failwithf
               "error in iter: %s (from %s) <> %s (from %s)"
               (Int.to_string !r_a)
               (this_to_string t_a)
               (Int.to_string !r_b)
               (that_to_string t_b)
               ()
         ;;

         let iteri (t_a, t_b) =
           let r_a, r_b = ref 0, ref 0 in
           This_queue.iteri t_a ~f:(fun i x -> r_a := !r_a + (x lxor i));
           That_queue.iteri t_b ~f:(fun i x -> r_b := !r_b + (x lxor i));
           if !r_a <> !r_b
           then
             failwithf
               "error in iteri: %s (from %s) <> %s (from %s)"
               (Int.to_string !r_a)
               (this_to_string t_a)
               (Int.to_string !r_b)
               (that_to_string t_b)
               ()
         ;;

         let dequeue (t_a, t_b) =
           let start_a = This_queue.to_array t_a in
           let start_b = That_queue.to_array t_b in
           let a, b = This_queue.dequeue t_a, That_queue.dequeue t_b in
           let end_a = This_queue.to_array t_a in
           let end_b = That_queue.to_array t_b in
           if (not ([%equal: int option] a b))
           || not ([%equal: int array] end_a end_b)
           then
             failwithf
               "error in dequeue: %s (%s -> %s) <> %s (%s -> %s)"
               (Option.value ~default:"None" (Option.map a ~f:Int.to_string))
               (array_string start_a)
               (array_string end_a)
               (Option.value ~default:"None" (Option.map b ~f:Int.to_string))
               (array_string start_b)
               (array_string end_b)
               ()
         ;;

         let clear (t_a, t_b) =
           This_queue.clear t_a;
           That_queue.clear t_b
         ;;

         let is_even x = x land 1 = 0

         let filter (t_a, t_b) =
           let t_a' = This_queue.filter t_a ~f:is_even in
           let t_b' = That_queue.filter t_b ~f:is_even in
           if not
                ([%equal: int array]
                   (This_queue.to_array t_a')
                   (That_queue.to_array t_b'))
           then
             failwithf
               "error in filter: %s -> %s vs. %s -> %s"
               (this_to_string t_a)
               (this_to_string t_a')
               (that_to_string t_b)
               (that_to_string t_b')
               ()
         ;;

         let filteri (t_a, t_b) =
           let t_a' =
             This_queue.filteri t_a ~f:(fun i j ->
               [%equal: bool] (is_even i) (is_even j))
           in
           let t_b' =
             That_queue.filteri t_b ~f:(fun i j ->
               [%equal: bool] (is_even i) (is_even j))
           in
           if not
                ([%equal: int array]
                   (This_queue.to_array t_a')
                   (That_queue.to_array t_b'))
           then
             failwithf
               "error in filteri: %s -> %s vs. %s -> %s"
               (this_to_string t_a)
               (this_to_string t_a')
               (that_to_string t_b)
               (that_to_string t_b')
               ()
         ;;

         let filter_inplace (t_a, t_b) =
           let start_a = This_queue.to_array t_a in
           let start_b = That_queue.to_array t_b in
           This_queue.filter_inplace t_a ~f:is_even;
           That_queue.filter_inplace t_b ~f:is_even;
           let end_a = This_queue.to_array t_a in
           let end_b = That_queue.to_array t_b in
           if not ([%equal: int array] end_a end_b)
           then
             failwithf
               "error in filter_inplace: %s -> %s vs. %s -> %s"
               (array_string start_a)
               (array_string end_a)
               (array_string start_b)
               (array_string end_b)
               ()
         ;;

         let filteri_inplace (t_a, t_b) =
           let start_a = This_queue.to_array t_a in
           let start_b = That_queue.to_array t_b in
           let f i x = [%equal: bool] (is_even i) (is_even x) in
           This_queue.filteri_inplace t_a ~f;
           That_queue.filteri_inplace t_b ~f;
           let end_a = This_queue.to_array t_a in
           let end_b = That_queue.to_array t_b in
           if not ([%equal: int array] end_a end_b)
           then
             failwithf
               "error in filteri_inplace: %s -> %s vs. %s -> %s"
               (array_string start_a)
               (array_string end_a)
               (array_string start_b)
               (array_string end_b)
               ()
         ;;

         let concat_map (t_a, t_b) =
           let f x = [ x; x + 1; x + 2 ] in
           let t_a' = This_queue.concat_map t_a ~f in
           let t_b' = That_queue.concat_map t_b ~f in
           if not
                ([%equal: int array]
                   (This_queue.to_array t_a')
                   (That_queue.to_array t_b'))
           then
             failwithf
               "error in concat_map: %s (for %s) <> %s (for %s)"
               (this_to_string t_a')
               (this_to_string t_a)
               (that_to_string t_b')
               (that_to_string t_b)
               ()
         ;;

         let concat_mapi (t_a, t_b) =
           let f i x = [ x; x + 1; x + 2; x + i ] in
           let t_a' = This_queue.concat_mapi t_a ~f in
           let t_b' = That_queue.concat_mapi t_b ~f in
           if not
                ([%equal: int array]
                   (This_queue.to_array t_a')
                   (That_queue.to_array t_b'))
           then
             failwithf
               "error in concat_mapi: %s (for %s) <> %s (for %s)"
               (this_to_string t_a')
               (this_to_string t_a)
               (that_to_string t_b')
               (that_to_string t_b)
               ()
         ;;

         let filter_map (t_a, t_b) =
           let f x = if is_even x then None else Some (x + 1) in
           let t_a' = This_queue.filter_map t_a ~f in
           let t_b' = That_queue.filter_map t_b ~f in
           if not
                ([%equal: int array]
                   (This_queue.to_array t_a')
                   (That_queue.to_array t_b'))
           then
             failwithf
               "error in filter_map: %s (for %s) <> %s (for %s)"
               (this_to_string t_a')
               (this_to_string t_a)
               (that_to_string t_b')
               (that_to_string t_b)
               ()
         ;;

         let filter_mapi (t_a, t_b) =
           let f i x =
             if [%equal: bool] (is_even i) (is_even x)
             then None
             else Some (x + 1 + i)
           in
           let t_a' = This_queue.filter_mapi t_a ~f in
           let t_b' = That_queue.filter_mapi t_b ~f in
           if not
                ([%equal: int array]
                   (This_queue.to_array t_a')
                   (That_queue.to_array t_b'))
           then
             failwithf
               "error in filter_mapi: %s (for %s) <> %s (for %s)"
               (this_to_string t_a')
               (this_to_string t_a)
               (that_to_string t_b')
               (that_to_string t_b)
               ()
         ;;

         let map (t_a, t_b) =
           let f x = x * 7 in
           let t_a' = This_queue.map t_a ~f in
           let t_b' = That_queue.map t_b ~f in
           if not
                ([%equal: int array]
                   (This_queue.to_array t_a')
                   (That_queue.to_array t_b'))
           then
             failwithf
               "error in map: %s (for %s) <> %s (for %s)"
               (this_to_string t_a')
               (this_to_string t_a)
               (that_to_string t_b')
               (that_to_string t_b)
               ()
         ;;

         let mapi (t_a, t_b) =
           let f i x = (x + 3) lxor i in
           let t_a' = This_queue.mapi t_a ~f in
           let t_b' = That_queue.mapi t_b ~f in
           if not
                ([%equal: int array]
                   (This_queue.to_array t_a')
                   (That_queue.to_array t_b'))
           then
             failwithf
               "error in mapi: %s (for %s) <> %s (for %s)"
               (this_to_string t_a')
               (this_to_string t_a)
               (that_to_string t_b')
               (that_to_string t_b)
               ()
         ;;

         let counti (t_a, t_b) =
           let f i x = i < 7 && i % 7 = x % 7 in
           let a' = This_queue.counti t_a ~f in
           let b' = That_queue.counti t_b ~f in
           if a' <> b'
           then
             failwithf
               "error in counti: %d (for %s) <> %d (for %s)"
               a'
               (this_to_string t_a)
               b'
               (that_to_string t_b)
               ()
         ;;

         let existsi (t_a, t_b) =
           let f i x = i < 7 && i % 7 = x % 7 in
           let a' = This_queue.existsi t_a ~f in
           let b' = That_queue.existsi t_b ~f in
           if not ([%equal: bool] a' b')
           then
             failwithf
               "error in existsi: %b (for %s) <> %b (for %s)"
               a'
               (this_to_string t_a)
               b'
               (that_to_string t_b)
               ()
         ;;

         let for_alli (t_a, t_b) =
           let f i x = i >= 7 || i % 7 <> x % 7 in
           let a' = This_queue.for_alli t_a ~f in
           let b' = That_queue.for_alli t_b ~f in
           if not ([%equal: bool] a' b')
           then
             failwithf
               "error in for_alli: %b (for %s) <> %b (for %s)"
               a'
               (this_to_string t_a)
               b'
               (that_to_string t_b)
               ()
         ;;

         let findi (t_a, t_b) =
           let f i x = i < 7 && i % 7 = x % 7 in
           let a' = This_queue.findi t_a ~f in
           let b' = That_queue.findi t_b ~f in
           if not ([%equal: (int * int) option] a' b')
           then
             failwithf
               "error in findi: %s (for %s) <> %s (for %s)"
               (Sexp.to_string ([%sexp_of: (int * int) option] a'))
               (this_to_string t_a)
               (Sexp.to_string ([%sexp_of: (int * int) option] b'))
               (that_to_string t_b)
               ()
         ;;

         let find_mapi (t_a, t_b) =
           let f i x = if i < 7 && i % 7 = x % 7 then Some (i + x) else None in
           let a' = This_queue.find_mapi t_a ~f in
           let b' = That_queue.find_mapi t_b ~f in
           if not ([%equal: int option] a' b')
           then
             failwithf
               "error in find_mapi: %s (for %s) <> %s (for %s)"
               (Sexp.to_string ([%sexp_of: int option] a'))
               (this_to_string t_a)
               (Sexp.to_string ([%sexp_of: int option] b'))
               (that_to_string t_b)
               ()
         ;;

         let copy (t_a, t_b) =
           let copy_a = This_queue.copy t_a in
           let copy_b = That_queue.copy t_b in
           let start_a = This_queue.to_array t_a in
           let start_b = That_queue.to_array t_b in
           let end_a = This_queue.to_array copy_a in
           let end_b = That_queue.to_array copy_b in
           if not ([%equal: int array] end_a end_b)
           then
             failwithf
               "error in copy: %s -> %s vs. %s -> %s"
               (array_string start_a)
               (array_string end_a)
               (array_string start_b)
               (array_string end_b)
               ()
         ;;

         let transfer (t_a, t_b) =
           let dst_a = This_queue.create () in
           let dst_b = That_queue.create () in
           (* sometimes puts some elements in the destination queues *)
           if Random.bool ()
           then
             List.iter [ 1; 2; 3; 4; 5 ] ~f:(fun elem ->
               This_queue.enqueue dst_a elem;
               That_queue.enqueue dst_b elem);
           let start_a = This_queue.to_array t_a in
           let start_b = That_queue.to_array t_b in
           This_queue.transfer ~src:t_a ~dst:dst_a;
           That_queue.transfer ~src:t_b ~dst:dst_b;
           let end_a = This_queue.to_array t_a in
           let end_b = That_queue.to_array t_b in
           let end_a' = This_queue.to_array dst_a in
           let end_b' = That_queue.to_array dst_b in
           if (not ([%equal: int array] end_a' end_b'))
           || not ([%equal: int array] end_a end_b)
           then
             failwithf
               "error in transfer: %s -> (%s, %s) vs. %s -> (%s, %s)"
               (array_string start_a)
               (array_string end_a)
               (array_string end_a')
               (array_string start_b)
               (array_string end_b)
               (array_string end_b)
               ()
         ;;

         let fold_check (t_a, t_b) =
           let make_list fold t = fold t ~init:[] ~f:(fun acc x -> x :: acc) in
           let this_l = make_list This_queue.fold t_a in
           let that_l = make_list That_queue.fold t_b in
           if not ([%equal: int list] this_l that_l)
           then
             failwithf
               "error in fold:  %s (from %s) <> %s (from %s)"
               (Sexp.to_string (this_l |> [%sexp_of: int list]))
               (this_to_string t_a)
               (Sexp.to_string (that_l |> [%sexp_of: int list]))
               (that_to_string t_b)
               ()
         ;;

         let foldi_check (t_a, t_b) =
           let make_list foldi t =
             foldi t ~init:[] ~f:(fun i acc x -> (i, x) :: acc)
           in
           let this_l = make_list This_queue.foldi t_a in
           let that_l = make_list That_queue.foldi t_b in
           if not ([%equal: (int * int) list] this_l that_l)
           then
             failwithf
               "error in foldi:  %s (from %s) <> %s (from %s)"
               (Sexp.to_string (this_l |> [%sexp_of: (int * int) list]))
               (this_to_string t_a)
               (Sexp.to_string (that_l |> [%sexp_of: (int * int) list]))
               (that_to_string t_b)
               ()
         ;;

         let length_check (t_a, t_b) =
           let this_len = This_queue.length t_a in
           let that_len = That_queue.length t_b in
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

         let%test_unit _ =
           let t = create () in
           let rec loop ~all_ops ~non_empty_ops =
             if all_ops <= 0 && non_empty_ops <= 0
             then (
               let t_a, t_b = t in
               let arr_a = This_queue.to_array t_a in
               let arr_b = That_queue.to_array t_b in
               if not ([%equal: int array] arr_a arr_b)
               then
                 failwithf
                   "queue final states not equal: %s vs. %s"
                   (array_string arr_a)
                   (array_string arr_b)
                   ())
             else (
               let queue_was_empty = This_queue.length (fst t) = 0 in
               let r = Random.int 195 in
               if r < 60
               then enqueue t (Random.int 10_000)
               else if r < 65
               then dequeue t
               else if r < 70
               then clear t
               else if r < 80
               then iter t
               else if r < 85
               then iteri t
               else if r < 90
               then fold_check t
               else if r < 95
               then foldi_check t
               else if r < 100
               then filter t
               else if r < 105
               then filteri t
               else if r < 110
               then concat_map t
               else if r < 115
               then concat_mapi t
               else if r < 120
               then transfer t
               else if r < 130
               then filter_map t
               else if r < 135
               then filter_mapi t
               else if r < 140
               then copy t
               else if r < 150
               then filter_inplace t
               else if r < 155
               then for_alli t
               else if r < 160
               then existsi t
               else if r < 165
               then counti t
               else if r < 170
               then findi t
               else if r < 175
               then find_mapi t
               else if r < 180
               then map t
               else if r < 185
               then mapi t
               else if r < 190
               then filteri_inplace t
               else if r < 195
               then length_check t
               else failwith "Impossible: We did [Random.int 195] above";
               loop
                 ~all_ops:(all_ops - 1)
                 ~non_empty_ops:
                   (if queue_was_empty
                    then non_empty_ops
                    else non_empty_ops - 1))
           in
           loop ~all_ops:30_000 ~non_empty_ops:20_000
         ;;
       end)
     ;;

     let binary_search = binary_search
     let binary_search_segmented = binary_search_segmented

     let%test_unit "modification-during-iteration" =
       let x = `A 0 in
       let t = of_list [ x; x ] in
       let f (`A n) =
         ignore n;
         clear t
       in
       assert (does_raise (fun () -> iter t ~f))
     ;;

     let%test_unit "more-modification-during-iteration" =
       let nested_iter_okay = ref false in
       let t = of_list [ `iter; `clear ] in
       assert (
         does_raise (fun () ->
           iter t ~f:(function
             | `iter ->
               iter t ~f:ignore;
               nested_iter_okay := true
             | `clear -> clear t)));
       assert !nested_iter_okay
     ;;

     let%test_unit "modification-during-filter" =
       let reached_unreachable = ref false in
       let t = of_list [ `clear; `unreachable ] in
       let f x =
         match x with
         | `clear ->
           clear t;
           false
         | `unreachable ->
           reached_unreachable := true;
           false
       in
       assert (does_raise (fun () -> filter t ~f));
       assert (not !reached_unreachable)
     ;;

     let%test_unit "modification-during-filter-inplace" =
       let reached_unreachable = ref false in
       let t = of_list [ `drop_this; `enqueue_new_element; `unreachable ] in
       let f x =
         (match x with
          | `drop_this | `new_element -> ()
          | `enqueue_new_element -> enqueue t `new_element
          | `unreachable -> reached_unreachable := true);
         false
       in
       assert (does_raise (fun () -> filter_inplace t ~f));
       (* even though we said to drop the first element, the aborted call to [filter_inplace]
          shouldn't have made that change *)
       (match peek_exn t with
        | `drop_this -> ()
        | `new_element | `enqueue_new_element | `unreachable ->
          failwith "Expected the first element to be `drop_this");
       assert (not !reached_unreachable)
     ;;

     let%test_unit "filter-inplace-during-iteration" =
       let reached_unreachable = ref false in
       let t = of_list [ `filter_inplace; `unreachable ] in
       let f x =
         match x with
         | `filter_inplace -> filter_inplace t ~f:(fun _ -> false)
         | `unreachable -> reached_unreachable := true
       in
       assert (does_raise (fun () -> iter t ~f));
       assert (not !reached_unreachable)
     ;;

     module Stable = struct
       module V1 = Stable.V1

       include Stable_unit_test.Make (struct
           type nonrec t = int V1.t [@@deriving sexp, bin_io, compare]

           let equal = [%compare.equal: t]

           let tests =
             let manipulated = Queue.of_list [ 0; 3; 6; 1 ] in
             ignore (Queue.dequeue_exn manipulated : int);
             ignore (Queue.dequeue_exn manipulated : int);
             Queue.enqueue manipulated 4;
             [ Queue.of_list [], "()", "\000"
             ; Queue.of_list [ 1; 2; 6; 4 ], "(1 2 6 4)", "\004\001\002\006\004"
             ; manipulated, "(6 1 4)", "\003\006\001\004"
             ]
           ;;
         end)
     end
   end
   (* This signature is here to remind us to update the unit tests whenever we
      change [Core_queue]. *) :
     module type of Queue))
;;
