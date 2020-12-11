open! Base

module type Hashtbl_for_testing = sig
  include Hashtbl.Accessors with type 'key key = 'key
  include Invariant.S2 with type ('key, 'data) t := ('key, 'data) t

  (* we don't define [module Poly : Hashtbl.S_poly] because we want to require only
     the minimal number of constructors necessary to implement the tests, and also avoid
     conflicting with any existing names. *)

  val create_poly : ?size:int -> unit -> ('key, 'data) t
  val of_alist_poly_exn : ('key * 'data) list -> ('key, 'data) t
  val of_alist_poly_or_error : ('key * 'data) list -> ('key, 'data) t Or_error.t
end

module Make (Hashtbl : Hashtbl_for_testing) = struct
  open Poly

  let test_data = [ "a", 1; "b", 2; "c", 3 ]

  let test_hash =
    let h = Hashtbl.create_poly () ~size:10 in
    List.iter test_data ~f:(fun (k, v) -> Hashtbl.set h ~key:k ~data:v);
    h
  ;;

  (* This is a very strong notion of equality on hash tables *)
  let equal t t' equal_data =
    let subtable t t' =
      try
        List.for_all (Hashtbl.keys t) ~f:(fun key ->
          equal_data (Hashtbl.find_exn t key) (Hashtbl.find_exn t' key))
      with
      | Invalid_argument _ -> false
    in
    subtable t t' && subtable t' t
  ;;

  let%test "find" =
    let found = Hashtbl.find test_hash "a" in
    let not_found = Hashtbl.find test_hash "A" in
    Hashtbl.invariant ignore ignore test_hash;
    match found, not_found with
    | Some _, None -> true
    | _ -> false
  ;;

  (* In js_of_ocaml, strings can be hashconst-ed. *)
  let%test ("findi_and_call"[@tags "no-js"]) =
    let our_hash = Hashtbl.copy test_hash in
    let test_string = "test string" in
    Hashtbl.add_exn our_hash ~key:test_string ~data:10;
    let test_string' = "test " ^ "string" in
    assert (not (phys_equal test_string test_string'));
    Hashtbl.findi_and_call
      our_hash
      test_string'
      ~if_found:(fun ~key ~data -> phys_equal test_string key && data = 10)
      ~if_not_found:(fun _ -> false)
  ;;

  let%test_unit "add" =
    let our_hash = Hashtbl.copy test_hash in
    let duplicate = Hashtbl.add our_hash ~key:"a" ~data:4 in
    let no_duplicate = Hashtbl.add our_hash ~key:"d" ~data:5 in
    assert (Hashtbl.find our_hash "a" = Some 1);
    assert (Hashtbl.find our_hash "d" = Some 5);
    Hashtbl.invariant ignore ignore our_hash;
    assert (
      match duplicate, no_duplicate with
      | `Duplicate, `Ok -> true
      | _ -> false)
  ;;

  let%test "iter" =
    let predicted =
      List.sort ~compare:Int.descending (List.map test_data ~f:(fun (_, v) -> v))
    in
    let found =
      let found = ref [] in
      Hashtbl.iter test_hash ~f:(fun v -> found := v :: !found);
      !found |> List.sort ~compare:Int.descending
    in
    List.equal Int.equal predicted found
  ;;

  let%test "iter_keys" =
    let predicted =
      List.sort ~compare:String.descending (List.map test_data ~f:(fun (k, _) -> k))
    in
    let found =
      let found = ref [] in
      Hashtbl.iter_keys test_hash ~f:(fun k -> found := k :: !found);
      !found |> List.sort ~compare:String.descending
    in
    List.equal String.equal predicted found
  ;;

  let%test_module "of_alist" =
    (module struct
      let%test "size" =
        let predicted = List.length test_data in
        let found = Hashtbl.length (Hashtbl.of_alist_poly_exn test_data) in
        predicted = found
      ;;

      let%test "right keys" =
        let predicted = List.map test_data ~f:(fun (k, _) -> k) in
        let found = Hashtbl.keys (Hashtbl.of_alist_poly_exn test_data) in
        let sp = List.sort ~compare:Poly.ascending predicted in
        let sf = List.sort ~compare:Poly.ascending found in
        sp = sf
      ;;
    end)
  ;;

  let%test_module "of_alist_or_error" =
    (module struct
      let%test "unique" = Result.is_ok (Hashtbl.of_alist_poly_or_error test_data)

      let%test "duplicate" =
        Result.is_error (Hashtbl.of_alist_poly_or_error (test_data @ test_data))
      ;;
    end)
  ;;

  let%test "size and right keys" =
    let predicted = List.map test_data ~f:(fun (k, _) -> k) in
    let found = Hashtbl.keys test_hash in
    let sp = List.sort ~compare:Poly.ascending predicted in
    let sf = List.sort ~compare:Poly.ascending found in
    sp = sf
  ;;

  let%test "size and right data" =
    let predicted = List.map test_data ~f:(fun (_, v) -> v) in
    let found = Hashtbl.data test_hash in
    let sp = List.sort ~compare:Poly.ascending predicted in
    let sf = List.sort ~compare:Poly.ascending found in
    sp = sf
  ;;

  let%test "map" =
    let add1 x = x + 1 in
    let predicted_data =
      List.sort ~compare:Poly.ascending (List.map test_data ~f:(fun (k, v) -> k, add1 v))
    in
    let found_alist =
      Hashtbl.map test_hash ~f:add1
      |> Hashtbl.to_alist
      |> List.sort ~compare:Poly.ascending
    in
    List.equal Poly.equal predicted_data found_alist
  ;;

  let%test_unit "filter_map" =
    let f x = Some x in
    let result = Hashtbl.filter_map test_hash ~f in
    assert (equal test_hash result Int.( = ));
    let is_even x = x % 2 = 0 in
    let add1_to_even x = if is_even x then Some (x + 1) else None in
    let predicted_data =
      List.filter_map test_data ~f:(fun (k, v) ->
        if is_even v then Some (k, v + 1) else None)
    in
    let found = Hashtbl.filter_map test_hash ~f:add1_to_even in
    let found_alist = List.sort ~compare:Poly.ascending (Hashtbl.to_alist found) in
    assert (List.equal Poly.equal predicted_data found_alist)
  ;;

  let%test "filter_inplace" =
    let f x = x <> 2 in
    let predicted_data =
      List.sort ~compare:Poly.ascending (List.filter test_data ~f:(fun (_, v) -> f v))
    in
    let test_hash = Hashtbl.copy test_hash in
    Hashtbl.filter_inplace test_hash ~f;
    let found_alist = Hashtbl.to_alist test_hash |> List.sort ~compare:Poly.ascending in
    List.equal Poly.equal predicted_data found_alist
  ;;

  let%test "filter_keys_inplace" =
    let f x = x = "c" in
    let predicted_data =
      List.sort ~compare:Poly.ascending (List.filter test_data ~f:(fun (k, _) -> f k))
    in
    let test_hash = Hashtbl.copy test_hash in
    Hashtbl.filter_keys_inplace test_hash ~f;
    let found_alist = Hashtbl.to_alist test_hash |> List.sort ~compare:Poly.ascending in
    List.equal Poly.equal predicted_data found_alist
  ;;

  let%test "filter_map_inplace" =
    let f x = if x = 1 then None else Some (x * 2) in
    let predicted_data =
      List.sort
        ~compare:Poly.ascending
        (List.filter_map test_data ~f:(fun (k, v) -> Option.map (f v) ~f:(fun x -> k, x)))
    in
    let test_hash = Hashtbl.copy test_hash in
    Hashtbl.filter_map_inplace test_hash ~f;
    let found_alist = Hashtbl.to_alist test_hash |> List.sort ~compare:Poly.ascending in
    List.equal Poly.equal predicted_data found_alist
  ;;

  let%test "map_inplace" =
    let f x = x + 3 in
    let predicted_data =
      List.sort ~compare:Poly.ascending (List.map test_data ~f:(fun (k, v) -> k, f v))
    in
    let test_hash = Hashtbl.copy test_hash in
    Hashtbl.map_inplace test_hash ~f;
    let found_alist = Hashtbl.to_alist test_hash |> List.sort ~compare:Poly.ascending in
    List.equal Poly.equal predicted_data found_alist
  ;;

  let%test_unit "insert-find-remove" =
    let t = Hashtbl.create_poly () ~size:1 in
    let inserted = ref [] in
    Random.init 123;
    let verify_inserted t =
      let missing =
        List.fold !inserted ~init:[] ~f:(fun acc (key, data) ->
          match Hashtbl.find t key with
          | None -> `Missing key :: acc
          | Some d -> if data = d then acc else `Wrong_data (key, data) :: acc)
      in
      match missing with
      | [] -> ()
      | _ ->
        raise_s
          [%message
            "some inserts are missing"
              (missing : [ `Missing of int | `Wrong_data of int * int ] list)]
    in
    let equal = Int.equal in
    let rec loop i t =
      if i < 2000
      then (
        let k = Random.int 10_000 in
        inserted := List.Assoc.add (List.Assoc.remove !inserted ~equal k) ~equal k i;
        Hashtbl.set t ~key:k ~data:i;
        Hashtbl.invariant ignore ignore t;
        verify_inserted t;
        loop (i + 1) t)
    in
    loop 0 t;
    List.iter !inserted ~f:(fun (x, _) ->
      Hashtbl.remove t x;
      Hashtbl.invariant ignore ignore t;
      (match Hashtbl.find t x with
       | None -> ()
       | Some _ -> failwith (Printf.sprintf "present after removal: %d" x));
      inserted := List.Assoc.remove !inserted ~equal x;
      verify_inserted t)
  ;;

  let%test_unit "clear" =
    let t = Hashtbl.create_poly () ~size:1 in
    let l = List.range 0 100 in
    let verify_present l = List.for_all l ~f:(Hashtbl.mem t) in
    let verify_not_present l = List.for_all l ~f:(fun i -> not (Hashtbl.mem t i)) in
    List.iter l ~f:(fun i -> Hashtbl.set t ~key:i ~data:(i * i));
    List.iter l ~f:(fun i -> Hashtbl.set t ~key:i ~data:(i * i));
    assert (Hashtbl.length t = 100);
    assert (verify_present l);
    Hashtbl.clear t;
    Hashtbl.invariant ignore ignore t;
    assert (Hashtbl.length t = 0);
    assert (verify_not_present l);
    let l = List.take l 42 in
    List.iter l ~f:(fun i -> Hashtbl.set t ~key:i ~data:(i * i));
    assert (Hashtbl.length t = 42);
    assert (verify_present l);
    Hashtbl.invariant ignore ignore t
  ;;

  let%test_unit "mem" =
    let t = Hashtbl.create_poly () ~size:1 in
    Hashtbl.invariant ignore ignore t;
    assert (not (Hashtbl.mem t "Fred"));
    Hashtbl.invariant ignore ignore t;
    Hashtbl.set t ~key:"Fred" ~data:"Wilma";
    Hashtbl.invariant ignore ignore t;
    assert (Hashtbl.mem t "Fred");
    Hashtbl.invariant ignore ignore t;
    Hashtbl.remove t "Fred";
    Hashtbl.invariant ignore ignore t;
    assert (not (Hashtbl.mem t "Fred"));
    Hashtbl.invariant ignore ignore t
  ;;

  let%test_unit "exists" =
    let t = Hashtbl.create_poly () in
    assert (not (Hashtbl.exists t ~f:(fun _ -> failwith "can't be called")));
    assert (not (Hashtbl.existsi t ~f:(fun ~key:_ ~data:_ -> failwith "can't be called")));
    Hashtbl.set t ~key:7 ~data:3;
    assert (not (Hashtbl.exists t ~f:(Int.equal 4)));
    Hashtbl.set t ~key:8 ~data:4;
    assert (Hashtbl.exists t ~f:(Int.equal 4));
    Hashtbl.set t ~key:9 ~data:5;
    assert (Hashtbl.existsi t ~f:(fun ~key ~data -> key + data = 14))
  ;;

  let%test_unit "for_all" =
    let t = Hashtbl.create_poly () in
    assert (Hashtbl.for_all t ~f:(fun _ -> failwith "can't be called"));
    assert (Hashtbl.for_alli t ~f:(fun ~key:_ ~data:_ -> failwith "can't be called"));
    Hashtbl.set t ~key:7 ~data:3;
    assert (Hashtbl.for_all t ~f:(fun x -> Int.equal x 3));
    Hashtbl.set t ~key:8 ~data:4;
    assert (not (Hashtbl.for_all t ~f:(fun x -> Int.equal x 3)));
    Hashtbl.set t ~key:9 ~data:5;
    assert (Hashtbl.for_alli t ~f:(fun ~key ~data -> key - 4 = data))
  ;;

  let%test_unit "count" =
    let t = Hashtbl.create_poly () in
    assert (Hashtbl.count t ~f:(fun _ -> failwith "can't be called") = 0);
    assert (Hashtbl.counti t ~f:(fun ~key:_ ~data:_ -> failwith "can't be called") = 0);
    Hashtbl.set t ~key:7 ~data:3;
    assert (Hashtbl.count t ~f:(fun x -> Int.equal x 3) = 1);
    Hashtbl.set t ~key:8 ~data:4;
    assert (Hashtbl.count t ~f:(fun x -> Int.equal x 3) = 1);
    Hashtbl.set t ~key:9 ~data:5;
    assert (Hashtbl.counti t ~f:(fun ~key ~data -> key - 4 = data) = 3)
  ;;

  let%test_unit "merge" =
    let make alist = Hashtbl.of_alist_poly_exn alist in
    let t1 = make [ 1, 111; 2, 222; 3, 333 ] in
    let t2 = make [ 1, 123; 2, 222; 4, 444 ] in
    [%test_result: (int * [ `Left of int | `Right of int | `Both of int * int ]) List.t]
      (Hashtbl.merge t1 t2 ~f:(fun ~key:_ ->
         function
         | `Left x -> Some (`Left x)
         | `Right y -> Some (`Right y)
         | `Both (x, y) -> if x = y then None else Some (`Both (x, y)))
       |> Hashtbl.to_alist
       |> List.sort ~compare:(fun (x, _) (y, _) -> Int.compare x y))
      ~expect:[ 1, `Both (111, 123); 3, `Left 333; 4, `Right 444 ]
  ;;
end
