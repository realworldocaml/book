open Core
module Hash = Base.Hash

module Bench (Hash : Hash.S) = struct
  let%bench_module ("" [@name_suffix Hash.description]) =
    (module struct
      module Ppx_hash_lib = struct
        module Std = struct
          module Hash = Base.Hash.F (Hash)
        end
      end

      module Hash = Ppx_hash_lib.Std.Hash
      open Hash.Builtin

      type a = int [@@deriving hash]
      type b = a * float [@@deriving hash]

      type c =
        | Foo
        | Bar
        | Baz of a * b * c
      [@@deriving hash]

      type d = (int * string) list [@@deriving hash]

      let hash_fold_d : Ppx_hash_lib.Std.Hash.state -> d -> Ppx_hash_lib.Std.Hash.state =
        (*fun hsv  ->
          fun arg  ->*)
        hash_fold_list (fun hsv arg ->
          let e0, e1 = arg in
          hash_fold_string (hash_fold_int hsv e0) e1)
      ;;

      (*hsv
        arg*)

      let _ = Foo, Bar
      let a = 32
      let b = 32, 42.0
      let c1 = Foo
      let c2 = Baz (a, b, c1)
      let rec cn n = if n <= 1 then c1 else Baz (n, (n, float_of_int n), cn (n - 1))
      let c10 = cn 10
      let c100 = cn 100

      let rec dn n =
        if n <= 0
        then []
        else
          (n, String.init n ~f:(fun i -> if i mod 2 = 0 then 'j' else 's')) :: dn (n - 1)
      ;;

      let d10 = dn 10
      let d100 = dn 100

      let%bench "hash_init" = Hash.alloc ()

      let state = Hash.alloc ()
      let run folder x = ignore (Hash.get_hash_value (folder (Hash.reset state) x))

      let%bench "hash a" = run hash_fold_a a
      let%bench "hash b" = run hash_fold_b b
      let%bench "hash c__1" = run hash_fold_c c1
      let%bench "hash c__2" = run hash_fold_c c2
      let%bench "hash c_10" = run hash_fold_c c10
      let%bench "hash c100" = run hash_fold_c c100
      let%bench "hash d_10" = run hash_fold_d d10
      let%bench "hash d100" = run hash_fold_d d100

      let _ = c2, c10, c100, d10, d100
    end)
  ;;
end

module Bench_hashtbl_hash = struct
  (* This module is a verbatim copy of the above, except that we use Hashtbl.hash every
     where. *)
  let%bench_module "Hashtbl.hash" =
    (module struct
      type a = int
      type b = a * float

      type c =
        | Foo
        | Bar
        | Baz of a * b * c

      type d = (int * string) list

      let hash_a = Hashtbl.hash
      let hash_b = Hashtbl.hash
      let hash_c = Hashtbl.hash
      let hash_d = Hashtbl.hash
      let _ = Foo, Bar
      let a = 32
      let b = 32, 42.0
      let c1 = Foo
      let c2 = Baz (a, b, c1)
      let rec cn n = if n <= 1 then c1 else Baz (n, (n, float_of_int n), cn (n - 1))
      let c10 = cn 10
      let c100 = cn 100

      let rec dn n : d =
        if n <= 0
        then []
        else
          (n, String.init n ~f:(fun i -> if i mod 2 = 0 then 'j' else 's')) :: dn (n - 1)
      ;;

      let d10 = dn 10
      let d100 = dn 100
      let run f x = ignore (f x)

      let%bench "hash a" = run hash_a a
      let%bench "hash b" = run hash_b b
      let%bench "hash c__1" = run hash_c c1
      let%bench "hash c__2" = run hash_c c2
      let%bench "hash c_10" = run hash_c c10
      let%bench "hash c100" = run hash_c c100
      let%bench "hash d_10" = run hash_d d10
      let%bench "hash d100" = run hash_d d100

      let _ = c2, c10, c100, d10, d100
    end)
  ;;
end

module Traverse_only : Hash.S = struct
  let description = "Traverse_only"

  type hash_value = int
  type state = unit
  type seed = unit

  let alloc () = ()
  let reset ?seed:_ () = ()
  let get_hash_value () = 0
  let fold_int () _ = ()
  let fold_int64 () _ = ()
  let fold_float () _ = ()
  let fold_string () _ = ()

  module For_tests = struct
    let compare_state _ _ = 0
    let state_to_string () = "()"
  end
end

(* This module enforces the rules described in ../hash_intf.ml *)
module Check_initialized_correctly : Hash.S = struct
  let description = "Check_initialized_correctly"

  type hash_value = int

  type state =
    { me : int
    ; valid : int ref
    }

  type seed = unit

  let next_id =
    let x = ref 0 in
    fun () ->
      incr x;
      !x
  ;;

  let alloc () = { me = next_id (); valid = ref (next_id ()) }

  let reset ?seed:_ t =
    let me = next_id () in
    t.valid := me;
    { me; valid = t.valid }
  ;;

  let assert_valid t = assert (t.me = !(t.valid))

  let change t =
    assert_valid t;
    let me = next_id () in
    t.valid := me;
    { me; valid = t.valid }
  ;;

  let get_hash_value t =
    let _ = change t in
    0
  ;;

  let fold_int t _ = change t
  let fold_int64 t _ = change t
  let fold_float t _ = change t
  let fold_string t _ = change t

  module For_tests = struct
    let compare_state a b =
      assert_valid a;
      assert_valid b;
      0
    ;;

    let state_to_string _ = "<state>"
  end

  let should_fail f =
    match f () with
    | exception _e -> ()
    | _ -> failwith "should have failed"
  ;;

  let%test_unit _ =
    should_fail (fun () ->
      let x = alloc () in
      let y = reset x in
      ignore (fold_int y 1);
      fold_int y 2)
  ;;

  let%test_unit _ =
    should_fail (fun () ->
      let x = alloc () in
      let y = reset x in
      let y2 = reset x in
      ignore (fold_int y 1);
      ignore (fold_int y2 1))
  ;;

  let%test_unit _ =
    should_fail (fun () ->
      let x = alloc () in
      ignore (fold_int x 1))
  ;;

  let%test_unit _ =
    should_fail (fun () ->
      let x = alloc () in
      let x = reset x in
      ignore (get_hash_value x);
      ignore (fold_int x 1))
  ;;
end

let%bench_module "" = (module Bench (Traverse_only))
let%bench_module "" = (module Bench (Check_initialized_correctly))
let%bench_module "" = (module Bench (Base.Hash))
let%bench_module "" = (module Bench (Siphash_lib.Siphash))
let%bench_module "" = (module Bench (Ppx_hash_runtime_test.Perfect_hash))
