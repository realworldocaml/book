open! Core_kernel
open! Import
open! Memo

let%test_module "lru" =
  (module struct
    let count = ref 0

    (* number of times f underlying function is run *)
    let f =
      general ~cache_size_bound:3 (fun i ->
        incr count;
        i)
    ;;

    let%test _ = f 0 = 0
    let%test _ = !count = 1
    let%test _ = f 1 = 1
    let%test _ = !count = 2
    let%test _ = f 0 = 0
    let%test _ = !count = 2
    let%test _ = f 3 = 3
    (* cache full *)
    let%test _ = !count = 3
    let%test _ = f 4 = 4
    (* evict 1 *)
    let%test _ = !count = 4
    let%test _ = f 0 = 0
    let%test _ = !count = 4
    let%test _ = f 1 = 1
    (* recompute 1 *)
    let%test _ = !count = 5
  end)
;;

let%test_module "comparable" =
  (module struct
    module Env = struct
      type t =
        { should_raise : bool ref
        ; f : int -> int
        }

      let create () =
        let should_raise = ref false in
        let f =
          of_comparable
            (module Int)
            (fun i ->
               print_s [%message "Computing value" (i : int)];
               if !should_raise then raise_s [%message "I should raise"];
               i + 1)
        in
        { should_raise; f }
      ;;
    end

    let%expect_test "Ensure no re computation" =
      let { Env.should_raise; f } = Env.create () in
      printf "%d" (f 5);
      [%expect {|
        ("Computing value" (i 5))
        6 |}];
      should_raise := true;
      printf "%d" (f 5);
      [%expect {| 6 |}]
    ;;

    let%expect_test "What happens if f raises" =
      let { Env.should_raise; f } = Env.create () in
      should_raise := true;
      show_raise (fun () -> printf "%d" (f 5));
      [%expect {|
        ("Computing value" (i 5))
        (raised "I should raise") |}];
      should_raise := false;
      show_raise (fun () -> printf "%d" (f 5));
      [%expect {| (raised "I should raise") |}]
    ;;
  end)
;;
