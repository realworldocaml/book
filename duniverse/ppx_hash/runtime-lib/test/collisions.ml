open Core_kernel

module Tests(Hash : Base.Hash.S with type hash_value = int) = struct

  module Ppx_hash_lib = struct
    module Std = struct module Hash = Base.Hash.F(Hash) end
  end
  open Ppx_hash_lib.Std.Hash.Builtin
  let hash = `dont_use

  module State = struct
    module T = struct
      type t = Hash.state
      let compare = Hash.For_tests.compare_state
      let sexp_of_t s : Sexp.t = Atom (Hash.For_tests.state_to_string s)
      let t_of_sexp _ = assert false
    end
    include T
    include Comparable.Make(T)
  end

  let should_have_no_collisions list sexp_of_t hash =
    let m =
      State.Map.of_alist_multi (
        List.map list
          ~f:(fun v -> hash (Hash.reset (Hash.alloc ())) v, v))
    in
    Map.iteri m ~f:(fun ~key:hash ~data:values ->
      match values with
      | [] -> assert false
      | [ _ ] -> ()
      | _ :: _ :: _ ->
        failwiths "collision" (hash, values) [%sexp_of: State.t * t list])

  module Ints = struct

    let should_have_no_collisions l s f =
      should_have_no_collisions l s (fun s x -> hash_fold_int s (f x))

    (* these tests can have false positives, but those should be fixable by tweaking the
       [ints] list. *)
    let ints = [0;1;2;3;100;500;
                (*0x1234567812345678;*)
                -1; -2]

    let (%) a b = [%hash: int*int] (a,b)

    let zero = 0

    (* The following tests require incrementally increasing hash function quality *)

    let%test_unit "simple combine2 collisions" =
      should_have_no_collisions
        (List.cartesian_product ints ints)
        [%sexp_of: int * int]
        (fun (a,b) -> a % b)

    let%test_unit "more complicated combine2 collisions" =
      should_have_no_collisions
        (List.cartesian_product ints ints)
        [%sexp_of: int * int]
        (fun (a,b) -> a % (b % zero))

    let%test_unit "yet more complicated combine2s collisions" =
      should_have_no_collisions
        (List.cartesian_product ints ints)
        [%sexp_of: int * int]
        (fun (a, b) ->
           ((zero % a) % (b % zero)))

  end

  let hash_int x =
    let h = Hash.alloc () in
    let h = Hash.reset h in
    Hash.fold_int h x

  let init () =
    let h = Hash.alloc () in
    Hash.reset h

  let hash_string x =
    let h = Hash.alloc () in
    let h = Hash.reset h in
    Hash.fold_string h x

  let (=) x y = Hash.For_tests.compare_state x y = 0
  let assert_different hash_t a b =
    assert (not (
      hash_t a = hash_t b
    ))

  let%test_unit _ = assert_different hash_int 0 (1 lsl 32)

  let%test_unit _ =
    let a1 = String.make 7 'a' in
    let b = String.make 7 'b' in
    let a2 = String.make 7 'a' in
    let c = Some 5 in
    let a3 = String.make 7 'a' in
    let d = Obj.new_block Obj.abstract_tag 1 in
    assert (hash_string a1 = hash_string a2);
    assert (hash_string a1 = hash_string a3);
    assert (not (phys_same a1 b));
    assert (not (phys_same a1 c));
    assert (not (phys_same a1 d))

  let%test_unit _ = assert_different hash_string "\200\200\200\200" "\200a\200\200"
  let%test_unit _ = assert_different hash_string "\200\200\200" "\200a\200"

  let%test_unit "int collisions" =
    Map.to_alist (
      Int.Map.of_alist_multi
        (List.init 100_000
           ~f:(fun i ->
             (Hash.get_hash_value (hash_int i) land (1 lsl 17 - 1), i)
           )))
    |> List.iter ~f:(fun (_, vs) ->
      (*  the number 10 is motivated by 0.9999 being close enough
         to 1 in the following R expression:

         ppois(10, lambda = 10^5 / 2^17) ^ (2^17)
         [1] 0.9999167

         With enough hand-waving and invocation of Poisson limit theorem
         I convinced myself that poisson distribution is an OK approximation. *)
      [%test_pred: int] (fun x -> x <= 10) (List.length vs))


  let%test_unit "list collisions" =
    should_have_no_collisions
      [
        [];
        [[]];
        [[[]]];
        [[["hello"]]];
        [[]; []];
      ]
      [%sexp_of: string list list list]
      [%hash_fold: string list list list]

  type 'a array_frozen = 'a array
  let sexp_of_array_frozen = sexp_of_array

  let%test_unit "array collisions" =
    should_have_no_collisions
      [
        [||];
        [|[||]|];
        [|[|[||]|]|];
        [|[|[|"hello"|]|]|];
        [|[||]; [||]|];
      ]
      [%sexp_of: string array_frozen array_frozen array_frozen]
      [%hash_fold: string array_frozen array_frozen array_frozen]

  let%test_unit "string collisions" =
    should_have_no_collisions
      [
        "",[16 lsl 56; 0];
        (String.make 8 '\000' ^"\002" ^ String.make 7 '\000') , [];
      ]
      [%sexp_of: string * int list]
      [%hash_fold: string * int list]

end

module I = Tests(Base.Hash)
module S = Tests(Siphash_lib.Siphash)
module P = Tests(Perfect_hash)
