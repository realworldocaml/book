open! Core_kernel
open Poly
open! Import
open! Blang

let andalso = O.( && )
let orelse = O.( || )

let%test_module "Stable.V1" =
  (module Stable_unit_test.Make (struct
       type t = string Stable.V1.t [@@deriving sexp, bin_io]

       let equal = Poly.( = )

       let test_blang =
         if_
           (base "foo")
           (not_ (or_ [ base "bara"; base "barb" ]))
           (not_ (and_ [ base "baza"; base "bazb" ]))
       ;;

       let test_sexp = "(if foo (not (or bara barb)) (not (and baza bazb)))"

       let test_bin =
         "\005\006\003foo\004\003\006\004bara\006\004barb\004\002\006\004baza\006\004bazb"
       ;;

       let tests =
         [ test_blang, test_sexp, test_bin; true_, "true", "\000"; false_, "false", "\001" ]
       ;;
     end))
;;

let%test_module "auto-simplification" =
  (module struct
    let a, b, c = base 1, base 2, base 3

    let ( = ) a b =
      invariant a;
      invariant b;
      Poly.( = ) a b
    ;;

    let%test _ = not_ true_ = false_
    let%test _ = not_ false_ = true_
    let%test _ = not_ (not_ a) = a
    let%test _ = andalso true_ b = b
    let%test _ = andalso a true_ = a
    let%test _ = andalso false_ b = false_
    let%test _ = andalso a false_ = false_
    let%test _ = orelse false_ b = b
    let%test _ = orelse a false_ = a
    let%test _ = orelse true_ b = true_
    let%test _ = orelse a true_ = true_
    let%test _ = if_ true_ b c = b
    let%test _ = if_ false_ b c = c
    let%test _ = if_ a true_ c = orelse a c
    let%test _ = if_ a b false_ = andalso a b
    let%test _ = if_ a b true_ = if_ (not_ a) true_ b
    (* b/c (if a b c) = (if (not a) c b) *)
    let%test _ = if_ a b true_ = orelse (not_ a) b
    let%test _ = if_ a false_ c = if_ (not_ a) c false_
    (* b/c (if a b c) = (if (not a) c b) *)
    let%test _ = if_ a false_ c = andalso (not_ a) c

    let%test_module "n-ary-and-or" =
      (module struct
        let%test _ = and_ [ a; b; c ] = andalso (andalso a b) c
        let%test _ = or_ [ a; b; c ] = orelse (orelse a b) c

        let test_and ts = and_ ts = List.fold ts ~init:true_ ~f:andalso
        let test_or ts = or_ ts = List.fold ts ~init:false_ ~f:orelse

        let%test _ = test_or []
        let%test _ = test_or [ a ]
        let%test _ = test_or [ true_ ]
        let%test _ = test_or [ false_ ]
        let%test _ = test_or [ a; true_; b ]
        let%test _ = test_or [ a; false_; b ]
        let%test _ = test_and []
        let%test _ = test_and [ a ]
        let%test _ = test_and [ true_ ]
        let%test _ = test_and [ false_ ]
        let%test _ = test_and [ a; true_; b ]
        let%test _ = test_and [ a; false_; b ]
      end)
    ;;
  end)
;;

let%test _ =
  [ 1; 2; 3; 4; 5; 6; 7 ]
  = values
      (and_
         [ or_ [ base 1; base 2 ]
         ; base 3
         ; true_
         ; if_ (base 4) (base 5) (base 6)
         ; not_ (base 7)
         ])
;;

let%test _ = gather_conjuncts (base 1) = [ base 1 ]
let%test _ = gather_conjuncts (and_ []) = []
let%test _ = gather_conjuncts (and_ [ base 1 ]) = [ base 1 ]
let%test _ = gather_conjuncts (and_ [ base 1; base 2 ]) = [ base 1; base 2 ]

let%test _ =
  gather_conjuncts (and_ [ base 1; base 2; base 3 ]) = [ base 1; base 2; base 3 ]
;;

let%test _ =
  gather_conjuncts
    (and_
       [ and_ [ and_ [ base 1; base 2 ]; base 3 ]
       ; and_ [ or_ [ base 4; base 5 ]; and_ [ base 6; base 7 ] ]
       ])
  = [ base 1; base 2; base 3; or_ [ base 4; base 5 ]; base 6; base 7 ]
;;

let%test _ = gather_disjuncts (base 1) = [ base 1 ]
let%test _ = gather_disjuncts (or_ []) = []
let%test _ = gather_disjuncts (or_ [ base 1 ]) = [ base 1 ]
let%test _ = gather_disjuncts (or_ [ base 1; base 2 ]) = [ base 1; base 2 ]

let%test _ =
  gather_disjuncts (or_ [ base 1; base 2; base 3 ]) = [ base 1; base 2; base 3 ]
;;

let%test _ =
  gather_disjuncts
    (or_
       [ or_ [ or_ [ base 1; base 2 ]; base 3 ]
       ; or_ [ and_ [ base 4; base 5 ]; or_ [ base 6; base 7 ] ]
       ])
  = [ base 1; base 2; base 3; and_ [ base 4; base 5 ]; base 6; base 7 ]
;;

let%test_module "bind short-circuiting" =
  (module struct
    let test expected_visits expr =
      let visited = ref [] in
      let f var =
        visited := var :: !visited;
        false_
      in
      match bind expr ~f with
      | True -> List.equal Int.equal expected_visits (List.rev !visited)
      | _ -> false
    ;;

    let%test _ = test [ 0 ] (or_ [ not_ (base 0); base 1 ])
    let%test _ = test [ 0; 1 ] (not_ (and_ [ not_ (base 0); base 1; base 2 ]))
    let%test _ = test [ 0; 2 ] (if_ (base 0) (base 1) (not_ (base 2)))
  end)
;;

let%test_module "laws" =
  (module struct
    type base =
      | A
      | B
      | C
    [@@deriving sexp_of]

    type 'a base_fun = base -> 'a

    let sexp_of_base_fun sexp_of_a (f : 'a base_fun) =
      Sexp.List
        [ Sexp.Atom "function"
        ; Sexp.List [ Sexp.Atom "A"; Sexp.Atom "->"; sexp_of_a (f A) ]
        ; Sexp.List [ Sexp.Atom "B"; Sexp.Atom "->"; sexp_of_a (f B) ]
        ; Sexp.List [ Sexp.Atom "C"; Sexp.Atom "->"; sexp_of_a (f C) ]
        ]
    ;;

    module Gen = struct
      (* all random values are generated from a fixed PRNG seed so that
         unit tests are deterministic *)
      let prng =
        Random.State.make
          (String.to_list
             "31bb128c352e2569228fbacc590e937a29a8bb8fc4bfe7126504ce3dc400be7f401fa6f5be5dba38"
           |> Array.of_list
           |> Array.map ~f:Char.to_int)
      ;;

      let bool () = Random.State.bool prng
      let element arr = arr.(Random.State.int prng (Array.length arr))

      let gen_blang gen_base =
        let atomic =
          [| (fun () -> constant (bool ())); (fun () -> base (gen_base ())) |]
        in
        let composite =
          [| (fun rand -> not_ (rand ()))
           ; (fun rand -> andalso (rand ()) (rand ()))
           ; (fun rand -> orelse (rand ()) (rand ()))
           ; (fun rand -> if_ (rand ()) (rand ()) (rand ()))
          |]
        in
        let rec aux ~depth =
          if depth <= 1
          then element atomic ()
          else element composite (fun () -> aux ~depth:(depth - 1))
        in
        aux
      ;;

      let gen_base =
        let bases = [| A; B; C |] in
        fun () -> element bases
      ;;

      let gen_base_fun codomain () =
        let a_val = element codomain in
        let b_val = element codomain in
        let c_val = element codomain in
        function
        | A -> a_val
        | B -> b_val
        | C -> c_val
      ;;

      let t () = gen_blang gen_base ~depth:5
      let f = gen_base_fun [| true; false |]
      let g = gen_base_fun [| `Unknown; `Known true; `Known false |]
      let tf () = t (), f ()
      let tg () = t (), g ()
    end

    let law gen sexp_of run =
      for _ = 0 to 100 do
        let arg = gen () in
        if not (run arg) then failwith (Sexp.to_string (sexp_of arg))
      done
    ;;

    let forall_t = law Gen.t [%sexp_of: base t]
    let forall_tf = law Gen.tf [%sexp_of: base t * bool base_fun]

    let forall_tg =
      law Gen.tg [%sexp_of: base t * [ `Known of bool | `Unknown ] base_fun]
    ;;

    let%test_unit _ = forall_t (fun t -> specialize t (fun _ -> `Unknown) = t)

    let%test_unit _ =
      forall_tf (fun (t, f) ->
        specialize t (fun x -> `Known (f x)) = constant (eval t f))
    ;;

    let%test_unit _ =
      forall_tg (fun (t, g) ->
        List.for_all (values (specialize t g)) ~f:(fun x -> g x = `Unknown))
    ;;

    let%test_unit _ =
      forall_tg (fun (t, g) ->
        (* an arbitrary [f] such that [f x = b] whenever [g x = `Known b] *)
        let f =
          let rand_fval x =
            match g x with
            | `Known b -> b
            | `Unknown -> Gen.bool ()
          in
          let a_val = rand_fval A in
          let b_val = rand_fval B in
          let c_val = rand_fval C in
          function
          | A -> a_val
          | B -> b_val
          | C -> c_val
        in
        eval t f = eval (specialize t g) f)
    ;;

    let%test_module "eval_set" =
      (module struct
        type base_set =
          | Odd
          | Even
          | Greater_than of int
          | Smaller_than of int
        [@@deriving sexp_of]

        let size = 10
        let universe = lazy (List.init size ~f:Fn.id |> Int.Set.of_list)

        let gen_base =
          let bases =
            [| Odd; Even; Greater_than (size / 2); Smaller_than (size / 2) |]
          in
          fun () -> Gen.element bases
        ;;

        let t () = Gen.gen_blang gen_base ~depth:5

        let set_of_base =
          Memo.general (fun t ->
            Int.Set.filter (force universe) ~f:(fun e ->
              match t with
              | Odd -> e mod 2 = 1
              | Even -> e mod 2 = 0
              | Greater_than x -> e > x
              | Smaller_than x -> e < x))
        ;;

        let run expression =
          let expect =
            Set.filter (force universe) ~f:(fun e ->
              eval expression (fun base -> Int.Set.mem (set_of_base base) e))
          in
          try
            [%test_result: Int.Set.t] ~expect (eval_set ~universe set_of_base expression)
          with
          | exn ->
            failwiths
              "fail on expression"
              (expression, exn)
              [%sexp_of: base_set t * Exn.t]
        ;;

        let%test_unit _ =
          for _ = 0 to 100 do
            run (t ())
          done
        ;;
      end)
    ;;
  end)
;;

let%expect_test "no-alloc-eval" =
  let blang =
    if_
      (base "foo")
      (not_ (or_ [ base "bara"; base "barb" ]))
      (not_ (and_ [ base "baza"; base "bazb" ]))
  in
  require_no_allocation [%here] (fun () ->
    let result = eval blang (fun _ -> false) in
    ignore (result : bool));
  [%expect {| |}]
;;

let%expect_test "quickcheck generator obeys invariants" =
  Quickcheck.test
    ~shrinker:[%quickcheck.shrinker: bool Blang.t]
    ~sexp_of:[%sexp_of: bool Blang.t]
    [%quickcheck.generator: bool Blang.t]
    ~f:Blang.invariant;
  [%expect {| |}]
;;
