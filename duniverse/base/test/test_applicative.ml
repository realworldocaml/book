open! Import

module Test_applicative_s (A : Applicative.S with type 'a t := 'a Or_error.t) :
  Applicative.S with type 'a t := 'a Or_error.t = struct
  let error = Or_error.error_string
  let return = A.return

  let%expect_test _ =
    print_s [%sexp (return "okay" : string Or_error.t)];
    [%expect {| (Ok okay) |}]
  ;;

  let apply = A.apply

  let%expect_test _ =
    let test x y = print_s [%sexp (apply x y : string Or_error.t)] in
    test (Ok String.capitalize) (Ok "okay");
    [%expect {| (Ok Okay) |}];
    test (error "not okay") (Ok "okay");
    [%expect {| (Error "not okay") |}];
    test (Ok String.capitalize) (error "not okay");
    [%expect {| (Error "not okay") |}];
    test (error "no fun") (error "no arg");
    [%expect {| (Error ("no fun" "no arg")) |}]
  ;;

  let ( <*> ) = A.( <*> )

  let%expect_test _ =
    let test x y = print_s [%sexp (x <*> y : string Or_error.t)] in
    test (Ok String.capitalize) (Ok "okay");
    [%expect {| (Ok Okay) |}];
    test (error "not okay") (Ok "okay");
    [%expect {| (Error "not okay") |}];
    test (Ok String.capitalize) (error "not okay");
    [%expect {| (Error "not okay") |}];
    test (error "no fun") (error "no arg");
    [%expect {| (Error ("no fun" "no arg")) |}]
  ;;

  let ( *> ) = A.( *> )

  let%expect_test _ =
    let test x y = print_s [%sexp (x *> y : string Or_error.t)] in
    test (Ok ()) (Ok "kay");
    [%expect {| (Ok kay) |}];
    test (error "not okay") (Ok "kay");
    [%expect {| (Error "not okay") |}];
    test (Ok ()) (error "not okay");
    [%expect {| (Error "not okay") |}];
    test (error "no fst") (error "no snd");
    [%expect {| (Error ("no fst" "no snd")) |}]
  ;;

  let ( <* ) = A.( <* )

  let%expect_test _ =
    let test x y = print_s [%sexp (x <* y : string Or_error.t)] in
    test (Ok "okay") (Ok ());
    [%expect {| (Ok okay) |}];
    test (error "not okay") (Ok ());
    [%expect {| (Error "not okay") |}];
    test (Ok "okay") (error "not okay");
    [%expect {| (Error "not okay") |}];
    test (error "no fst") (error "no snd");
    [%expect {| (Error ("no fst" "no snd")) |}]
  ;;

  let both = A.both

  let%expect_test _ =
    let test x y = print_s [%sexp (both x y : (string * string) Or_error.t)] in
    test (Ok "o") (Ok "kay");
    [%expect {| (Ok (o kay)) |}];
    test (error "not okay") (Ok "kay");
    [%expect {| (Error "not okay") |}];
    test (Ok "o") (error "not okay");
    [%expect {| (Error "not okay") |}];
    test (error "no fst") (error "no snd");
    [%expect {| (Error ("no fst" "no snd")) |}]
  ;;

  let map = A.map

  let%expect_test _ =
    let test x = print_s [%sexp (map x ~f:String.capitalize : string Or_error.t)] in
    test (Ok "okay");
    [%expect {| (Ok Okay) |}];
    test (error "not okay");
    [%expect {| (Error "not okay") |}]
  ;;

  let ( >>| ) = A.( >>| )

  let%expect_test _ =
    let test x = print_s [%sexp (x >>| String.capitalize : string Or_error.t)] in
    test (Ok "okay");
    [%expect {| (Ok Okay) |}];
    test (error "not okay");
    [%expect {| (Error "not okay") |}]
  ;;

  let map2 = A.map2

  let%expect_test _ =
    let test x y = print_s [%sexp (map2 x y ~f:( ^ ) : string Or_error.t)] in
    test (Ok "o") (Ok "kay");
    [%expect {| (Ok okay) |}];
    test (error "not okay") (Ok "kay");
    [%expect {| (Error "not okay") |}];
    test (Ok "o") (error "not okay");
    [%expect {| (Error "not okay") |}];
    test (error "no fst") (error "no snd");
    [%expect {| (Error ("no fst" "no snd")) |}]
  ;;

  let map3 = A.map3

  let%expect_test _ =
    let test x y z =
      print_s [%sexp (map3 x y z ~f:(fun a b c -> a ^ b ^ c) : string Or_error.t)]
    in
    test (Ok "o") (Ok "k") (Ok "ay");
    [%expect {| (Ok okay) |}];
    test (error "not okay") (Ok "k") (Ok "ay");
    [%expect {| (Error "not okay") |}];
    test (Ok "o") (error "not okay") (Ok "ay");
    [%expect {| (Error "not okay") |}];
    test (Ok "o") (Ok "k") (error "not okay");
    [%expect {| (Error "not okay") |}];
    test (error "no 1st") (error "no 2nd") (error "no 3rd");
    [%expect {| (Error ("no 1st" "no 2nd" "no 3rd")) |}]
  ;;

  let all = A.all

  let%expect_test _ =
    let test list = print_s [%sexp (all list : string list Or_error.t)] in
    test [];
    [%expect {| (Ok ()) |}];
    test [ Ok "okay" ];
    [%expect {| (Ok (okay)) |}];
    test [ Ok "o"; Ok "kay" ];
    [%expect {| (Ok (o kay)) |}];
    test [ Ok "o"; Ok "k"; Ok "ay" ];
    [%expect {| (Ok (o k ay)) |}];
    test [ error "oh no!" ];
    [%expect {| (Error "oh no!") |}];
    test [ error "oh no!"; Ok "okay" ];
    [%expect {| (Error "oh no!") |}];
    test [ Ok "okay"; error "oh no!" ];
    [%expect {| (Error "oh no!") |}];
    test [ error "oh no!"; Ok "o"; Ok "kay" ];
    [%expect {| (Error "oh no!") |}];
    test [ Ok "o"; error "oh no!"; Ok "aay" ];
    [%expect {| (Error "oh no!") |}];
    test [ Ok "o"; Ok "kay"; error "oh no!" ];
    [%expect {| (Error "oh no!") |}];
    test [ error "oh"; error "no"; error "!" ];
    [%expect {| (Error (oh no !)) |}]
  ;;

  let all_unit = A.all_unit

  let%expect_test _ =
    let test list = print_s [%sexp (all_unit list : unit Or_error.t)] in
    test [];
    [%expect {| (Ok ()) |}];
    test [ Ok () ];
    [%expect {| (Ok ()) |}];
    test [ Ok (); Ok () ];
    [%expect {| (Ok ()) |}];
    test [ Ok (); Ok (); Ok () ];
    [%expect {| (Ok ()) |}];
    test [ error "oh no!" ];
    [%expect {| (Error "oh no!") |}];
    test [ error "oh no!"; Ok () ];
    [%expect {| (Error "oh no!") |}];
    test [ Ok (); error "oh no!" ];
    [%expect {| (Error "oh no!") |}];
    test [ error "oh no!"; Ok (); Ok () ];
    [%expect {| (Error "oh no!") |}];
    test [ Ok (); error "oh no!"; Ok () ];
    [%expect {| (Error "oh no!") |}];
    test [ Ok (); Ok (); error "oh no!" ];
    [%expect {| (Error "oh no!") |}];
    test [ error "oh"; error "no"; error "!" ];
    [%expect {| (Error (oh no !)) |}]
  ;;

  module Applicative_infix = A.Applicative_infix
end

let%test_module "Make" =
  (module Test_applicative_s (Applicative.Make (struct
                                type 'a t = 'a Or_error.t

                                let return = Or_error.return
                                let apply = Or_error.apply
                                let map = `Define_using_apply
                              end)))
;;

let%test_module "Make" =
  (module Test_applicative_s (Applicative.Make_using_map2 (struct
                                type 'a t = 'a Or_error.t

                                let return = Or_error.return
                                let map2 = Or_error.map2
                                let map = `Define_using_map2
                              end)))
;;

(* While law-abiding applicatives shouldn't be relying functions being called
   the minimal number of times, it is good for performance that things be this
   way. For many applicatives this will not matter very much, but for others,
   like Bonsai, it is a little more significant, since extra calls construct
   more Incremental nodes, yielding more strain on the Incremental stabilizer.

   The point is that we should not assume that the input applicative instance
   can be frivolous in creating nodes in the applicative call-tree.
*)
let%expect_test _ =
  let module A = struct
    type 'a t =
      | Other of string
      | Return : 'a -> 'a t
      | Map : ('a -> 'b) * 'a t -> 'b t
      | Map2 : ('a -> 'b -> 'c) * 'a t * 'b t -> 'c t

    include Applicative.Make_using_map2 (struct
        type nonrec 'a t = 'a t

        let return x = Return x
        let map2 a b ~f = Map2 (f, a, b)
        let map = `Custom (fun a ~f -> Map (f, a))
      end)

    let rec sexp_of_t : type a. a t -> Sexp.t = function
      | Other x -> Atom x
      | Return _ -> Atom "Return"
      | Map (_, a) -> List [ Atom "Map"; sexp_of_t a ]
      | Map2 (_, a, b) -> List [ Atom "Map2"; sexp_of_t a; sexp_of_t b ]
    ;;
  end
  in
  let open A in
  let test x = print_s [%sexp (x : A.t)] in
  let a, b, c, d = Other "A", Other "B", Other "C", Other "D" in
  test (map2 a b ~f:(fun a b -> a, b));
  [%expect {| (Map2 A B) |}];
  test (both a b);
  [%expect {| (Map2 A B) |}];
  test (all_unit [ a; b; c; d ]);
  [%expect {|
    (Map2 (Map2 (Map2 (Map2 Return A) B) C) D) |}];
  test (a *> b);
  [%expect {| (Map2 A B) |}]
;;

(* These functors serve only to check that the signatures for various Foo and Foo2 module
   types don't drift apart over time. *)
module _ = struct
  open Applicative

  (* Applicative_infix to Applicative_infix2 *)

  module _ (X : Applicative_infix) : Applicative_infix2 with type ('a, 'e) t = 'a X.t =
  struct
    include X

    type ('a, 'e) t = 'a X.t
  end

  (* Applicative_infix2 to Applicative_infix *)
  module _ (X : Applicative_infix2) : Applicative_infix with type 'a t = ('a, unit) X.t =
  struct
    include X

    type 'a t = ('a, unit) X.t
  end

  (* Applicative_infix2 to Applicative_infix3 *)
  module _ (X : Applicative_infix2) :
    Applicative_infix3 with type ('a, 'd, 'e) t = ('a, 'd) X.t = struct
    include X

    type ('a, 'd, 'e) t = ('a, 'd) X.t
  end

  (* Applicative_infix3 to Applicative_infix2 *)
  module _ (X : Applicative_infix3) :
    Applicative_infix2 with type ('a, 'd) t = ('a, 'd, unit) X.t = struct
    include X

    type ('a, 'd) t = ('a, 'd, unit) X.t
  end

  (* Let_syntax to Let_syntax2 *)
  module _ (X : Let_syntax) : Let_syntax2 with type ('a, 'e) t = 'a X.t = struct
    include X

    type ('a, 'e) t = 'a X.t
  end

  (* Let_syntax2 to Let_syntax *)
  module _ (X : Let_syntax2) : Let_syntax with type 'a t = ('a, unit) X.t = struct
    include X

    type 'a t = ('a, unit) X.t
  end

  (* Let_syntax2 to Let_syntax3 *)
  module _ (X : Let_syntax2) : Let_syntax3 with type ('a, 'd, 'e) t = ('a, 'd) X.t =
  struct
    include X

    type ('a, 'd, 'e) t = ('a, 'd) X.t
  end

  (* Let_syntax3 to Let_syntax2 *)
  module _ (X : Let_syntax3) : Let_syntax2 with type ('a, 'd) t = ('a, 'd, unit) X.t =
  struct
    include X

    type ('a, 'd) t = ('a, 'd, unit) X.t
  end
end
