open! Base
open Base_quickcheck

module Variant : sig
  type t =
    | Nullary
    | Unary of bool
    | Binary of int * float
    | N_ary of string list
  [@@deriving quickcheck]
end = struct
  type t =
    | Nullary
    | Unary of bool
    | Binary of int * float
    | N_ary of string list
  [@@deriving compare, quickcheck, sexp_of]

  (* Perhaps we want more examples of the variants with more contents. We can override the
     default derived generator with one that uses [Generator.weighted_union] to skew the
     distribution. *)
  let quickcheck_generator =
    Generator.weighted_union
      [ 1., Generator.return Nullary
      ; 2., [%quickcheck.generator: bool] |> Generator.map ~f:(fun bool -> Unary bool)
      ; ( 4.
        , [%quickcheck.generator: int * float]
          |> Generator.map ~f:(fun (int, float) -> Binary (int, float)) )
      ; ( 10.
        , [%quickcheck.generator: string list]
          |> Generator.map ~f:(fun list -> N_ary list) )
      ]
  ;;

  (* We can test our distribution: *)
  let%expect_test _ =
    let open Expect_test_helpers_core in
    Test.with_sample_exn
      quickcheck_generator
      ~config:{ Test.default_config with test_count = 20 }
      ~f:(fun sequence ->
        sequence
        |> Sequence.to_list
        |> List.sort ~compare
        |> List.map ~f:sexp_of_t
        |> List.iter ~f:print_s);
    [%expect
      {|
      Nullary
      Nullary
      (Binary -58823712978749242 1.326895442392441E+36)
      (Binary -192459552073625 2.75)
      (Binary 85 -0)
      (Binary 52814 11.770832120594708)
      (Binary 870067995 1048576)
      (Binary 1757545005705 -13.928729046486296)
      (Binary 195313760848289 -1.75)
      (Binary 1215235890521588953 1.7728695309706382)
      (N_ary ())
      (N_ary ())
      (N_ary (""))
      (N_ary ("" "" "" "" "" "" "\222" y @))
      (N_ary (5K 2))
      (N_ary (B Mh))
      (N_ary (DM))
      (N_ary (L7N))
      (N_ary (bAW6zR `y7O 1V7))
      (N_ary ("\219\171nqZ" "asa\250Y")) |}]
  ;;
end

module Record : sig
  type t =
    { rationals : float list
    ; index : int
    }
  [@@deriving quickcheck]
end = struct
  type t =
    { rationals : float list
    ; index : int
    }
  [@@deriving compare, quickcheck, sexp_of]

  (* We might want to choose [rationals] from a distribution of finite floats, and for
     [index] to be a legal index into [rationals]. We can override the default derived
     generator with one that uses and some custom distributions, as well as
     [Generator.bind] so the choice of [index] can depend on the choice of [rationals]. *)
  let quickcheck_generator =
    let open Generator.Let_syntax in
    let%bind rationals = Generator.list_non_empty Generator.float_positive_or_zero in
    let%bind index = Generator.int_uniform_inclusive 0 (List.length rationals - 1) in
    return { rationals; index }
  ;;

  (* We can test our distribution: *)
  let%expect_test _ =
    let open Expect_test_helpers_core in
    Test.with_sample_exn
      quickcheck_generator
      ~config:{ Test.default_config with test_count = 20 }
      ~f:(fun sequence ->
        sequence
        |> Sequence.to_list
        |> List.sort ~compare
        |> List.stable_sort
             ~compare:
               (Comparable.lift Int.ascending ~f:(fun t -> List.length t.rationals))
        |> List.map ~f:sexp_of_t
        |> List.iter ~f:print_s);
    [%expect
      {|
      ((rationals (2.8280262305352377E-308)) (index 0))
      ((rationals (4.2366697150646817E-308)) (index 0))
      ((rationals (1.3789672079675011E-186)) (index 0))
      ((rationals (0.01171875)) (index 0))
      ((rationals (1.9795913696289062)) (index 0))
      ((rationals (2.75)) (index 0))
      ((rationals (8)) (index 0))
      ((rationals (8)) (index 0))
      ((rationals (1157627904)) (index 0))
      ((rationals (6.1431989399976011E+106)) (index 0))
      ((rationals (5.5427886579169E+199)) (index 0))
      ((rationals (9.90958590546204E+307)) (index 0))
      ((rationals (1.7550194473742371E+308)) (index 0))
      ((rationals (0.8616666835732758 1.1385925121758191)) (index 1))
      ((rationals (2.8237828741950466E-13 1.8224854137420152 0.01824775352821284))
       (index 2))
      ((rationals (1.7976931348614984E+308 0.05622608376936 0.466064453125))
       (index 2))
      ((rationals (
         1.6166417891736373E-20
         2.2250738585072009E-308
         6.27752527870636E-24
         1.7360032353802967E+308
         0.36672276957627137
         59045.107299804688
         2.9870388295270649E+111
         3.5407791527709811E-308
         2853992448
         2.20056001438752
         1.9901765337663721E+76
         18014397435740160
         5.007667344459188E+230
         2.0470928690338857E+18))
       (index 11))
      ((rationals (
         1.40631103515625
         0.99999618530273438
         0.14919622650404563
         9.470385479937987E+307
         3.4916006318991669E+46
         7.144487913378675E+94
         6.1754202605911972E-23
         5.1212177483591671E+252
         0.19299798905001353
         3.0922966003417969
         1.07706310793392E-321
         3.5288404323765937E-71
         192
         1.5763809981878558E+308))
       (index 9))
      ((rationals (
         1.0061122386630193E-15
         1.2983366397190109E+274
         1.5967894544723646E-20
         5.8227197141673614E-25
         1.05190200652924E-313
         5.2438958771526814
         1.1424624566895193E+280
         446422.03912734985
         1.7807147461350098E+308
         0.84189605712890625
         9.8710317674614133E-178
         1.6875
         2.79233146312789E-16
         4.6931086289980425E-05
         4.46562265651841E+101
         3.75))
       (index 5))
      ((rationals (
         23828.06884765625
         1.3858716369066866E-143
         7.9926092692070288E-33
         0.0060125291347503662
         5.2492643594741821
         2.9802322359939737E-08
         0.00014042180987416941
         3.638671875
         1.28993722023237E-317
         7.242851245042631E-12
         0.062499999534338713
         0.002296784776262939
         237.88091122743026
         2.2765450197646858E-274
         126.14432978630066
         2.716326520622772E+60))
       (index 6)) |}]
  ;;
end
