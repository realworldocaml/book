open! Core_kernel
open! Import
open! Timing_wheel

let max_time = Private.max_time

module Time_ns = struct
  include Time_ns

  module Span = struct
    include Span

    let of_int_ns i = i |> Int63.of_int |> of_int63_ns
  end

  let sexp_of_t = Alternate_sexp.sexp_of_t
end

let show t = print_s [%sexp (t : _ t)]

(* giga-nanosecond *)
let gibi = 2. ** 30.
let gibi_nanos float = float *. gibi |> Time_ns.Span.of_ns

module Num_key_bits = struct
  open! Private.Num_key_bits

  let%test_unit _ = invariant zero
end

module Alarm_precision = struct
  include Alarm_precision

  let sexp_of_t t =
    [%message
      ""
        ~_:(t : t)
        ~_:
          (String.concat
             [ t |> to_span |> Time_ns.Span.to_int63_ns |> Int63.to_string_hum; "ns" ])]
  ;;

  let print t = print_s [%sexp (t : t)]

  let%expect_test "constants" =
    print about_one_day;
    [%expect {|
      (19h32m48.744177664s 70_368_744_177_664ns) |}];
    print about_one_second;
    [%expect {|
      (1.073741824s 1_073_741_824ns) |}];
    print about_one_microsecond;
    [%expect {|
      (1.024us 1_024ns) |}];
    print about_one_millisecond;
    [%expect {|
      (1.048576ms 1_048_576ns) |}];
    print one_nanosecond;
    [%expect {|
     (1ns 1ns) |}]
  ;;

  let%expect_test "[div]" =
    for pow2 = -3 to 3 do
      print (div about_one_second ~pow2)
    done;
    [%expect
      {|
      (8.589934592s 8_589_934_592ns)
      (4.294967296s 4_294_967_296ns)
      (2.147483648s 2_147_483_648ns)
      (1.073741824s 1_073_741_824ns)
      (536.870912ms 536_870_912ns)
      (268.435456ms 268_435_456ns)
      (134.217728ms 134_217_728ns) |}]
  ;;

  let%expect_test "[mul]" =
    for pow2 = -3 to 3 do
      print (mul about_one_second ~pow2)
    done;
    [%expect
      {|
      (134.217728ms 134_217_728ns)
      (268.435456ms 268_435_456ns)
      (536.870912ms 536_870_912ns)
      (1.073741824s 1_073_741_824ns)
      (2.147483648s 2_147_483_648ns)
      (4.294967296s 4_294_967_296ns)
      (8.589934592s 8_589_934_592ns) |}]
  ;;

  let%expect_test "[of_span_floor_pow2_ns]" =
    List.iter
      [ about_one_day
      ; about_one_second
      ; about_one_millisecond
      ; about_one_microsecond
      ; one_nanosecond
      ]
      ~f:(fun t ->
        require [%here] (equal t (t |> to_span |> of_span_floor_pow2_ns));
        if Time_ns.Span.( > ) (t |> to_span) Time_ns.Span.nanosecond
        then
          require
            [%here]
            (equal
               t
               (Time_ns.Span.( + ) (t |> to_span) Time_ns.Span.nanosecond
                |> of_span_floor_pow2_ns)));
    List.iter [ 1.; 1E-3; 1E-6 ] ~f:(fun span ->
      let span = Time_ns.Span.of_sec span in
      print_s
        [%message
          ""
            (span : Time_ns.Span.t)
            ~alarm_precision:(span |> of_span_floor_pow2_ns : t)]);
    [%expect
      {|
      ((span 1s) (alarm_precision (536.870912ms 536_870_912ns)))
      ((span 1ms) (alarm_precision (524.288us 524_288ns)))
      ((span 1us) (alarm_precision (512ns 512ns))) |}]
  ;;
end

let%expect_test "[Config.microsecond_precision]" =
  print_s [%sexp (Config.microsecond_precision () : Config.t)];
  [%expect {|
    ((alarm_precision 1.024us) (level_bits (10 10 6 6 5))) |}];
  print_s
    [%sexp (Config.durations (Config.microsecond_precision ()) : Time_ns.Span.t list)];
  [%expect
    {|
    (1.048576ms
     1.073741824s
     1m8.719476736s
     1h13m18.046511104s
     1d15h5m37.488355328s) |}]
;;

let%expect_test _ =
  print_s [%sexp (Level_bits.max_num_bits : int)];
  [%expect {|
    62 |}]
;;

let%expect_test "invalid level bits" =
  let test level_bits =
    require_does_raise [%here] (fun () -> Level_bits.create_exn level_bits);
    require_does_raise [%here] ~hide_positions:true (fun () ->
      [%of_sexp: Level_bits.t] ([%sexp_of: int list] level_bits))
  in
  test [];
  [%expect
    {|
    (Failure "Level_bits.create_exn requires a nonempty list")
    "Assert_failure timing_wheel.ml:LINE:COL" |}];
  test [ 0 ];
  [%expect
    {|
    ("Level_bits.create_exn got nonpositive num bits" (0))
    "Assert_failure timing_wheel.ml:LINE:COL" |}];
  test [ -1 ];
  [%expect
    {|
    ("Level_bits.create_exn got nonpositive num bits" (-1))
    "Assert_failure timing_wheel.ml:LINE:COL" |}];
  test [ 2; 0; 1 ];
  [%expect
    {|
    ("Level_bits.create_exn got nonpositive num bits" (2 0 1))
    "Assert_failure timing_wheel.ml:LINE:COL" |}];
  test [ Level_bits.max_num_bits + 1 ];
  [%expect
    {|
    ("Level_bits.create_exn got too many bits"
      (63)
      (got          63)
      (max_num_bits 62))
    "Assert_failure timing_wheel.ml:LINE:COL" |}];
  test (List.init (Level_bits.max_num_bits + 1) ~f:Fn.id);
  [%expect
    {|
    ("Level_bits.create_exn got nonpositive num bits"
     (0
      1
      2
      3
      4
      5
      6
      7
      8
      9
      10
      11
      12
      13
      14
      15
      16
      17
      18
      19
      20
      21
      22
      23
      24
      25
      26
      27
      28
      29
      30
      31
      32
      33
      34
      35
      36
      37
      38
      39
      40
      41
      42
      43
      44
      45
      46
      47
      48
      49
      50
      51
      52
      53
      54
      55
      56
      57
      58
      59
      60
      61
      62))
    "Assert_failure timing_wheel.ml:LINE:COL" |}]
;;

let%expect_test _ = Level_bits.invariant Level_bits.default

let%expect_test "[Level_bits.num_bits]" =
  let num_bits bits =
    let level_bits = Level_bits.create_exn bits in
    print_s [%sexp (Level_bits.num_bits level_bits : int)];
    let sexp = [%sexp (level_bits : Level_bits.t)] in
    require_equal
      [%here]
      (module Sexp)
      sexp
      (sexp |> [%of_sexp: Level_bits.t] |> [%sexp_of: Level_bits.t])
  in
  num_bits [ 1 ];
  [%expect {|
    1 |}];
  num_bits [ 1; 1 ];
  [%expect {|
    2 |}];
  num_bits [ 1; 2; 3 ];
  [%expect {|
    6 |}]
;;

let create_config ?extend_to_max_num_bits ?level_bits ~alarm_precision () =
  Config.create
    ()
    ~alarm_precision:(alarm_precision |> Alarm_precision.of_span_floor_pow2_ns)
    ?level_bits:
      (Option.map level_bits ~f:(Level_bits.create_exn ?extend_to_max_num_bits))
;;

let%expect_test "[Config.create] with negative alarm precision" =
  require_does_raise [%here] (fun () ->
    create_config ~alarm_precision:(gibi_nanos (-1.)) ());
  [%expect
    {|
    ("[Alarm_precision.of_span_floor_pow2_ns] got non-positive span"
     (span -1.073741824s)) |}]
;;

let%expect_test "[Config.create] with zero alarm precision" =
  require_does_raise [%here] (fun () ->
    create_config ~alarm_precision:(gibi_nanos 0.) ());
  [%expect
    {|
    ("[Alarm_precision.of_span_floor_pow2_ns] got non-positive span" (span 0s)) |}]
;;

let%expect_test "[Config.create] with one second alarm precision" =
  print_s [%sexp (create_config ~alarm_precision:(gibi_nanos 1.) () : Config.t)];
  [%expect {|
    ((alarm_precision 1.073741824s) (level_bits (11 10 10 1))) |}]
;;

let%expect_test "[Config.durations]" =
  let durations ?extend_to_max_num_bits level_bits =
    print_s
      [%sexp
        (Config.durations
           (create_config
              ?extend_to_max_num_bits
              ~alarm_precision:(gibi_nanos 1.)
              ~level_bits
              ())
         : Time_ns.Span.t list)]
  in
  durations [ 1 ];
  [%expect {|
    (2.147483648s) |}];
  durations [ 2; 1 ];
  [%expect {|
    (4.294967296s 8.589934592s) |}];
  durations (List.init 32 ~f:(const 1));
  [%expect
    {|
    (2.147483648s
     4.294967296s
     8.589934592s
     17.179869184s
     34.359738368s
     1m8.719476736s
     2m17.438953472s
     4m34.877906944s
     9m9.755813888s
     18m19.511627776s
     36m39.023255552s
     1h13m18.046511104s
     2h26m36.093022208s
     4h53m12.186044416s
     9h46m24.372088832s
     19h32m48.744177664s
     1d15h5m37.488355328s
     3d6h11m14.976710656s
     6d12h22m29.953421312s
     13d44m59.906842624s
     26d1h29m59.813685248s
     52d2h59m59.627370496s
     104d5h59m59.254740992s
     208d11h59m58.509481984s
     416d23h59m57.018963968s
     833d23h59m54.037927936s
     1667d23h59m48.075855872s
     3335d23h59m36.151711744s
     6671d23h59m12.303423488s
     13343d23h58m24.606846976s
     26687d23h56m49.213693952s
     53375d23h53m38.427387903s) |}];
  durations [ 10; 10; 10 ] ~extend_to_max_num_bits:true;
  [%expect
    {|
    (18m19.511627776s
     13d44m59.906842624s
     13343d23h58m24.606846976s
     26687d23h56m49.213693952s
     53375d23h53m38.427387903s) |}]
;;

let%expect_test "[level_bits], [config], and [max_allowed_alarm_time]" =
  List.iter
    [ Level_bits.default; Level_bits.create_exn [ 1 ]; Level_bits.create_exn [ 10 ] ]
    ~f:(fun level_bits ->
      List.iter
        Alarm_precision.
          [ one_nanosecond
          ; about_one_microsecond
          ; about_one_millisecond
          ; about_one_second
          ; about_one_day
          ]
        ~f:(fun alarm_precision ->
          let config = Config.create ~alarm_precision ~level_bits () in
          print_s
            [%message
              ""
                (level_bits : Level_bits.t)
                (config : Config.t)
                ~max_allowed_alarm_time:
                  (max_allowed_alarm_time (create ~config ~start:Time_ns.epoch)
                   : Time_ns.t)]));
  [%expect
    {|
    ((level_bits (11 10 10 10 10 10 1))
     (config ((alarm_precision 1ns) (level_bits (11 10 10 10 10 10 1))))
     (max_allowed_alarm_time "2116-02-20 23:53:38.427387903Z"))
    ((level_bits (11 10 10 10 10 10 1))
     (config ((alarm_precision 1.024us) (level_bits (11 10 10 10 10 1))))
     (max_allowed_alarm_time "2116-02-20 23:53:38.427387903Z"))
    ((level_bits (11 10 10 10 10 10 1))
     (config ((alarm_precision 1.048576ms) (level_bits (11 10 10 10 1))))
     (max_allowed_alarm_time "2116-02-20 23:53:38.427387903Z"))
    ((level_bits (11 10 10 10 10 10 1))
     (config ((alarm_precision 1.073741824s) (level_bits (11 10 10 1))))
     (max_allowed_alarm_time "2116-02-20 23:53:38.427387903Z"))
    ((level_bits (11 10 10 10 10 10 1))
     (config ((alarm_precision 19h32m48.744177664s) (level_bits (11 5))))
     (max_allowed_alarm_time "2116-02-20 23:53:38.427387903Z"))
    ((level_bits (1))
     (config ((alarm_precision 1ns) (level_bits (1))))
     (max_allowed_alarm_time "1970-01-01 00:00:00.000000001Z"))
    ((level_bits (1))
     (config ((alarm_precision 1.024us) (level_bits (1))))
     (max_allowed_alarm_time "1970-01-01 00:00:00.000002047Z"))
    ((level_bits (1))
     (config ((alarm_precision 1.048576ms) (level_bits (1))))
     (max_allowed_alarm_time "1970-01-01 00:00:00.002097151Z"))
    ((level_bits (1))
     (config ((alarm_precision 1.073741824s) (level_bits (1))))
     (max_allowed_alarm_time "1970-01-01 00:00:02.147483647Z"))
    ((level_bits (1))
     (config ((alarm_precision 19h32m48.744177664s) (level_bits (1))))
     (max_allowed_alarm_time "1970-01-02 15:05:37.488355327Z"))
    ((level_bits (10))
     (config ((alarm_precision 1ns) (level_bits (10))))
     (max_allowed_alarm_time "1970-01-01 00:00:00.000001023Z"))
    ((level_bits (10))
     (config ((alarm_precision 1.024us) (level_bits (10))))
     (max_allowed_alarm_time "1970-01-01 00:00:00.001048575Z"))
    ((level_bits (10))
     (config ((alarm_precision 1.048576ms) (level_bits (10))))
     (max_allowed_alarm_time "1970-01-01 00:00:01.073741823Z"))
    ((level_bits (10))
     (config ((alarm_precision 1.073741824s) (level_bits (10))))
     (max_allowed_alarm_time "1970-01-01 00:18:19.511627775Z"))
    ((level_bits (10))
     (config ((alarm_precision 19h32m48.744177664s) (level_bits (10))))
     (max_allowed_alarm_time "1972-04-13 23:59:54.037927935Z")) |}]
;;

let%expect_test "[level_bits] and [max_allowed_alarm_time] with \
                 [~extend_to_max_num_bits:true]"
  =
  List.iter [ 1E-9; 1E-6; 1E-3; 1.; 10. ] ~f:(fun s ->
    let alarm_precision = gibi_nanos s in
    let config =
      create_config ~alarm_precision ~extend_to_max_num_bits:true ~level_bits:[ 1 ] ()
    in
    let max_allowed_alarm_time =
      max_allowed_alarm_time (create ~config ~start:Time_ns.epoch)
    in
    print_s
      [%message
        ""
          ~alarm_precision:(Config.alarm_precision config : Time_ns.Span.t)
          ~num_level_bits:(Level_bits.num_bits (Config.level_bits config) : int)
          (max_allowed_alarm_time : Time_ns.t)]);
  [%expect
    {|
    ((alarm_precision 1ns)
     (num_level_bits  62)
     (max_allowed_alarm_time "2116-02-20 23:53:38.427387903Z"))
    ((alarm_precision 1.024us)
     (num_level_bits  52)
     (max_allowed_alarm_time "2116-02-20 23:53:38.427387903Z"))
    ((alarm_precision 1.048576ms)
     (num_level_bits  42)
     (max_allowed_alarm_time "2116-02-20 23:53:38.427387903Z"))
    ((alarm_precision 1.073741824s)
     (num_level_bits  32)
     (max_allowed_alarm_time "2116-02-20 23:53:38.427387903Z"))
    ((alarm_precision 8.589934592s)
     (num_level_bits  29)
     (max_allowed_alarm_time "2116-02-20 23:53:38.427387903Z")) |}]
;;

let create_unit
      ?extend_to_max_num_bits
      ?level_bits
      ?(start = Time_ns.epoch)
      ?(alarm_precision = gibi_nanos 1.)
      ()
  =
  create
    ~config:(create_config ?extend_to_max_num_bits ?level_bits () ~alarm_precision)
    ~start
;;

let%expect_test "[min_allowed_alarm_interval_num], [max_allowed_alarm_interval_num]" =
  let test level_bits =
    let t = create_unit () ~level_bits in
    require_equal
      [%here]
      (module Interval_num)
      (min_allowed_alarm_interval_num t)
      Interval_num.zero;
    print_s
      [%message
        ""
          ~min_allowed_alarm_interval_num:
            (min_allowed_alarm_interval_num t : Interval_num.t)
          ~max_allowed_alarm_interval_num:
            (max_allowed_alarm_interval_num t : Interval_num.t)]
  in
  test [ 1 ];
  [%expect
    {|
      ((min_allowed_alarm_interval_num 0)
       (max_allowed_alarm_interval_num 1)) |}];
  test [ 1; 1 ];
  [%expect
    {|
      ((min_allowed_alarm_interval_num 0)
       (max_allowed_alarm_interval_num 5)) |}];
  test [ 1; 1; 1 ];
  [%expect
    {|
      ((min_allowed_alarm_interval_num 0)
       (max_allowed_alarm_interval_num 11)) |}];
  test [ 2 ];
  [%expect
    {|
      ((min_allowed_alarm_interval_num 0)
       (max_allowed_alarm_interval_num 3)) |}];
  test [ 3 ];
  [%expect
    {|
      ((min_allowed_alarm_interval_num 0)
       (max_allowed_alarm_interval_num 7)) |}];
  test [ 3; 1 ];
  [%expect
    {|
      ((min_allowed_alarm_interval_num 0)
       (max_allowed_alarm_interval_num 23)) |}]
;;

let%expect_test "[is_empty], [interval_num], [length], [mem]" =
  let t = create_unit () ~level_bits:[ 1 ] in
  require [%here] (is_empty t);
  require [%here] (length t = 0);
  let a1 = add_at_interval_num t ~at:Interval_num.zero () in
  let a2 = add_at_interval_num t ~at:Interval_num.zero () in
  let show () =
    print_s
      [%message
        ""
          ~length:(length t : int)
          ~is_empty:(is_empty t : bool)
          ~interval_num1:
            (Or_error.try_with (fun () -> Alarm.interval_num t a1)
             : Interval_num.t Or_error.t)
          ~interval_num2:
            (Or_error.try_with (fun () -> Alarm.interval_num t a2)
             : Interval_num.t Or_error.t)
          ~mem1:(mem t a1 : bool)
          ~mem2:(mem t a2 : bool)]
  in
  show ();
  [%expect
    {|
      ((length   2)
       (is_empty false)
       (interval_num1 (Ok 0))
       (interval_num2 (Ok 0))
       (mem1 true)
       (mem2 true)) |}];
  remove t a1;
  show ();
  [%expect
    {|
      ((length   1)
       (is_empty false)
       (interval_num1 (Error "Timing_wheel got invalid alarm"))
       (interval_num2 (Ok    0))
       (mem1 false)
       (mem2 true)) |}];
  reschedule_at_interval_num t a2 ~at:(Interval_num.of_int 1);
  show ();
  [%expect
    {|
      ((length   1)
       (is_empty false)
       (interval_num1 (Error "Timing_wheel got invalid alarm"))
       (interval_num2 (Ok    1))
       (mem1 false)
       (mem2 true)) |}];
  require_does_raise [%here] (fun () ->
    reschedule_at_interval_num t a1 ~at:(Interval_num.of_int 1));
  [%expect
    {|
      (Failure "Timing_wheel cannot reschedule alarm not in timing wheel") |}];
  remove t a2;
  show ();
  [%expect
    {|
      ((length   0)
       (is_empty true)
       (interval_num1 (Error "Timing_wheel got invalid alarm"))
       (interval_num2 (Error "Timing_wheel got invalid alarm"))
       (mem1 false)
       (mem2 false)) |}]
;;

let advance_clock_to_interval_num t ~to_ ~handle_fired =
  advance_clock t ~to_:(interval_num_start t to_) ~handle_fired
;;

let%expect_test "[add] failures" =
  let t = create_unit () ~level_bits:[ 1 ] in
  let add ~at = ignore (add_at_interval_num t ~at () : _ Alarm.t) in
  for
    interval_num = Interval_num.to_int_exn (min_allowed_alarm_interval_num t)
    to Interval_num.to_int_exn (max_allowed_alarm_interval_num t)
  do
    add ~at:(Interval_num.of_int interval_num)
  done;
  let check_adds_fail () =
    List.iter
      [ Interval_num.min_value
      ; Interval_num.pred (min_allowed_alarm_interval_num t)
      ; Interval_num.succ (max_allowed_alarm_interval_num t)
      ; Interval_num.max_value
      ]
      ~f:(fun at -> require_does_raise [%here] (fun () -> add ~at))
  in
  check_adds_fail ();
  [%expect
    {|
    ("Timing_wheel.interval_num_start got too small interval_num"
     (interval_num     -4_611_686_018_427_387_904)
     (min_interval_num 0))
    ("Timing_wheel.interval_num_start got too small interval_num"
     (interval_num     -1)
     (min_interval_num 0))
    ("Timing_wheel.add_at_interval_num got invalid interval num"
     (interval_num                   2)
     (min_allowed_alarm_interval_num 0)
     (max_allowed_alarm_interval_num 1))
    ("Timing_wheel.interval_num_start got too large interval_num"
     (interval_num       4_611_686_018_427_387_903)
     (t.max_interval_num 4_294_967_295)) |}];
  advance_clock_to_interval_num t ~to_:Interval_num.one ~handle_fired:ignore;
  check_adds_fail ();
  [%expect
    {|
    ("Timing_wheel.interval_num_start got too small interval_num"
     (interval_num     -4_611_686_018_427_387_904)
     (min_interval_num 0))
    ("Timing_wheel.add_at_interval_num got invalid interval num"
     (interval_num                   0)
     (min_allowed_alarm_interval_num 1)
     (max_allowed_alarm_interval_num 2))
    ("Timing_wheel.add_at_interval_num got invalid interval num"
     (interval_num                   3)
     (min_allowed_alarm_interval_num 1)
     (max_allowed_alarm_interval_num 2))
    ("Timing_wheel.interval_num_start got too large interval_num"
     (interval_num       4_611_686_018_427_387_903)
     (t.max_interval_num 4_294_967_295)) |}];
  advance_clock_to_interval_num
    t
    ~to_:(max_allowed_alarm_interval_num t)
    ~handle_fired:ignore;
  check_adds_fail ();
  [%expect
    {|
    ("Timing_wheel.interval_num_start got too small interval_num"
     (interval_num     -4_611_686_018_427_387_904)
     (min_interval_num 0))
    ("Timing_wheel.add_at_interval_num got invalid interval num"
     (interval_num                   1)
     (min_allowed_alarm_interval_num 2)
     (max_allowed_alarm_interval_num 3))
    ("Timing_wheel.add_at_interval_num got invalid interval num"
     (interval_num                   4)
     (min_allowed_alarm_interval_num 2)
     (max_allowed_alarm_interval_num 3))
    ("Timing_wheel.interval_num_start got too large interval_num"
     (interval_num       4_611_686_018_427_387_903)
     (t.max_interval_num 4_294_967_295)) |}];
  advance_clock t ~to_:Private.max_time ~handle_fired:ignore;
  check_adds_fail ();
  [%expect
    {|
    ("Timing_wheel.interval_num_start got too small interval_num"
     (interval_num     -4_611_686_018_427_387_904)
     (min_interval_num 0))
    ("Timing_wheel.add_at_interval_num got invalid interval num"
     (interval_num                   4_294_967_294)
     (min_allowed_alarm_interval_num 4_294_967_295)
     (max_allowed_alarm_interval_num 4_294_967_296))
    ("Timing_wheel.interval_num_start got too large interval_num"
     (interval_num       4_294_967_296)
     (t.max_interval_num 4_294_967_295))
    ("Timing_wheel.interval_num_start got too large interval_num"
     (interval_num       4_611_686_018_427_387_903)
     (t.max_interval_num 4_294_967_295)) |}]
;;

let%expect_test "[clear]" =
  let t = create_unit () ~level_bits:[ 1; 1 ] in
  clear t;
  let _e1 = add_at_interval_num t ~at:Interval_num.zero () in
  let _e2 = add_at_interval_num t ~at:(Interval_num.of_int 2) () in
  show t;
  [%expect
    {|
      ((config ((alarm_precision 1.073741824s) (level_bits (1 1))))
       (start            "1970-01-01 00:00:00Z")
       (max_interval_num 4_294_967_295)
       (now              "1970-01-01 00:00:00Z")
       (alarms (
         ((at "1970-01-01 00:00:00Z")           (value _))
         ((at "1970-01-01 00:00:02.147483648Z") (value _))))) |}];
  clear t;
  show t;
  [%expect
    {|
      ((config ((alarm_precision 1.073741824s) (level_bits (1 1))))
       (start            "1970-01-01 00:00:00Z")
       (max_interval_num 4_294_967_295)
       (now              "1970-01-01 00:00:00Z")
       (alarms ())) |}]
;;

let advance_clock_to_interval_num_return_removed_interval_nums t ~to_ =
  let r = ref [] in
  let handle_fired alarm = r := Alarm.interval_num t alarm :: !r in
  advance_clock_to_interval_num t ~to_ ~handle_fired;
  !r
;;

let%expect_test "[advance_clock] to max interval num" =
  let t = create_unit () ~level_bits:[ 1 ] in
  let add ~at = ignore (add_at_interval_num t ~at () : _ Alarm.t) in
  add ~at:Interval_num.zero;
  add ~at:Interval_num.one;
  require_does_raise [%here] (fun () ->
    advance_clock_to_interval_num t ~to_:Interval_num.max_value ~handle_fired:ignore);
  [%expect
    {|
      ("Timing_wheel.interval_num_start got too large interval_num"
       (interval_num       4_611_686_018_427_387_903)
       (t.max_interval_num 4_294_967_295)) |}];
  let max_interval_num = interval_num t Private.max_time in
  advance_clock_to_interval_num t ~to_:max_interval_num ~handle_fired:ignore;
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (1))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "2116-02-20 23:53:37.35364608Z")
     (alarms ())) |}];
  add ~at:max_interval_num;
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (1))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "2116-02-20 23:53:37.35364608Z")
     (alarms ((
       (at    "2116-02-20 23:53:37.35364608Z")
       (value _))))) |}]
;;

let%expect_test "[advance_clock_to_interval_num]" =
  let num_tests = ref 0 in
  (* [all_sums n] returns all combinations of nonnegative ints that sum to [n]. *)
  let all_sums n =
    let results = Array.create ~len:(n + 1) [] in
    results.(0) <- [ [] ];
    for i = 1 to n do
      results.(i)
      <- List.concat
           (List.init i ~f:(fun j ->
              let first = j + 1 in
              List.map results.(i - first) ~f:(fun rest -> first :: rest)))
    done;
    results.(n)
  in
  let module Initial_min_allowed_interval_num = struct
    type t =
      | Zero
      | Large
    [@@deriving enumerate, sexp_of]
  end
  in
  let test
        ~num_bits
        ~level_bits
        ~(initial_min_allowed_interval_num : Initial_min_allowed_interval_num.t)
        ~step
    =
    incr num_tests;
    let t = create_unit () ~level_bits in
    let max_interval_num = interval_num t Private.max_time in
    let initial_min_allowed_interval_num =
      match initial_min_allowed_interval_num with
      | Zero -> Interval_num.zero
      | Large ->
        Interval_num.sub
          max_interval_num
          (Interval_num.Span.of_int63 (Int63.shift_left Int63.one num_bits))
    in
    try
      advance_clock_to_interval_num
        t
        ~to_:initial_min_allowed_interval_num
        ~handle_fired:ignore;
      require_equal
        [%here]
        (module Interval_num)
        (min_allowed_alarm_interval_num t)
        initial_min_allowed_interval_num;
      require
        [%here]
        (Interval_num.( >= )
           (max_allowed_alarm_interval_num t)
           (Interval_num.add
              (min_allowed_alarm_interval_num t)
              (Interval_num.Span.of_int63
                 (Int63.( - ) (Int63.shift_left Int63.one num_bits) Int63.one))));
      let interval_nums =
        List.init
          (Interval_num.Span.to_int_exn
             (Interval_num.diff
                (max_allowed_alarm_interval_num t)
                (min_allowed_alarm_interval_num t)))
          ~f:(fun i ->
            Interval_num.add
              (min_allowed_alarm_interval_num t)
              (Interval_num.Span.of_int i))
      in
      let n = ref 0 in
      List.iter interval_nums ~f:(fun at ->
        ignore (add_at_interval_num t ~at () : _ Alarm.t);
        incr n;
        require [%here] (length t = !n));
      let removed = ref [] in
      while length t > 0 do
        let interval_nums_removed =
          advance_clock_to_interval_num_return_removed_interval_nums
            t
            ~to_:
              (Interval_num.min
                 max_interval_num
                 (Interval_num.add (min_allowed_alarm_interval_num t) step))
        in
        removed := interval_nums_removed @ !removed;
        List.iter interval_nums_removed ~f:(fun interval_num ->
          require
            [%here]
            (Interval_num.( < ) interval_num (min_allowed_alarm_interval_num t)))
      done;
      let interval_nums_removed = List.sort !removed ~compare:Interval_num.compare in
      require [%here] (Poly.equal interval_nums_removed interval_nums)
    with
    | exn -> failwiths ~here:[%here] "failure" (exn, t) [%sexp_of: exn * _ t]
  in
  let num_bits = 6 in
  let all_sums = all_sums num_bits in
  List.iter
    Initial_min_allowed_interval_num.all
    ~f:(fun initial_min_allowed_interval_num ->
      for step = 1 to 1 lsl num_bits do
        List.iter all_sums ~f:(fun level_bits ->
          test
            ~num_bits
            ~level_bits
            ~initial_min_allowed_interval_num
            ~step:(Interval_num.Span.of_int step))
      done);
  print_s [%message (num_tests : int ref)];
  [%expect {| (num_tests 4_096) |}]
;;

module Interval_num_option = struct
  type t = Interval_num.t option [@@deriving compare, sexp_of]

  let equal = [%compare.equal: t]
end

let%expect_test "[advance_clock]" =
  let t = create_unit () ~level_bits:[ 1; 1; 1; 1 ] in
  require [%here] (is_none (min_alarm_interval_num t));
  let _elt = add_at_interval_num t ~at:Interval_num.zero () in
  require_equal
    [%here]
    (module Interval_num_option)
    (min_alarm_interval_num t)
    (Some Interval_num.zero);
  let max_interval_num = 10 in
  for interval_num = 1 to max_interval_num do
    let at = Interval_num.of_int interval_num in
    require_does_not_raise [%here] (fun () ->
      ignore (add_at_interval_num t ~at () : _ Alarm.t));
    require_equal
      [%here]
      (module Interval_num_option)
      (min_alarm_interval_num t)
      (Some Interval_num.zero)
  done;
  for interval_num = 1 to max_interval_num + 1 do
    let interval_num = Interval_num.of_int interval_num in
    (match
       advance_clock_to_interval_num_return_removed_interval_nums t ~to_:interval_num
     with
     | [ interval_num' ] ->
       require_equal
         [%here]
         (module Interval_num)
         interval_num'
         (Interval_num.pred interval_num)
     | _ -> require [%here] false);
    require_equal
      [%here]
      (module Interval_num_option)
      (min_alarm_interval_num t)
      (if Interval_num.( <= ) interval_num (Interval_num.of_int max_interval_num)
       then Some interval_num
       else None)
  done
;;

let%expect_test "[min_alarm_interval_num]" =
  let t = create_unit () ~level_bits:[ 1; 1; 1; 1 ] in
  let max_interval_num = Interval_num.of_int 10 in
  let elts =
    List.init
      (Interval_num.to_int_exn max_interval_num + 1)
      ~f:(fun interval_num ->
        add_at_interval_num t ~at:(Interval_num.of_int interval_num) ())
  in
  List.iter elts ~f:(fun elt ->
    let interval_num = Alarm.interval_num t elt in
    remove t elt;
    require_equal
      [%here]
      (module Interval_num_option)
      (min_alarm_interval_num t)
      (if Interval_num.( < ) interval_num max_interval_num
       then Some (Interval_num.succ interval_num)
       else None))
;;

let%expect_test "[iter]" =
  let t = create_unit () ~level_bits:[ 1; 1; 1; 1 ] in
  let count () =
    let r = ref 0 in
    iter t ~f:(fun _ -> incr r);
    !r
  in
  let show_count () = print_s [%sexp (count () : int)] in
  show_count ();
  [%expect {|
      0 |}];
  let num_elts = 10 in
  for interval_num = 0 to num_elts - 1 do
    ignore (add_at_interval_num t ~at:(Interval_num.of_int interval_num) () : _ Alarm.t)
  done;
  show_count ();
  [%expect {|
      10 |}];
  advance_clock_to_interval_num t ~to_:Interval_num.one ~handle_fired:ignore;
  show_count ();
  [%expect {|
      9 |}];
  advance_clock_to_interval_num
    t
    ~to_:(Interval_num.of_int num_elts)
    ~handle_fired:ignore;
  show_count ();
  [%expect {|
      0 |}]
;;

let%expect_test "[iter]" =
  let t = create_unit () ~level_bits:[ 1; 1; 1; 1 ] in
  let elts = ref [] in
  for interval_num = 0 to Interval_num.to_int_exn (max_allowed_alarm_interval_num t) do
    elts := add_at_interval_num t ~at:(Interval_num.of_int interval_num) () :: !elts
  done;
  let elts' = ref [] in
  iter t ~f:(fun elt -> elts' := elt :: !elts');
  let sort elts =
    List.sort elts ~compare:(fun elt1 elt2 ->
      Interval_num.compare (Alarm.interval_num t elt1) (Alarm.interval_num t elt2))
  in
  require [%here] (List.equal phys_equal (sort !elts) (sort !elts'))
;;

let%expect_test "start after epoch" =
  let t = create_unit ~start:(Time_ns.add Time_ns.epoch (gibi_nanos 1.)) () in
  invariant ignore t
;;

let%expect_test "invalid alarm precision" =
  let test alarm_precision =
    require_does_raise [%here] (fun () -> create_unit ~alarm_precision ())
  in
  test (gibi_nanos (-1.));
  [%expect
    {|
    ("[Alarm_precision.of_span_floor_pow2_ns] got non-positive span"
     (span -1.073741824s)) |}];
  test (gibi_nanos 0.);
  [%expect
    {|
    ("[Alarm_precision.of_span_floor_pow2_ns] got non-positive span" (span 0s)) |}]
;;

let%expect_test "[Private.interval_num_internal]" =
  for time = -5 to 4 do
    print_s
      [%message
        ""
          (time : int)
          ~interval_num:
            (Interval_num.to_int_exn
               (Private.interval_num_internal
                  ~alarm_precision:
                    (Alarm_precision.of_span_floor_pow2_ns
                       (Time_ns.Span.of_int63_ns (Int63.of_int 4)))
                  ~time:(Time_ns.of_int_ns_since_epoch time))
             : int)]
  done;
  [%expect
    {|
    ((time         -5)
     (interval_num -2))
    ((time         -4)
     (interval_num -1))
    ((time         -3)
     (interval_num -1))
    ((time         -2)
     (interval_num -1))
    ((time         -1)
     (interval_num -1))
    ((time         0)
     (interval_num 0))
    ((time         1)
     (interval_num 0))
    ((time         2)
     (interval_num 0))
    ((time         3)
     (interval_num 0))
    ((time         4)
     (interval_num 1)) |}]
;;

let%expect_test "[interval_num_start], [interval_start]" =
  let t = create_unit () in
  require [%here] (not (mem t (Alarm.null ())));
  let start = start t in
  let test after =
    let time = Time_ns.add start (gibi_nanos after) in
    let interval_num = interval_num t time in
    let interval_num_start = interval_num_start t interval_num in
    let interval_start = interval_start t time in
    print_s
      [%message
        ""
          (interval_num : Interval_num.t)
          (interval_num_start : Time_ns.t)
          (interval_start : Time_ns.t)];
    require [%here] (Time_ns.equal interval_num_start interval_start)
  in
  test 0.;
  [%expect
    {|
    ((interval_num       0)
     (interval_num_start "1970-01-01 00:00:00Z")
     (interval_start     "1970-01-01 00:00:00Z")) |}];
  test 0.1;
  [%expect
    {|
    ((interval_num       0)
     (interval_num_start "1970-01-01 00:00:00Z")
     (interval_start     "1970-01-01 00:00:00Z")) |}];
  test 0.99;
  [%expect
    {|
    ((interval_num       0)
     (interval_num_start "1970-01-01 00:00:00Z")
     (interval_start     "1970-01-01 00:00:00Z")) |}];
  test 1.;
  [%expect
    {|
    ((interval_num       1)
     (interval_num_start "1970-01-01 00:00:01.073741824Z")
     (interval_start     "1970-01-01 00:00:01.073741824Z")) |}];
  test 1.5;
  [%expect
    {|
    ((interval_num       1)
     (interval_num_start "1970-01-01 00:00:01.073741824Z")
     (interval_start     "1970-01-01 00:00:01.073741824Z")) |}];
  test 1.99;
  [%expect
    {|
    ((interval_num       1)
     (interval_num_start "1970-01-01 00:00:01.073741824Z")
     (interval_start     "1970-01-01 00:00:01.073741824Z")) |}];
  test 2.;
  [%expect
    {|
    ((interval_num       2)
     (interval_num_start "1970-01-01 00:00:02.147483648Z")
     (interval_start     "1970-01-01 00:00:02.147483648Z")) |}]
;;

let%expect_test "[advance_clock]" =
  let t = create_unit () in
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (11 10 10 1))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "1970-01-01 00:00:00Z")
     (alarms ())) |}];
  let to_ = Time_ns.add (now t) (gibi_nanos 1.) in
  advance_clock t ~to_ ~handle_fired:ignore;
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (11 10 10 1))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "1970-01-01 00:00:01.073741824Z")
     (alarms ())) |}]
;;

let%expect_test "min alarm at [max_time]" =
  List.iter [ false; true ] ~f:(fun advance_to_max ->
    List.iter [ 1; 2 ] ~f:(fun ns ->
      let alarm_precision = Time_ns.Span.scale_int Time_ns.Span.nanosecond ns in
      let t =
        create_unit
          ~alarm_precision
          ~level_bits:[ 1 ]
          ~extend_to_max_num_bits:true
          ()
      in
      if advance_to_max then advance_clock t ~to_:max_time ~handle_fired:ignore;
      ignore (add t ~at:max_time () : _ Alarm.t);
      print_s [%message "" (advance_to_max : bool) (ns : int)];
      require_equal
        [%here]
        (module Interval_num)
        (min_alarm_interval_num_exn t)
        (interval_num t max_time)));
  [%expect
    {|
    ((advance_to_max false)
     (ns             1))
    ((advance_to_max false)
     (ns             2))
    ((advance_to_max true)
     (ns             1))
    ((advance_to_max true)
     (ns             2)) |}]
;;

let%expect_test "[advance_clock ~to_:max_time]" =
  List.iter [ 1; 2; 4 ] ~f:(fun ns ->
    let alarm_precision = Time_ns.Span.scale_int Time_ns.Span.nanosecond ns in
    for level0_bits = 1 to 3 do
      require_does_not_raise [%here] ~cr:CR_soon (fun () ->
        let t =
          create_unit
            ~alarm_precision
            ~level_bits:[ level0_bits ]
            ~extend_to_max_num_bits:true
            ()
        in
        print_s [%message "" (alarm_precision : Time_ns.Span.t) (level0_bits : int)];
        for i = 10 downto 0 do
          ignore
            (add
               t
               ~at:(Time_ns.sub max_time (Time_ns.Span.of_int_ns i))
               (fun () -> print_s [%message "alarm" (i : int)])
             : _ Alarm.t)
        done;
        for i = 10 downto 0 do
          print_s [%message "advance" (i : int)];
          advance_clock
            t
            ~to_:(Time_ns.sub max_time (Time_ns.Span.of_int_ns i))
            ~handle_fired:(fun a -> Alarm.value t a ())
        done)
    done);
  [%expect
    {|
    ((alarm_precision 1ns)
     (level0_bits     1))
    (advance (i 10))
    (advance (i 9))
    (alarm (i 10))
    (advance (i 8))
    (alarm (i 9))
    (advance (i 7))
    (alarm (i 8))
    (advance (i 6))
    (alarm (i 7))
    (advance (i 5))
    (alarm (i 6))
    (advance (i 4))
    (alarm (i 5))
    (advance (i 3))
    (alarm (i 4))
    (advance (i 2))
    (alarm (i 3))
    (advance (i 1))
    (alarm (i 2))
    (advance (i 0))
    (alarm (i 1))
    ((alarm_precision 1ns)
     (level0_bits     2))
    (advance (i 10))
    (advance (i 9))
    (alarm (i 10))
    (advance (i 8))
    (alarm (i 9))
    (advance (i 7))
    (alarm (i 8))
    (advance (i 6))
    (alarm (i 7))
    (advance (i 5))
    (alarm (i 6))
    (advance (i 4))
    (alarm (i 5))
    (advance (i 3))
    (alarm (i 4))
    (advance (i 2))
    (alarm (i 3))
    (advance (i 1))
    (alarm (i 2))
    (advance (i 0))
    (alarm (i 1))
    ((alarm_precision 1ns)
     (level0_bits     3))
    (advance (i 10))
    (advance (i 9))
    (alarm (i 10))
    (advance (i 8))
    (alarm (i 9))
    (advance (i 7))
    (alarm (i 8))
    (advance (i 6))
    (alarm (i 7))
    (advance (i 5))
    (alarm (i 6))
    (advance (i 4))
    (alarm (i 5))
    (advance (i 3))
    (alarm (i 4))
    (advance (i 2))
    (alarm (i 3))
    (advance (i 1))
    (alarm (i 2))
    (advance (i 0))
    (alarm (i 1))
    ((alarm_precision 2ns)
     (level0_bits     1))
    (advance (i 10))
    (advance (i 9))
    (alarm (i 10))
    (advance (i 8))
    (advance (i 7))
    (alarm (i 9))
    (alarm (i 8))
    (advance (i 6))
    (advance (i 5))
    (alarm (i 7))
    (alarm (i 6))
    (advance (i 4))
    (advance (i 3))
    (alarm (i 5))
    (alarm (i 4))
    (advance (i 2))
    (advance (i 1))
    (alarm (i 3))
    (alarm (i 2))
    (advance (i 0))
    ((alarm_precision 2ns)
     (level0_bits     2))
    (advance (i 10))
    (advance (i 9))
    (alarm (i 10))
    (advance (i 8))
    (advance (i 7))
    (alarm (i 9))
    (alarm (i 8))
    (advance (i 6))
    (advance (i 5))
    (alarm (i 7))
    (alarm (i 6))
    (advance (i 4))
    (advance (i 3))
    (alarm (i 5))
    (alarm (i 4))
    (advance (i 2))
    (advance (i 1))
    (alarm (i 3))
    (alarm (i 2))
    (advance (i 0))
    ((alarm_precision 2ns)
     (level0_bits     3))
    (advance (i 10))
    (advance (i 9))
    (alarm (i 10))
    (advance (i 8))
    (advance (i 7))
    (alarm (i 9))
    (alarm (i 8))
    (advance (i 6))
    (advance (i 5))
    (alarm (i 7))
    (alarm (i 6))
    (advance (i 4))
    (advance (i 3))
    (alarm (i 5))
    (alarm (i 4))
    (advance (i 2))
    (advance (i 1))
    (alarm (i 3))
    (alarm (i 2))
    (advance (i 0))
    ((alarm_precision 4ns)
     (level0_bits     1))
    (advance (i 10))
    (advance (i 9))
    (advance (i 8))
    (advance (i 7))
    (alarm (i 10))
    (alarm (i 9))
    (alarm (i 8))
    (advance (i 6))
    (advance (i 5))
    (advance (i 4))
    (advance (i 3))
    (alarm (i 7))
    (alarm (i 6))
    (alarm (i 5))
    (alarm (i 4))
    (advance (i 2))
    (advance (i 1))
    (advance (i 0))
    ((alarm_precision 4ns)
     (level0_bits     2))
    (advance (i 10))
    (advance (i 9))
    (advance (i 8))
    (advance (i 7))
    (alarm (i 10))
    (alarm (i 9))
    (alarm (i 8))
    (advance (i 6))
    (advance (i 5))
    (advance (i 4))
    (advance (i 3))
    (alarm (i 7))
    (alarm (i 6))
    (alarm (i 5))
    (alarm (i 4))
    (advance (i 2))
    (advance (i 1))
    (advance (i 0))
    ((alarm_precision 4ns)
     (level0_bits     3))
    (advance (i 10))
    (advance (i 9))
    (advance (i 8))
    (advance (i 7))
    (alarm (i 10))
    (alarm (i 9))
    (alarm (i 8))
    (advance (i 6))
    (advance (i 5))
    (advance (i 4))
    (advance (i 3))
    (alarm (i 7))
    (alarm (i 6))
    (alarm (i 5))
    (alarm (i 4))
    (advance (i 2))
    (advance (i 1))
    (advance (i 0)) |}]
;;

let%expect_test "[is_empty], [length]" =
  let t = create_unit () in
  let show () =
    print_s [%message "" ~is_empty:(is_empty t : bool) ~length:(length t : int)]
  in
  show ();
  [%expect {|
    ((is_empty true)
     (length   0)) |}];
  let alarm = add t ~at:(now t) () in
  show ();
  [%expect {|
    ((is_empty false)
     (length   1)) |}];
  remove t alarm;
  show ();
  [%expect {|
    ((is_empty true)
     (length   0)) |}]
;;

let%expect_test "[iter]" =
  let t = create_unit () in
  iter t ~f:(fun _ -> require [%here] false);
  let alarm1 = add t ~at:(now t) () in
  iter t ~f:(fun alarm -> require [%here] (phys_equal alarm alarm1));
  let alarm2 = add t ~at:(now t) () in
  let r = ref 0 in
  iter t ~f:(fun alarm ->
    require [%here] (phys_equal alarm alarm1 || phys_equal alarm alarm2);
    incr r);
  print_s [%message (r : int ref)];
  [%expect {|
    (r 2) |}];
  remove t alarm1;
  remove t alarm2;
  iter t ~f:(fun _ -> require [%here] false)
;;

let%expect_test "access to a removed alarm doesn't segfault" =
  let t =
    create
      ~config:(create_config ~alarm_precision:(gibi_nanos 1.) ())
      ~start:Time_ns.epoch
  in
  let alarm = add t ~at:(Time_ns.add (now t) (gibi_nanos 5.)) (ref 1) in
  let show_mem () = print_s [%sexp (mem t alarm : bool)] in
  show_mem ();
  [%expect {|
    true |}];
  remove t alarm;
  show_mem ();
  [%expect {|
    false |}];
  require_does_raise [%here] (fun _ -> Alarm.interval_num t alarm);
  [%expect {|
    "Timing_wheel got invalid alarm" |}];
  require_does_raise [%here] (fun _ -> Alarm.at t alarm);
  [%expect {|
    "Timing_wheel got invalid alarm" |}];
  require_does_raise [%here] (fun _ -> Alarm.value t alarm);
  [%expect {|
    "Timing_wheel got invalid alarm" |}]
;;

(* Check that [reschedule] and [reschedule_at_interval_num] leave an alarm in the timing
   wheel but reschedule its scheduled time. *)
let test_reschedule reschedule =
  let epoch_plus n_seconds = Time_ns.add Time_ns.epoch (gibi_nanos n_seconds) in
  let t =
    create
      ~config:(create_config ~alarm_precision:(gibi_nanos 1.) ~level_bits:[ 10 ] ())
      ~start:(epoch_plus 0.)
  in
  (* add alarm1 before alarm2, test initial conditions *)
  let alarm1 = add t ~at:(epoch_plus 5.) () in
  let alarm2 = add t ~at:(epoch_plus 10.) () in
  let show () =
    let alarm_at alarm = if mem t alarm then Some (Alarm.at t alarm) else None in
    print_s
      [%message
        ""
          ~now:(now t : Time_ns.t)
          ~next_alarm_fires_at:(next_alarm_fires_at t : Time_ns.t option)
          ~alarm1_at:(alarm_at alarm1 : Time_ns.t option)
          ~alarm2_at:(alarm_at alarm2 : Time_ns.t option)]
  in
  show ();
  print_endline "Reschedule alarm1 after alarm2; alarm2 becomes next.";
  reschedule t alarm1 ~at:(epoch_plus 15.);
  show ();
  print_endline "Advance time past alarm1's original time; nothing fires.";
  advance_clock t ~to_:(epoch_plus 7.) ~handle_fired:(fun _ -> require [%here] false);
  show ();
  print_endline "Reschedule alarm1 before alarm2 again; alarm1 becomes next.";
  reschedule t alarm1 ~at:(epoch_plus 8.);
  show ();
  print_endline "Advance time past alarm1, alarm1 fires but alarm2 does not.";
  advance_clock t ~to_:(epoch_plus 9.) ~handle_fired:ignore;
  show ();
  print_endline "Cannot reschedule the already-fired alarm1.";
  require_does_raise [%here] (fun _ -> reschedule t alarm1 ~at:(epoch_plus 20.));
  show ();
  print_endline "Cannot reschedule before current time.";
  require_does_raise [%here] (fun _ -> reschedule t alarm2 ~at:(epoch_plus 8.));
  show ();
  print_endline "Cannot reschedule arbitrarily far in the future.";
  require_does_raise [%here] (fun _ ->
    reschedule t alarm2 ~at:(Time_ns.add (max_allowed_alarm_time t) (gibi_nanos 1.)));
  print_endline "Fire alarm2.";
  advance_clock t ~to_:(epoch_plus 11.) ~handle_fired:ignore;
  show ()
;;

let%expect_test "[reschedule]" =
  test_reschedule (fun t alarm ~at -> reschedule t alarm ~at);
  [%expect
    {|
((now "1970-01-01 00:00:00Z")
 (next_alarm_fires_at ("1970-01-01 00:00:06.442450944Z"))
 (alarm1_at ("1970-01-01 00:00:05.36870912Z"))
 (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
Reschedule alarm1 after alarm2; alarm2 becomes next.
((now "1970-01-01 00:00:00Z")
 (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
 (alarm1_at ("1970-01-01 00:00:16.10612736Z"))
 (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
Advance time past alarm1's original time; nothing fires.
((now "1970-01-01 00:00:07.516192768Z")
 (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
 (alarm1_at ("1970-01-01 00:00:16.10612736Z"))
 (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
Reschedule alarm1 before alarm2 again; alarm1 becomes next.
((now "1970-01-01 00:00:07.516192768Z")
 (next_alarm_fires_at ("1970-01-01 00:00:09.663676416Z"))
 (alarm1_at ("1970-01-01 00:00:08.589934592Z"))
 (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
Advance time past alarm1, alarm1 fires but alarm2 does not.
((now "1970-01-01 00:00:09.663676416Z")
 (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
 (alarm1_at ())
 (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
Cannot reschedule the already-fired alarm1.
(Failure "Timing_wheel cannot reschedule alarm not in timing wheel")
((now "1970-01-01 00:00:09.663676416Z")
 (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
 (alarm1_at ())
 (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
Cannot reschedule before current time.
("Timing_wheel cannot schedule alarm before start of current interval"
 (at "1970-01-01 00:00:08.589934592Z")
 (now_interval_num_start "1970-01-01 00:00:09.663676416Z"))
((now "1970-01-01 00:00:09.663676416Z")
 (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
 (alarm1_at ())
 (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
Cannot reschedule arbitrarily far in the future.
("Timing_wheel cannot schedule alarm that far in the future"
 (at "1970-01-01 00:18:30.249046015Z")
 (max_allowed_alarm_time "1970-01-01 00:18:29.175304191Z"))
Fire alarm2.
((now "1970-01-01 00:00:11.811160064Z")
 (next_alarm_fires_at ())
 (alarm1_at           ())
 (alarm2_at           ())) |}]
;;

let%expect_test "[reschedule_at_interval_num]" =
  test_reschedule (fun t alarm ~at ->
    reschedule_at_interval_num t alarm ~at:(interval_num t at));
  [%expect
    {|
((now "1970-01-01 00:00:00Z")
 (next_alarm_fires_at ("1970-01-01 00:00:06.442450944Z"))
 (alarm1_at ("1970-01-01 00:00:05.36870912Z"))
 (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
Reschedule alarm1 after alarm2; alarm2 becomes next.
((now "1970-01-01 00:00:00Z")
 (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
 (alarm1_at ("1970-01-01 00:00:16.10612736Z"))
 (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
Advance time past alarm1's original time; nothing fires.
((now "1970-01-01 00:00:07.516192768Z")
 (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
 (alarm1_at ("1970-01-01 00:00:16.10612736Z"))
 (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
Reschedule alarm1 before alarm2 again; alarm1 becomes next.
((now "1970-01-01 00:00:07.516192768Z")
 (next_alarm_fires_at ("1970-01-01 00:00:09.663676416Z"))
 (alarm1_at ("1970-01-01 00:00:08.589934592Z"))
 (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
Advance time past alarm1, alarm1 fires but alarm2 does not.
((now "1970-01-01 00:00:09.663676416Z")
 (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
 (alarm1_at ())
 (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
Cannot reschedule the already-fired alarm1.
(Failure "Timing_wheel cannot reschedule alarm not in timing wheel")
((now "1970-01-01 00:00:09.663676416Z")
 (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
 (alarm1_at ())
 (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
Cannot reschedule before current time.
("Timing_wheel cannot schedule alarm before start of current interval"
 (at "1970-01-01 00:00:08.589934592Z")
 (now_interval_num_start "1970-01-01 00:00:09.663676416Z"))
((now "1970-01-01 00:00:09.663676416Z")
 (next_alarm_fires_at ("1970-01-01 00:00:11.811160064Z"))
 (alarm1_at ())
 (alarm2_at ("1970-01-01 00:00:10.73741824Z")))
Cannot reschedule arbitrarily far in the future.
("Timing_wheel cannot schedule alarm that far in the future"
 (at "1970-01-01 00:18:29.175304192Z")
 (max_allowed_alarm_time "1970-01-01 00:18:29.175304191Z"))
Fire alarm2.
((now "1970-01-01 00:00:11.811160064Z")
 (next_alarm_fires_at ())
 (alarm1_at           ())
 (alarm2_at           ())) |}]
;;

let%expect_test "[advance_clock] fires alarms at the right time" =
  let test ~add ~num_alarms ~alarm_precision ~alarm_separation ~advance_by =
    let t = create ~config:(create_config ~alarm_precision ()) ~start:Time_ns.epoch in
    for i = 1 to num_alarms do
      let at =
        Time_ns.add (now t) (Time_ns.Span.scale alarm_separation (Float.of_int i))
      in
      ignore
        (add t ~at (fun () -> require [%here] (Time_ns.( <= ) at (now t))) : _ Alarm.t)
    done;
    while not (is_empty t) do
      let to_ = Time_ns.add (now t) advance_by in
      advance_clock t ~to_ ~handle_fired:(fun alarm -> Alarm.value t alarm ());
      require_equal
        [%here]
        (module Interval_num)
        (now_interval_num t)
        (interval_num t to_)
    done
  in
  List.iter
    [ add; (fun t ~at a -> add_at_interval_num t ~at:(interval_num t at) a) ]
    ~f:(fun add ->
      List.iter [ 100 ] ~f:(fun num_alarms ->
        List.iter [ 1.; 0.5; 0.1 ] ~f:(fun s ->
          let alarm_precision = gibi_nanos s in
          List.iter [ 0.01; 0.1; 0.5; 1.; 2.; 10. ] ~f:(fun s ->
            let alarm_separation = gibi_nanos s in
            List.iter [ 0.1; 0.5; 1.; 2.; 10. ] ~f:(fun s ->
              let advance_by = gibi_nanos s in
              test
                ~add
                ~num_alarms
                ~alarm_precision
                ~alarm_separation
                ~advance_by)))))
;;

let%expect_test "[add] and [advance_clock]" =
  let t =
    create
      ~config:(create_config ~alarm_precision:(gibi_nanos 1.) ~level_bits:[ 10 ] ())
      ~start:Time_ns.epoch
  in
  let add ~after f = ignore (add t ~at:(Time_ns.add (now t) after) f : _ Alarm.t) in
  let advance_clock by =
    advance_clock
      t
      ~to_:(Time_ns.add (now t) by)
      ~handle_fired:(fun alarm -> Alarm.value t alarm ())
  in
  require_does_raise [%here] (fun () -> add ~after:(gibi_nanos (-1.)) ignore);
  [%expect
    {|
    ("Timing_wheel cannot schedule alarm before start of current interval"
     (at "1969-12-31 23:59:58.926258176Z")
     (now_interval_num_start "1970-01-01 00:00:00Z")) |}];
  require_equal
    [%here]
    (module Time_ns)
    (Time_ns.add (max_allowed_alarm_time t) Time_ns.Span.nanosecond)
    (Time_ns.add (now t) (gibi_nanos 1024.));
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (10))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "1970-01-01 00:00:00Z")
     (alarms ())) |}];
  add ~after:(gibi_nanos 30.) ignore;
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (10))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "1970-01-01 00:00:00Z")
     (alarms ((
       (at    "1970-01-01 00:00:32.21225472Z")
       (value _))))) |}];
  advance_clock (gibi_nanos 30.);
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (10))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "1970-01-01 00:00:32.21225472Z")
     (alarms ((
       (at    "1970-01-01 00:00:32.21225472Z")
       (value _))))) |}];
  advance_clock (gibi_nanos 1.);
  show t;
  [%expect
    {|
    ((config ((alarm_precision 1.073741824s) (level_bits (10))))
     (start            "1970-01-01 00:00:00Z")
     (max_interval_num 4_294_967_295)
     (now              "1970-01-01 00:00:33.285996544Z")
     (alarms ())) |}]
;;

let%expect_test "[next_alarm_fires_at]" =
  let t = create_unit ~level_bits:[ 10 ] () in
  let next_alarm_fires_after () =
    print_s
      [%message
        ""
          ~next_alarm_fires_after:
            (Option.map (next_alarm_fires_at t) ~f:(fun time ->
               Time_ns.diff time Time_ns.epoch)
             : Time_ns.Span.t option)]
  in
  let add_at at =
    ignore (add t ~at:(Time_ns.add Time_ns.epoch at) () : _ Alarm.t);
    next_alarm_fires_after ()
  in
  let advance_clock span =
    advance_clock t ~to_:(Time_ns.add Time_ns.epoch span) ~handle_fired:ignore;
    next_alarm_fires_after ()
  in
  add_at (gibi_nanos 2.);
  [%expect {|
    (next_alarm_fires_after (3.221225472s)) |}];
  add_at (gibi_nanos 1.5);
  [%expect {|
    (next_alarm_fires_after (2.147483648s)) |}];
  add_at (gibi_nanos 1.0);
  [%expect {|
    (next_alarm_fires_after (2.147483648s)) |}];
  add_at (gibi_nanos 0.5);
  [%expect {|
    (next_alarm_fires_after (1.073741824s)) |}];
  add_at (gibi_nanos 0.1);
  [%expect {|
    (next_alarm_fires_after (1.073741824s)) |}];
  advance_clock (gibi_nanos 0.5);
  [%expect {|
    (next_alarm_fires_after (1.073741824s)) |}];
  advance_clock (gibi_nanos 1.);
  [%expect {|
    (next_alarm_fires_after (2.147483648s)) |}];
  advance_clock (gibi_nanos 1.5);
  [%expect {|
    (next_alarm_fires_after (2.147483648s)) |}];
  advance_clock (gibi_nanos 2.);
  [%expect {|
    (next_alarm_fires_after (3.221225472s)) |}];
  advance_clock (gibi_nanos 3.);
  [%expect {|
    (next_alarm_fires_after ()) |}]
;;

let%expect_test "[next_alarm_fires_at] with an alarm at [max_time]" =
  let t = create_unit ~level_bits:[ 1 ] ~extend_to_max_num_bits:true () in
  ignore (add t ~at:max_time () : _ Alarm.t);
  print_s [%sexp (next_alarm_fires_at t : Time_ns.t option)];
  [%expect {| () |}];
  require_does_raise [%here] (fun () -> next_alarm_fires_at_exn t);
  [%expect
    {|
    ("Timing_wheel.next_alarm_fires_at_exn with all alarms in max interval"
     (timing_wheel (
       (config (
         (alarm_precision 1.073741824s)
         (level_bits (
           1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))))
       (start            "1970-01-01 00:00:00Z")
       (max_interval_num 4_294_967_295)
       (now              "1970-01-01 00:00:00Z")
       (alarms ((
         (at    "2116-02-20 23:53:38.427387903Z")
         (value _))))))) |}]
;;

let%expect_test "[fire_past_alarms] - all possible subsets of alarms in the first \
                 bucket that fire"
  =
  let start = Time_ns.epoch in
  let at sec = Time_ns.add start (gibi_nanos sec) in
  let at1 = at 1. in
  let at2 = at 2. in
  let num_tests = ref 0 in
  for num_elts = 0 to 5 do
    let rec loop i ats =
      incr num_tests;
      if i > 0
      then (
        loop (i - 1) (at1 :: ats);
        loop (i - 1) (at2 :: ats))
      else (
        let t =
          create ~start ~config:(create_config ~alarm_precision:(gibi_nanos 60.) ())
        in
        let num_fired = ref 0 in
        List.iter ats ~f:(fun at ->
          let alarm = add t ~at () in
          require_equal
            [%here]
            (module Interval_num)
            (Alarm.interval_num t alarm)
            Interval_num.zero);
        advance_clock t ~to_:at1 ~handle_fired:(fun _ -> require [%here] false);
        fire_past_alarms t ~handle_fired:(fun alarm ->
          if Time_ns.equal (Alarm.at t alarm) at1
          then incr num_fired
          else require [%here] false);
        require_equal
          [%here]
          (module Int)
          !num_fired
          (List.count ats ~f:(Time_ns.equal at1)))
    in
    loop num_elts []
  done;
  print_s [%message (num_tests : int ref)];
  [%expect {|
    (num_tests 120) |}]
;;

let%expect_test "alarm buckets" =
  let start = Time_ns.epoch in
  let t : bool ref t =
    create ~config:(create_config ~alarm_precision:(gibi_nanos 1.) ()) ~start
  in
  let handle_fired (a : bool ref Alarm.t) : unit =
    let r = Alarm.value t a in
    require [%here] (not !r);
    r := true
  in
  let precision = alarm_precision t in
  let precision_0_2 = Time_ns.Span.scale precision 0.2 in
  let _ = add t ~at:(Time_ns.add start precision) (ref false) in
  let base = next_alarm_fires_at t |> Option.value_exn in
  let step0 = Time_ns.add base precision_0_2 in
  let step1 = Time_ns.add step0 precision_0_2 in
  let step2 = Time_ns.add step1 precision_0_2 in
  let step3 = Time_ns.add step2 precision in
  (* Check all alarm will be in the same bucket but step3 *)
  let interval_num0 = interval_num t step0 in
  let interval_num1 = interval_num t step1 in
  let interval_num2 = interval_num t step2 in
  let interval_num3 = interval_num t step3 in
  print_s
    [%message
      ""
        (interval_num0 : Interval_num.t)
        (interval_num1 : Interval_num.t)
        (interval_num2 : Interval_num.t)
        (interval_num3 : Interval_num.t)];
  [%expect
    {|
    ((interval_num0 2)
     (interval_num1 2)
     (interval_num2 2)
     (interval_num3 3)) |}];
  let step1_fired = ref false in
  let step2_fired = ref false in
  let step3_fired = ref false in
  let _ = add t ~at:step1 step1_fired in
  let _ = add t ~at:step2 step2_fired in
  let _ = add t ~at:step3 step3_fired in
  let show () =
    print_s
      [%message
        "" (step1_fired : bool ref) (step2_fired : bool ref) (step3_fired : bool ref)]
  in
  (* Nothing should be triggered before *)
  advance_clock t ~to_:step0 ~handle_fired;
  fire_past_alarms t ~handle_fired;
  show ();
  [%expect
    {|
    ((step1_fired false)
     (step2_fired false)
     (step3_fired false)) |}];
  advance_clock t ~to_:step1 ~handle_fired;
  show ();
  [%expect
    {|
    ((step1_fired false)
     (step2_fired false)
     (step3_fired false)) |}];
  show ();
  [%expect
    {|
    ((step1_fired false)
     (step2_fired false)
     (step3_fired false)) |}];
  fire_past_alarms t ~handle_fired;
  show ();
  [%expect
    {|
    ((step1_fired true)
     (step2_fired false)
     (step3_fired false)) |}];
  advance_clock t ~to_:step2 ~handle_fired;
  show ();
  [%expect
    {|
    ((step1_fired true)
     (step2_fired false)
     (step3_fired false)) |}];
  fire_past_alarms t ~handle_fired;
  show ();
  [%expect
    {|
    ((step1_fired true)
     (step2_fired true)
     (step3_fired false)) |}]
;;

let%expect_test "[max_alarm_time_in_min_interval]" =
  let t = create_unit () ~level_bits:[ 1; 1 ] in
  let max_alarm_time_in_min_interval () =
    print_s [%sexp (max_alarm_time_in_min_interval t : Time_ns.t option)]
  in
  max_alarm_time_in_min_interval ();
  [%expect {|
    () |}];
  let add_after span = add t ~at:(Time_ns.add (now t) span) ignore in
  let a = add_after (gibi_nanos 0.5) in
  max_alarm_time_in_min_interval ();
  [%expect {|
    ("1970-01-01 00:00:00.536870912Z") |}];
  remove t a;
  max_alarm_time_in_min_interval ();
  [%expect {|
    () |}];
  (* Add two alarms that appear in different intervals, but in the same slot on the
     second level of the timing wheel.  So the correct [max_alarm_time_in_min_interval] is
     2.1, not 3.9. *)
  let _ = add_after (gibi_nanos 2.1) in
  let _ = add_after (gibi_nanos 3.9) in
  max_alarm_time_in_min_interval ();
  [%expect {|
    ("1970-01-01 00:00:02.25485783Z") |}]
;;

let%expect_test "multiple alarms at the same time are fired in insertion order" =
  let t = create_unit () in
  let delta = Time_ns.Span.of_sec 1. in
  let at = Time_ns.(add epoch delta) in
  for i = 0 to 5 do
    ignore (add t ~at i)
  done;
  advance_clock t ~to_:(Time_ns.add at delta) ~handle_fired:(fun alarm ->
    print_s [%sexp (Alarm.value t alarm : int)]);
  [%expect {|
    0
    1
    2
    3
    4
    5 |}]
;;

let%expect_test "max_alarm_time can not be exceeded by [add] or [add_at_interval_num]" =
  let t = create_unit ~level_bits:[ 10 ] () in
  let succ t =
    let r = Time_ns.add t Time_ns.Span.nanosecond in
    assert (Time_ns.( > ) r t);
    r
  in
  let bad_time = succ (max_allowed_alarm_time t) in
  require_does_raise [%here] (fun () -> add t ~at:bad_time ());
  [%expect
    {|
    ("Timing_wheel cannot schedule alarm that far in the future"
     (at "1970-01-01 00:18:19.511627776Z")
     (max_allowed_alarm_time "1970-01-01 00:18:19.511627775Z")) |}];
  require_does_raise [%here] (fun () ->
    add_at_interval_num
      t
      ~at:(Interval_num.succ (interval_num t (max_allowed_alarm_time t)))
      ());
  [%expect
    {|
    ("Timing_wheel.add_at_interval_num got invalid interval num"
     (interval_num                   1_024)
     (min_allowed_alarm_interval_num 0)
     (max_allowed_alarm_interval_num 1_023)) |}]
;;
