open! Core_kernel
open! Expect_test_helpers_core
open! Bus

let does_raise = Exn.does_raise

include (
struct
  module Callback_arity = Callback_arity

  type nonrec ('a, 'b) t = ('a, 'b) t
  type ('a, 'b) bus = ('a, 'b) t

  module Read_only = struct
    type 'a t = 'a Read_only.t [@@deriving sexp_of]

    let invariant = Read_only.invariant
  end

  module Read_write = struct
    type 'a t = 'a Read_write.t [@@deriving sexp_of]

    let invariant = Read_write.invariant
  end

  module On_subscription_after_first_write = On_subscription_after_first_write
  module Subscriber = Subscriber
  module Fold_arity = Fold_arity

  let callback_arity = callback_arity
  let close = close
  let create = create
  let fold_exn = fold_exn
  let is_closed = is_closed
  let iter_exn = iter_exn
  let num_subscribers = num_subscribers
  let read_only = read_only
  let subscribe_exn = subscribe_exn
  let unsubscribe = unsubscribe
  let write = write
  let write2 = write2
  let write3 = write3
  let write4 = write4
  let write5 = write5

  let create1 created_from ~on_subscription_after_first_write =
    create
      created_from
      Arity1
      ~on_subscription_after_first_write
      ~on_callback_raise:Error.raise
  ;;

  let create2 created_from ~on_subscription_after_first_write =
    create
      created_from
      Arity2
      ~on_subscription_after_first_write
      ~on_callback_raise:Error.raise
  ;;

  let sexp_of_t = sexp_of_t

  module Arity_1 (W : sig
      val write : ('a -> unit) Read_write.t -> 'a -> unit
    end) =
  struct
    let write = W.write

    let%expect_test "sexp_of_t" =
      let bus = create1 [%here] ~on_subscription_after_first_write:Raise in
      ignore (subscribe_exn bus [%here] ~f:ignore : _ Subscriber.t);
      ignore (bus |> [%sexp_of: (_, _) t] : Sexp.t)
    ;;

    let%expect_test "write to empty bus" =
      let bus = create1 [%here] ~on_subscription_after_first_write:Allow in
      write bus ()
    ;;

    let%expect_test "can subscribe and unsubscribe" =
      let call_count = ref 0 in
      let callback _v = incr call_count in
      let bus = create1 [%here] ~on_subscription_after_first_write:Allow in
      let print_bus () =
        print_s
          [%message
            ""
              ~num_subscribers:(num_subscribers bus : int)
              ~is_closed:(is_closed bus : bool)
              (call_count : int ref)]
      in
      print_bus ();
      [%expect
        {|
        ((num_subscribers 0)
         (is_closed       false)
         (call_count      0)) |}];
      let subscription = subscribe_exn bus [%here] ~f:callback in
      print_bus ();
      [%expect
        {|
        ((num_subscribers 1)
         (is_closed       false)
         (call_count      0)) |}];
      write bus ();
      unsubscribe bus subscription;
      print_bus ();
      [%expect
        {|
        ((num_subscribers 0)
         (is_closed       false)
         (call_count      1)) |}];
      write bus ();
      print_bus ();
      [%expect
        {|
        ((num_subscribers 0)
         (is_closed       false)
         (call_count      1)) |}]
    ;;

    let%expect_test "subscriber raise" =
      let bus = create1 [%here] ~on_subscription_after_first_write:Raise in
      ignore
        (subscribe_exn bus [%here] ~f:(fun _ ->
           raise_s [%message "subscriber raising"])
         : _ Subscriber.t);
      show_raise ~hide_positions:true (fun () -> write bus ());
      [%expect
        {|
        (raised (
          "Bus subscriber raised"
          (exn "subscriber raising")
          (backtrace ("<backtrace elided in test>"))
          (subscriber (
            Bus.Subscriber.t (subscribed_from lib/bus/test/test_bus.ml:LINE:COL))))) |}]
    ;;

    let%expect_test "~on_subscription_after_first_write:Raise" =
      let callback _ = () in
      let bus = create1 [%here] ~on_subscription_after_first_write:Raise in
      write bus ();
      require [%here] (does_raise (fun () -> subscribe_exn bus [%here] ~f:callback))
    ;;

    let%expect_test "~on_subscription_after_first_write:Allow" =
      let call_count = ref 0 in
      let callback _ = incr call_count in
      let bus = create1 [%here] ~on_subscription_after_first_write:Allow in
      write bus ();
      ignore (subscribe_exn bus [%here] ~f:callback : _ Subscriber.t);
      print_s [%message (call_count : int ref)];
      [%expect {|
        (call_count 0) |}];
      write bus ();
      print_s [%message (call_count : int ref)];
      [%expect {|
        (call_count 1) |}]
    ;;

    let%expect_test "on_close is called" =
      let callback _ = () in
      let bus = create1 [%here] ~on_subscription_after_first_write:Allow in
      let on_close () = print_string "Closed" in
      ignore (subscribe_exn bus [%here] ~on_close ~f:callback : _ Subscriber.t);
      Bus.close bus;
      [%expect {|
        Closed |}]
    ;;

    let%expect_test "on_close is not called if you unsubscribe" =
      let callback _ = () in
      let bus = create1 [%here] ~on_subscription_after_first_write:Allow in
      let on_close () = print_string "Closed" in
      Bus.unsubscribe bus (subscribe_exn bus [%here] ~on_close ~f:callback);
      Bus.close bus;
      [%expect {| |}]
    ;;

    let%expect_test "~on_subscription_after_first_write:Allow_and_send_last_value" =
      let bus =
        create
          [%here]
          ~on_subscription_after_first_write:Allow_and_send_last_value
          Arity1
          ~on_callback_raise:Error.raise
      in
      let subscribe n =
        ignore
          (subscribe_exn bus [%here] ~f:(fun s ->
             printf "Subscriber %d, value received: %s\n" n s)
           : _ Subscriber.t)
      in
      subscribe 1;
      [%expect {| |}];
      write bus "orange";
      [%expect {|
        Subscriber 1, value received: orange |}];
      subscribe 2;
      [%expect {|
        Subscriber 2, value received: orange |}];
      write bus "banana";
      [%expect
        {|
        Subscriber 1, value received: banana
        Subscriber 2, value received: banana |}];
      subscribe 3;
      [%expect {|
        Subscriber 3, value received: banana |}]
    ;;

    let%expect_test "unsubscribe is idempotent" =
      let bus = create1 [%here] ~on_subscription_after_first_write:Raise in
      let subscriber = Bus.subscribe_exn bus [%here] ~f:ignore in
      unsubscribe bus subscriber;
      unsubscribe bus subscriber
    ;;

    let%expect_test "mid-callback unsubscribe takes effect for the next write" =
      let bus = create1 [%here] ~on_subscription_after_first_write:Raise in
      let subscriber2 = ref None in
      let call_count1 = ref 0 in
      let callback1 _ =
        incr call_count1;
        unsubscribe bus (Option.value_exn !subscriber2)
      in
      let subscriber1 = subscribe_exn bus [%here] ~f:callback1 in
      let call_count2 = ref 0 in
      let callback2 _ =
        incr call_count2;
        unsubscribe bus subscriber1
      in
      let print_call_counts () =
        print_s [%message (call_count1 : int ref) (call_count2 : int ref)]
      in
      subscriber2 := Some (subscribe_exn bus [%here] ~f:callback2);
      write bus ();
      print_call_counts ();
      [%expect {|
        ((call_count1 1)
         (call_count2 1)) |}];
      write bus ();
      print_call_counts ();
      [%expect {|
        ((call_count1 1)
         (call_count2 1)) |}]
    ;;

    let%expect_test "subscribe_exn ~on_callback_raise" =
      let r1 = ref 0 in
      let r2 = ref 0 in
      let print_rs () = print_s [%message (r1 : int ref) (r2 : int ref)] in
      let bus =
        create
          [%here]
          Arity1
          ~on_subscription_after_first_write:Allow
          ~on_callback_raise:(fun _ -> incr r1)
      in
      ignore
        (subscribe_exn
           bus
           [%here]
           ~f:(fun () -> failwith "")
           ~on_callback_raise:(fun _ -> incr r2)
         : _ Subscriber.t);
      print_rs ();
      [%expect {|
        ((r1 0)
         (r2 0)) |}];
      write bus ();
      print_rs ();
      [%expect {|
        ((r1 0)
         (r2 1)) |}]
    ;;

    let%expect_test "correct exception raised for [subscribe_exn ~extract_exn:true]" =
      List.iter Bool.all ~f:(fun extract_exn ->
        let bus =
          create
            [%here]
            Arity1
            ~on_subscription_after_first_write:Allow
            ~on_callback_raise:ignore
        in
        ignore
          (subscribe_exn
             bus
             [%here]
             ~extract_exn
             ~f:(fun () -> assert false)
             ~on_callback_raise:(fun error ->
               print_s
                 ~hide_positions:true
                 [%message (extract_exn : bool) (error : Error.t)])
           : (unit -> _) Subscriber.t);
        write bus ());
      [%expect
        {|
        ((extract_exn false)
         (error (
           "Bus subscriber raised"
           (exn "Assert_failure test_bus.ml:LINE:COL")
           (backtrace ("<backtrace elided in test>"))
           (subscriber (
             Bus.Subscriber.t (
               (on_callback_raise <fun>)
               (subscribed_from   lib/bus/test/test_bus.ml:LINE:COL)))))))
        ((extract_exn true)
         (error       "Assert_failure test_bus.ml:LINE:COL")) |}]
    ;;

    let%expect_test "subscribe_exn ~on_callback_raise:raise" =
      let r = ref 0 in
      let print_r () = print_s [%message (r : int ref)] in
      let bus =
        create
          [%here]
          Arity1
          ~on_subscription_after_first_write:Allow
          ~on_callback_raise:(fun _ -> incr r)
      in
      ignore
        (subscribe_exn
           bus
           [%here]
           ~f:(fun () -> failwith "")
           ~on_callback_raise:Error.raise
         : _ Subscriber.t);
      print_r ();
      [%expect {|
        (r 0) |}];
      write bus ();
      print_r ();
      [%expect {|
        (r 1) |}]
    ;;

    let%expect_test "close is idempotent" =
      let bus = create1 [%here] ~on_subscription_after_first_write:Raise in
      close bus;
      close bus
    ;;

    let%expect_test "write raises after close" =
      let bus = create1 [%here] ~on_subscription_after_first_write:Raise in
      write bus ();
      close bus;
      print_s [%message (is_closed bus : bool)];
      [%expect {|
        ("is_closed bus" true) |}];
      show_raise ~hide_positions:true (fun () -> write bus ());
      [%expect
        {|
        (raised (
          "[Bus.write] called on closed bus"
          ((callback_arity Arity1)
           (created_from   lib/bus/test/test_bus.ml:LINE:COL)
           (on_subscription_after_first_write Raise)
           (state                             Closed)
           (write_ever_called                 true)
           (subscribers ()))
          lib/bus/src/bus.ml:LINE:COL)) |}]
    ;;

    let%expect_test "[subscribe_exn] does not raise after [close], and [on_close] is \
                     not called"
      =
      let bus = create1 [%here] ~on_subscription_after_first_write:Raise in
      let bus_r = read_only bus in
      close bus;
      ignore
        (subscribe_exn
           bus_r
           [%here]
           ~on_close:(fun () -> raise_s [%message "[on_close] should not be called"])
           ~f:ignore
         : _ Subscriber.t)
    ;;

    let%expect_test "during a write, [subscribe_exn] does not raise after [close], \
                     and [on_close] is not called"
      =
      let bus = create1 [%here] ~on_subscription_after_first_write:Allow in
      let bus_r = read_only bus in
      ignore
        (subscribe_exn bus_r [%here] ~f:(fun () ->
           close bus;
           ignore
             (subscribe_exn
                bus_r
                [%here]
                ~on_close:(fun () ->
                  raise_s [%message "[on_close] should not be called"])
                ~f:ignore
              : _ Subscriber.t))
         : _ Subscriber.t);
      write bus ()
    ;;

    let%expect_test "During write, if [close] and [subscribe_exn] are called, \
                     [on_close] will be called only if the subscriptions happen \
                     before [close]. This depends on the order of when the callbacks \
                     were fired, which could change over time."
      =
      let bus = create1 [%here] ~on_subscription_after_first_write:Allow in
      let bus_r = read_only bus in
      let subscribe_with_on_close i () =
        ignore
          (subscribe_exn
             bus_r
             [%here]
             ~on_close:(fun () -> printf "[on_close] #%d called\n" i)
             ~f:ignore
           : _ Subscriber.t)
      in
      let callbacks =
        [ subscribe_with_on_close 0
        ; (fun () ->
             subscribe_with_on_close 1 ();
             close bus;
             subscribe_with_on_close 2 ())
        ; subscribe_with_on_close 3
        ]
      in
      List.iter callbacks ~f:(fun callback ->
        ignore (subscribe_exn bus_r [%here] ~f:callback : _ Subscriber.t));
      write bus ();
      (* Since [close] is called after the [on_close] functions 0 and 1 are registered,
         only those two are called. *)
      [%expect {|
        [on_close] #0 called
        [on_close] #1 called |}]
    ;;

    let%expect_test "During write, if [unsubscribe] is called before [close], then \
                     the corresponding [on_close] will not be called. But if [close] \
                     is called before [unsubscribe], [on_close] will be called."
      =
      let bus = create1 [%here] ~on_subscription_after_first_write:Allow in
      let bus_r = read_only bus in
      let subscriber1 =
        subscribe_exn
          bus_r
          [%here]
          ~on_close:(fun () ->
            require [%here] false;
            print_s [%message "subscriber1 [on_close]"])
          ~f:ignore
      in
      ignore
        (subscribe_exn bus_r [%here] ~f:(fun () ->
           print_s [%message "unsubscribing subscriber1"];
           unsubscribe bus_r subscriber1;
           print_s [%message "closing bus"];
           close bus)
         : _ Subscriber.t);
      let subscriber2 =
        subscribe_exn
          bus_r
          [%here]
          ~on_close:(fun () -> print_s [%message "subcriber2 [on_close]"])
          ~f:ignore
      in
      ignore
        (subscribe_exn bus_r [%here] ~f:(fun () ->
           print_s [%message "unsubscribing subscriber2"];
           unsubscribe bus_r subscriber2)
         : _ Subscriber.t);
      write bus ();
      [%expect
        {|
        "unsubscribing subscriber1"
        "closing bus"
        "unsubscribing subscriber2"
        "subcriber2 [on_close]" |}]
    ;;

    let%expect_test "after [close], [write t] without the value to be written" =
      let bus = create1 [%here] ~on_subscription_after_first_write:Raise in
      close bus;
      show_raise ~hide_positions:true (fun () -> write bus);
      [%expect {| "did not raise" |}]
    ;;

    let%expect_test "close takes effect after all writes" =
      let bus = create1 [%here] ~on_subscription_after_first_write:Raise in
      let call_count = ref 0 in
      let callback () =
        incr call_count;
        close bus
      in
      let (_ : _ Subscriber.t) = subscribe_exn bus [%here] ~f:callback in
      let (_ : _ Subscriber.t) = subscribe_exn bus [%here] ~f:callback in
      write bus ();
      print_s [%message (call_count : int ref)];
      [%expect {|
        (call_count 2) |}]
    ;;

    let%expect_test "raising in [on_callback_raise] closes the bus" =
      let t =
        create
          [%here]
          Arity1
          ~on_subscription_after_first_write:Raise
          ~on_callback_raise:Error.raise
      in
      iter_exn t [%here] ~f:(fun _ -> failwith "");
      require [%here] (does_raise (fun () -> write t ()));
      require [%here] (is_closed t)
    ;;

    let%expect_test "fold threads values through future calls" =
      let bus = create1 [%here] ~on_subscription_after_first_write:Raise in
      let last1 = ref 0 in
      let last2 = ref "" in
      let print_lasts () = print_s [%message (last1 : int ref) (last2 : string ref)] in
      fold_exn bus [%here] Arity1 ~init:0 ~f:(fun acc () ->
        let this = acc + 1 in
        last1 := this;
        this);
      fold_exn bus [%here] Arity1 ~init:"" ~f:(fun acc () ->
        let this = acc ^ "." in
        last2 := this;
        this);
      write bus ();
      write bus ();
      write bus ();
      print_lasts ();
      [%expect {|
        ((last1 3)
         (last2 ...)) |}];
      write bus ();
      write bus ();
      print_lasts ();
      [%expect {|
        ((last1 5)
         (last2 .....)) |}]
    ;;
  end

  module A1_1 = Arity_1 (struct
      let write = write
    end)

  module Arity_2 (W : sig
      val write : ('a -> 'b -> unit) Read_write.t -> 'a -> 'b -> unit
    end) =
  struct
    let write = W.write

    let%expect_test "on_callback_raise called for every error" =
      let call_count = ref 0 in
      let on_callback_raise _ = incr call_count in
      let bus =
        create
          [%here]
          Arity2
          ~on_subscription_after_first_write:Allow
          ~on_callback_raise
      in
      let callback1 _ _ = failwith "callback1" in
      let callback2 _ _ = failwith "callback2" in
      ignore (subscribe_exn bus [%here] ~f:callback1 : _ Subscriber.t);
      ignore (subscribe_exn bus [%here] ~f:callback2 : _ Subscriber.t);
      write bus () ();
      print_s [%message (call_count : int ref)];
      [%expect {|
        (call_count 2) |}]
    ;;

    let%expect_test "mid-callback subscribe_exn takes effect for the next write" =
      let bus = create2 [%here] ~on_subscription_after_first_write:Allow in
      let call_count2 = ref 0 in
      let callback2 _ _ = incr call_count2 in
      let call_count1 = ref 0 in
      let callback1 _ _ =
        incr call_count1;
        ignore (subscribe_exn bus [%here] ~f:callback2 : _ Subscriber.t)
      in
      ignore (subscribe_exn bus [%here] ~f:callback1 : _ Subscriber.t);
      write bus () ();
      write bus () ();
      print_s [%message (call_count1 : int ref) (call_count2 : int ref)];
      [%expect {|
        ((call_count1 2)
         (call_count2 1)) |}]
    ;;
  end

  module A2_2 = Arity_2 (struct
      let write = write2
    end)
end
(* This signature constraint is here to remind us to add a unit test whenever
   the interface to [Bus] changes. *) :
  module type of Bus)
