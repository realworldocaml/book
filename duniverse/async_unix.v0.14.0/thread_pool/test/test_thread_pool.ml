open! Core
open! Expect_test_helpers_core
open! Thread_pool
open! Thread_pool.Private
module Debug = Async_kernel.Async_kernel_private.Debug

let debug = Debug.thread_pool

let sec = Time_ns.Span.of_sec

let%test_module _ =
  (module struct
    let () = check_invariant := true

    let wait_until_no_unfinished_work t =
      let rec loop i =
        if unfinished_work t > 0
        then (
          Time_ns.pause (sec 0.01);
          loop (i + 1))
      in
      loop 0
    ;;

    (* [create] and [finished_with]. *)
    let%expect_test _ =
      let t = ok_exn (create ~max_num_threads:1 ()) in
      assert (max_num_threads t = 1);
      assert (num_threads t = 0);
      (* no threads should have been created *)
      finished_with t
    ;;

    (* Error cases for [create]. *)
    let%expect_test _ =
      List.iter [ -1; 0 ] ~f:(fun max_num_threads ->
        require [%here] (Result.is_error (create ~max_num_threads ())))
    ;;

    (* Error cases for [add_work]. *)
    let%expect_test _ =
      let t = ok_exn (create ~max_num_threads:1 ()) in
      finished_with t;
      assert (Result.is_error (add_work t ignore))
    ;;

    (* Work finishing after [finished_with] is called causes the thread pool to finish. *)
    let%expect_test _ =
      let t = ok_exn (create ~max_num_threads:1 ()) in
      let finish_work = Thread_safe_ivar.create () in
      ok_exn (add_work t (fun () -> Thread_safe_ivar.read finish_work));
      finished_with t;
      Thread_safe_ivar.fill finish_work ();
      wait_until_no_unfinished_work t;
      assert (is_finished t)
    ;;

    (* Check that the expected concurrency is used. *)
    let%expect_test _ =
      List.iter [ 1; 2; 5; 10; 100; 1000 ] ~f:(fun num_jobs ->
        List.iter ([ 1; 2; 5; 10 ]
                   @ if Sys.word_size = 32
                   then [] (* not enough address space when the stack limit is high *)
                   else [ 100 ]) ~f:(fun max_num_threads ->
          if debug
          then
            eprintf
              "num_jobs = %d  max_num_threads = %d\n%!"
              num_jobs
              max_num_threads;
          let mutex = Nano_mutex.create () in
          let expected_max_concurrent_jobs = min num_jobs max_num_threads in
          let max_observed_concurrent_jobs = ref 0 in
          let num_concurrent_jobs = ref 0 in
          let job_starts = ref [] in
          let t = ok_exn (create ~max_num_threads ()) in
          let worker_threads_have_fully_started = Thread_safe_ivar.create () in
          let worker_threads_should_continue = Thread_safe_ivar.create () in
          let (_ : Core.Thread.t) =
            Core.Thread.create
              ~on_uncaught_exn:`Print_to_stderr
              (fun () ->
                 let start = Time_ns.now () in
                 let rec loop () =
                   if is_in_use t
                   then (
                     let how_long = Time_ns.diff (Time_ns.now ()) start in
                     if Time_ns.Span.( >= ) how_long (sec 10.)
                     then (
                       Debug.log
                         "thread-pool unit test hung"
                         ( t
                         , worker_threads_have_fully_started
                         , worker_threads_should_continue )
                         [%sexp_of:
                           t * unit Thread_safe_ivar.t * unit Thread_safe_ivar.t];
                       Caml.exit 1)
                     else (
                       Time_ns.pause (sec 0.1);
                       loop ()))
                 in
                 loop ())
              ()
          in
          let jobs = ref [] in
          for i = 0 to num_jobs - 1 do
            let job =
              ok_exn
                (add_work t (fun () ->
                   Nano_mutex.critical_section mutex
                     ~f:(fun () ->
                       job_starts := i :: !job_starts;
                       if List.length !job_starts = expected_max_concurrent_jobs
                       then Thread_safe_ivar.fill worker_threads_have_fully_started ();
                       incr num_concurrent_jobs;
                       max_observed_concurrent_jobs :=
                         max !max_observed_concurrent_jobs !num_concurrent_jobs;
                       assert (!num_concurrent_jobs <= max_num_threads);
                     );
                   Thread_safe_ivar.read worker_threads_should_continue;
                   Nano_mutex.critical_section mutex
                     ~f:(fun () ->
                       decr num_concurrent_jobs)))
            in
            jobs := job :: !jobs
          done;
          Thread_safe_ivar.read worker_threads_have_fully_started;
          assert (!num_concurrent_jobs = expected_max_concurrent_jobs);
          assert (List.length !job_starts = expected_max_concurrent_jobs);
          if max_num_threads = 1
          then
            assert (
              List.equal
                Int.equal
                !job_starts
                (List.init expected_max_concurrent_jobs ~f:Fn.id));
          Thread_safe_ivar.fill worker_threads_should_continue ();
          wait_until_no_unfinished_work t;
          assert (!max_observed_concurrent_jobs = expected_max_concurrent_jobs);
          if max_num_threads = 1
          then
            assert (
              List.equal
                Int.equal
                (List.rev !job_starts)
                (List.init num_jobs ~f:Fn.id));
          assert (num_threads t <= max_num_threads);
          finished_with t))
    ;;

    (* Helper threads. *)

    let%expect_test _ =
      let t = ok_exn (create ~max_num_threads:1 ()) in
      let helper_thread = ok_exn (create_helper_thread t) in
      let helper_continue = Thread_safe_ivar.create () in
      let helper_finished = Thread_safe_ivar.create () in
      let work_finished = Thread_safe_ivar.create () in
      ok_exn
        (add_work_for_helper_thread t helper_thread (fun () ->
           Thread_safe_ivar.read helper_continue;
           Thread_safe_ivar.fill helper_finished ()));
      ok_exn (add_work t (fun () -> Thread_safe_ivar.fill work_finished ()));
      Thread_safe_ivar.fill helper_continue ();
      Thread_safe_ivar.read helper_finished;
      finished_with_helper_thread t helper_thread;
      Thread_safe_ivar.read work_finished;
      wait_until_no_unfinished_work t;
      finished_with t
    ;;

    (* Calling [finished_with_helper_thread] while work remains is allowed, and causes
       the thread to be returned to the general pool once it finishes all its work. *)
    let%expect_test _ =
      let t = ok_exn (create ~max_num_threads:1 ()) in
      let helper_thread = ok_exn (create_helper_thread t) in
      let general_work_got_done = ref false in
      ok_exn (add_work t (fun () -> general_work_got_done := true));
      let helper_continue = Thread_safe_ivar.create () in
      let helper_finished = Thread_safe_ivar.create () in
      ok_exn
        (add_work_for_helper_thread t helper_thread (fun () ->
           Thread_safe_ivar.read helper_continue;
           Thread_safe_ivar.fill helper_finished ()));
      finished_with_helper_thread t helper_thread;
      assert (Result.is_error (add_work_for_helper_thread t helper_thread Fn.ignore));
      assert (not !general_work_got_done);
      Thread_safe_ivar.fill helper_continue ();
      Thread_safe_ivar.read helper_finished;
      wait_until_no_unfinished_work t;
      assert !general_work_got_done;
      finished_with t
    ;;

    (* Error cases for mismatches between pool and helper thread. *)
    let%expect_test _ =
      let t = ok_exn (create ~max_num_threads:1 ()) in
      let t_bad = ok_exn (create ~max_num_threads:1 ()) in
      let helper_thread = ok_exn (create_helper_thread t) in
      assert (Result.is_error (add_work_for_helper_thread t_bad helper_thread ignore));
      assert (
        Result.is_error
          (Result.try_with (fun () -> finished_with_helper_thread t_bad helper_thread)));
      finished_with_helper_thread t helper_thread;
      finished_with t
    ;;

    (* Setting thread name and priority. *)
    let%expect_test _ =
      let module RLimit = Core.Unix.RLimit in
      Result.iter RLimit.nice ~f:(fun rlimit_nice ->
        let test_parameters =
          let nice_limit = RLimit.get rlimit_nice in
          match nice_limit.max with
          | Infinity ->
            let max = 40 in
            `Test ({ nice_limit with cur = Limit (Int64.of_int_exn max) }, max)
          | Limit max ->
            if Int64.( < ) max (Int64.of_int 2)
            then `Cannot_test
            else `Test ({ nice_limit with cur = Limit max }, Int64.to_int_exn max)
        in
        match test_parameters with
        | `Cannot_test -> ()
        | `Test (nice_limit, cur_limit) ->
          Core.Unix.RLimit.set rlimit_nice nice_limit;
          for priority = 20 - cur_limit to 20 do
            let initial_priority = Priority.of_int priority in
            match Linux_ext.getpriority, Linux_ext.pr_get_name with
            | Error _, _
            | _, Error _ -> ()
            | Ok getpriority, Ok get_name ->
              let t = ok_exn (create ~max_num_threads:2 ()) in
              let check4
                    ~name
                    ~priority
                    (check :
                       ?name:string -> ?priority:Priority.t -> unit -> unit Or_error.t)
                =
                ok_exn (check ());
                ok_exn (check ~name ());
                ok_exn (check ~priority ());
                ok_exn (check ~name ~priority ());
                wait_until_no_unfinished_work t
              in
              check4
                ~name:"new name"
                ~priority:(Priority.decr initial_priority)
                (fun ?name ?priority () ->
                   add_work ?priority ?name t (fun () ->
                     assert (
                       String.equal
                         (get_name ())
                         (Option.value name ~default:default_thread_name));
                     assert (
                       Priority.equal
                         (getpriority ())
                         (Option.value priority ~default:(default_priority t)))));
              check4
                ~name:"new name"
                ~priority:(Priority.decr initial_priority)
                (fun ?name ?priority () ->
                   let helper_thread =
                     ok_exn (create_helper_thread t ?priority ?name)
                   in
                   let default_thread_name =
                     Option.value name ~default:default_thread_name
                   in
                   let default_priority =
                     Option.value priority ~default:(default_priority t)
                   in
                   check4
                     ~name:"new name 2"
                     ~priority:(Priority.decr initial_priority)
                     (fun ?name ?priority () ->
                        add_work_for_helper_thread
                          ?priority
                          ?name
                          t
                          helper_thread
                          (fun () ->
                             assert (
                               String.equal
                                 (get_name ())
                                 (Option.value name ~default:default_thread_name));
                             assert (
                               Priority.equal
                                 (getpriority ())
                                 (Option.value priority ~default:default_priority))));
                   finished_with_helper_thread t helper_thread;
                   Ok ());
              finished_with t
          done)
    ;;

    (* [Core.Thread.create] failure *)
    let%expect_test _ =
      let t = ok_exn (create ~max_num_threads:2 ()) in
      (* simulate failure *)
      set_last_thread_creation_failure t (Time_ns.now ());
      set_thread_creation_failure_lockout t (sec 100.);
      let finish_work = Thread_safe_ivar.create () in
      ok_exn (add_work t (fun () -> Thread_safe_ivar.read finish_work));
      assert (has_unstarted_work t);
      set_thread_creation_failure_lockout t (sec 0.);
      ok_exn (add_work t (fun () -> Thread_safe_ivar.read finish_work));
      Thread_safe_ivar.fill finish_work ();
      wait_until_no_unfinished_work t
    ;;

    let%expect_test "become_helper_thread" =
      let t = ok_exn (create ~max_num_threads:1 ()) in
      let helper_thread = Thread_safe_ivar.create () in
      ok_exn
        (add_work t (fun () ->
           Thread_safe_ivar.fill helper_thread (ok_exn (become_helper_thread t))));
      let helper_thread = Thread_safe_ivar.read helper_thread in
      let helper_thread_finished = Thread_safe_ivar.create () in
      ok_exn
        (add_work_for_helper_thread t helper_thread (fun () ->
           Thread_safe_ivar.fill helper_thread_finished ()));
      Thread_safe_ivar.read helper_thread_finished;
      finished_with_helper_thread t helper_thread;
      wait_until_no_unfinished_work t
    ;;

    (* Affinity *)
    module Cpuset = Cpu_affinity.Cpuset

    let%expect_test _ =
      let t = ok_exn (create ~max_num_threads:1 ()) in
      assert (
        match cpu_affinity t with
        | Inherit -> true
        | Cpuset _ -> false)
    ;;

    let%expect_test _ =
      let cpuset = Int.Set.singleton 0 |> Cpuset.create_exn in
      let t = ok_exn (create ~cpu_affinity:(Cpuset cpuset) ~max_num_threads:1 ()) in
      assert (
        match cpu_affinity t with
        | Inherit -> true
        | Cpuset actual_cpuset -> Cpuset.equal actual_cpuset cpuset)
    ;;

    let test_affinity ~(cpu_affinity : Cpu_affinity.t) ~max_num_threads =
      let get_affinity_self () =
        ok_exn Thread.getaffinity_self_exn () |> Cpuset.create_exn
      in
      let expected_cpuset =
        match cpu_affinity with
        | Inherit -> get_affinity_self ()
        | Cpuset cpuset -> cpuset
      in
      let reported_cpuset = ref None in
      let t = ok_exn (create ~cpu_affinity ~max_num_threads ()) in
      ok_exn (add_work t (fun () -> reported_cpuset := Some (get_affinity_self ())));
      finished_with t;
      wait_until_no_unfinished_work t;
      assert (Cpuset.equal expected_cpuset (Option.value_exn !reported_cpuset))
    ;;

    let%expect_test "inherit affinity" =
      test_affinity ~cpu_affinity:Inherit ~max_num_threads:1
    ;;

    let%expect_test "empty cpuset is invalid" =
      require_does_raise [%here] ~hide_positions:true (fun () ->
        ignore (Cpuset.create_exn Int.Set.empty : Cpuset.t));
      [%expect {|
        ("validation failed" (
          ()
          ("validation errors" (("" "value 0 < bound 1")))
          lib/thread_pool_cpu_affinity/src/thread_pool_cpu_affinity.ml:LINE:COL)) |}]
    ;;

    let%expect_test "can affinitize to a single core" =
      test_affinity
        ~cpu_affinity:(Cpuset (Int.Set.singleton 0 |> Cpuset.create_exn))
        ~max_num_threads:1
    ;;

    module Quickcheck_cpuset = struct
      type t = Int.Set.t [@@deriving sexp_of]

      let quickcheck_generator =
        let cpuids = ok_exn Thread.getaffinity_self_exn () |> Int.Set.to_list in
        (* Fail if it's not possible to test on more than one CPU. We already
           have a test above to cover that case, so the rest of this would be
           pointless. *)
        let cpucount = List.length cpuids in
        assert (cpucount > 1);
        let open Base_quickcheck in
        let open Generator.Let_syntax in
        let%bind permutation = Generator.list_permutations cpuids in
        let%map count = Generator.int_inclusive 1 (cpucount - 1) in
        List.take permutation count |> Int.Set.of_list
      ;;

      let quickcheck_shrinker = Base_quickcheck.Shrinker.atomic
    end

    let%expect_test "can affinitize to any subset on the system" =
      Base_quickcheck.Test.run_exn
        ~config:{ Base_quickcheck.Test.default_config with test_count = 100 }
        ~f:(fun cpuset ->
          test_affinity
            ~cpu_affinity:(Cpuset (cpuset |> Cpuset.create_exn))
            ~max_num_threads:1)
        (module Quickcheck_cpuset)
    ;;
  end)
;;
