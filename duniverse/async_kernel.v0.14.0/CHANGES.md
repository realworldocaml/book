## git version

- Deprecated `Deferred.choice` type alias.  Use `Deferred.Choice.t`
  instead.

- Removed deprecated `ignore` functions from `Deferred`,
  `Deferred.Result`, and `Deferred.Or_error` (and the corresponding
  `Eager_deferred` modules).

- `Deferred.Memo` no longer requires the key to be `of_sexp`'able.

- rename paramater of `Pipe.merge` from `cmp` to `compare`.

- Added `Pipe.fork` function, to transfer the read values into two
  freshly-created readers

- Deprecate `Synchronous_time_source.alarm_upper_bound` in favor of
  `Synchronous_time_source.max_allowed_alarm_time`.

- Add `get_bind_to_interface` function to read current status of SO_BINDTODEVICE socket
  option. Make both `bind_to_interface` and `get_bind_to_interface` use new type
  `Bound_interface_name.t` for its argument/return value.

## v0.11

- Removed a number of modules from `Async_kernel.Async_kernel_private`.
  Exposed some modules directly in `Async_kernel`:
  + `Async_kernel_scheduler`
  + `Persistent_connection` (as `Async_kernel_persistent_connection`)
  + `Require_explicit_time_source`
    (as `Async_kernel_require_explicit_time_source`)

- Changed the return types in `Deferred_memo` to emphasis the correct
  staging, using the type `'a Staged.t`.

## v0.10

- Added `Pipe.singleton` function, for creating a pipe filled with a single
  element.

- Removed a per-cycle allocation from the Async scheduler.

- Added `Synchronous_time_source.timing_wheel_now`, which removes the special
  behavior of now for the wall-clock time source.

- Removed some type aliases from `Deferred` submodules: `Array.t`, `List.t`,
  `Queue.t`, `Sequence.t`.

## v0.9

## 113.43.00

- Export `Time_source` from `Async_kernel`.

- Name the non-`t` arguments to `Error.tag` and similar functions to allow
  easier partial application.

- Name the non-`t` arguments to `Error.tag` and similar functions to allow
  easier partial application.

- Switched if-then-else style from:

    if test then begin
      E1;
      E2;
    end else begin
      E3;
      E4;
    end

  to:

    if test
    then (
      E1;
      E2)
    else (
      E3;
      E4);

  This style puts keywords at the start of lines and closing delimiters
  at the end.  And makes it lighter-weight to be more consistent about
  using the delimiters.

- Switched `Async_kernel` from `>>=` and `>>|` to `let%bind` and
  `let%map`, where appropriate.

- Mirroring `Or_error.error_s`, like several of the other `Or_error`
  functions are mirrored

- Improved a unit test in `Pipe`.

- Removed from `Monitor.try_with`'s implementation the use of
  `Deferred.choose`, which is very costly in terms of allocation and
  memory use.

- Reworked the implementation of `Monitor.try_with` and related
  functions, using a new underlying concept, `Ok_and_exns`, which
  represents the output of a computation running in a detached monitor,
  i.e.:

    type 'a t =
      { ok   : 'a Deferred.t
      ; exns : exn Stream.t
      }

  Using this explicit type simplifies reasoning, as compared to implicit
  monitor error streams.

- Some minor cleanup in `core_kernel/src/scheduler.ml`.

- Added `Scheduler.make_async_unusable`.

- Added `Pipe.create_reader`, which is like `Pipe.init`, but has different
  behavior w.r.t. `f` raising.  It forces the caller to choose the behavior
  on raise to avoid subtle bugs related to closing the reading end.
  The comment recommends `~close_on_exception:false`.

    val create_reader
      :  close_on_exception:bool
      -> ('a Writer.t -> unit Deferred.t)
      -> 'a Reader.t

  A child feature deprecates `init` and replaces calls with
  `create_reader ~close_on_exception:true`.

  `Pipe.init_reader` was renamed to `Pipe.create_writer` so that the names are
  consistent.  Its behaviour was not changed: it also closes the pipe if `f`
  raises, but here this is probably what you want, so that the writer is notified
  that the reader is dead.

  Changed `Pipe.of_sequence`, which is implemented with `create_reader`,
  to use `~close_on_exception:false`.  This seems like an improvement, because
  it won't treat raising as end of sequence.

  Changed `Pipe.unfold`, which is implemented with `create_reader`, to use
  the `~close_on_exception:false`.  This shouldn't be a problem, because
  `unfold` is barely used.  And it was never specified what `f` raising
  means.  It seems fine to not treat raising as end-of-sequence, because
  `unfold` already has a way to express end-of-sequence.

  Here's some more explanation of motivation for the change.  In the
  current world, `Pipe.init` looks like this:

    let init f =
      let r, w = create () in
      don't_wait_for (Monitor.protect (fun () -> f w)
                        ~finally:(fun () -> close w; Deferred.unit));
      r
    ;;

  This means that if `f` raises, then the pipe is both closed and the
  exception is sent to the monitor active when `init` was called.

  If you have something (like `Pipe.fold_without_pushback`) consuming
  the pipe, the exception being delivered can race against the fold
  finishing, and you could end up missing it. Moreover, the race seems
  to reliably go in the direction you don't want:

    $ cat pipes.ml
    #!/some/path/jane-script run
    open Core.Std
    open Async.Std

    let main () =
      Monitor.try_with_or_error (fun () ->
        Pipe.init (fun _writer -> assert false)
        |> Pipe.fold_without_pushback ~init:() ~f:(fun () () -> ())
      )
      >>= fun res ->
      printf !"Result: %{sexp:unit Or_error.t}\n" res;
      exit 0

    let () =
      don't_wait_for (main ());
      never_returns (Scheduler.go ())

    $ ./pipes.ml
    Result: (Ok ())
    2015-10-21 15:42:22.494737+01:00 Error Exception raised to Monitor.try_with that already returned
      (monitor.ml.Error_
      ((exn "Assert_failure /home/toto/pipes.ml:7:30")
    `snip`

- Deprecated `Pipe.init` and replaced its uses with the semantically
  equivalent:

    Pipe.create_reader ~close_on_exception:true

  See the parent feature for an explanation of why `Pipe.init` had the
  wrong default behavior.

  If this is the third time you're reading this feature, please accept my
  apologies!  Review of this feature produced requests for the parent feature.

- Adds phantom type to Validated as witness of invariant

  The witness prevents types from different applications of the Validated functors
  from unifying with one another.

- add compare to pipe on the internal id

- Added `Bvar`, a new basic data structure, to Async.

  `Bvar` is like an `Ivar` that can be filled multiple times, or
  can be thought of as a condition variable that supports `broadcast`
  but does not support `signal`.

- Switch the implementation of `Scheduler.yield` to use a `Bvar.t`

- Adds `Scheduler.yield_until_no_jobs_remain` which is determined when
  the transitive closure of all currently runnable jobs and jobs that
  become runnable in the process of their execution have been run to
  completion.

  This differs from `Scheduler.yield` whose result is determined as soon
  as the scheduler has completed the current cycle.

- Monad.Let_syntax.return

  Added to `Monad.Syntax.Let_syntax`:

    val return : 'a -> 'a t

  so that when one does:

    open Some_random_monad.Let_syntax

  `return` is in scope.

- Added `Bvar.invariant`.

- Moved the newly introduced expect test from `Async_kernel` to
  `Async_kernel_test`, to avoid having `Async_kernel` depend on `Unix`,
  which breaks the 32-bit build.

- An experiment to make async errors more readable.

  Before:

    (monitor.ml.Error_
     ((exn (Failure "something went wrong"))
      (backtrace
       ("Raised at file \"pervasives.ml\", line 30, characters 22-33"
        "Called from file \"deferred0.ml\", line 64, characters 64-69"
        "Called from file \"job_queue.ml\", line 160, characters 6-47" ""))
      (monitor
       (((name try_with) (here ()) (id 2) (has_seen_error true)
         (is_detached true))))))

    (monitor.ml.Error_
     ((exn (Failure "something went wrong"))
      (backtrace
       ("Raised at file \"pervasives.ml\", line 30, characters 22-33"
        "Called from file \"deferred0.ml\", line 64, characters 64-69"
        "Called from file \"job_queue.ml\", line 160, characters 6-47" ""))
      (monitor
       (((name foo) (here (lib/async/a.ml:13:56)) (id 2) (has_seen_error true)
         (is_detached true))))))

  After:

    (monitor.ml.Error (Failure "something went wrong")
     ("Raised at file \"pervasives.ml\", line 30, characters 22-33"
      "Called from file \"deferred0.ml\", line 64, characters 64-69"
      "Called from file \"job_queue.ml\", line 160, characters 6-47")))

    (monitor.ml.Error (Failure "something went wrong")
     ("Raised at file \"pervasives.ml\", line 30, characters 22-33"
      "Called from file \"deferred0.ml\", line 64, characters 64-69"
      "Called from file \"job_queue.ml\", line 160, characters 6-47"
      "Caught by monitor foo at file \"lib/async/a.ml\", line 13, characters 56-56"))

  So:
  - remove the trailing "" in the backtrace
  - remove the ugly underscore in the exception name
  - remove the monitor when it contains nothing useful
  - when the monitor contains something useful, namely name or position, just stick that
    information in the rest of the backtrace
  - remove the fields exception, backtrace, monitor. It's ok in the code, but in the
    sexp it doesn't add much.
  - remove the names for the monitors from `Monitor.try_with, Deferred.Or_error.try_with`
    and `Deferred.Or_error.try_with_join` since they are so common they contain no
    information

  A child feature tries to remove the useless bits of backtraces, since they can represent a
  significant source of noise (in the examples above, the whole backtraces are noise).

- Added module `Async.Std.Require_explicit_time_source`, so that one can
  require code to be explicit about what time source is used and not
  unintentionally use the wall clock.  The idiom is to do:

    open! Require_explicit_time_source

  or, in an import.ml:

    include Require_explicit_time_source

- Added a test demonstrating a space leak in `Monitor.try_with`, in which
  the `Ok` result is held on to by background jobs.

- Added `Async_kernel.Ivar_filler` a new module that is a reference to
  an ivar that allows one to fill the ivar, but not to read it.
  This allows the implementation to drop the reference to the ivar
  once it is full, which can be useful to avoid holding onto unused
  memory.

  This will be used to fix the space leak in `Monitor.try_with`.

- Fixed the space leak in `Monitor.try_with`, using an `Ivar_filler` to
  avoid holding on to memory unnecessarily.

  Here are the allocation numbers for `lib/async/bench/try_with.ml` at
  the base and tip of the feature.

      |                | base | tip |
      |----------------+------+-----|
      | minor words    |  113 | 120 |
      | promoted words |   74 |  78 |
      | live words     |   71 |  75 |

  Unsurprisingly, the `Ivar_filler` costs a little.  But not as much as
  switching the implementation back to `choose`.

- Remove more noise from the async errors, namely useless backtraces that say things like
  "and this is called in the scheduler, in Deferred.bind". We already know the error comes
  from async from the monitor.ml.Error line. Also this is frequently repeated, when several
  monitor errors are nested, making things worse.

  Here is a synthetic example:
  before:

    ((monitor.ml.Error (Failure "something went wrong")
      ("Raised at file \"pervasives.ml\", line 30, characters 22-33"
       "Called from file \"a.ml\", line 16, characters 17-48"
       "Called from file \"deferred0.ml\", line 64, characters 64-69"
       "Called from file \"job_queue.ml\", line 160, characters 6-47"))
     (monitor.ml.Error (Failure "something went wrong")
      ("Raised at file \"pervasives.ml\", line 30, characters 22-33"
       "Called from file \"a.ml\", line 22, characters 26-57"
       "Called from file \"deferred1.ml\", line 14, characters 63-68"
       "Called from file \"job_queue.ml\", line 160, characters 6-47"))
     (monitor.ml.Error (Failure "something went wrong")
      ("Raised at file \"pervasives.ml\", line 30, characters 22-33"
       "Called from file \"monitor.ml\", line 256, characters 42-51"
       "Called from file \"job_queue.ml\", line 160, characters 6-47"))
     (monitor.ml.Error (Failure "something went wrong")
      ("Raised at file \"pervasives.ml\", line 30, characters 22-33"
       "Called from file \"a.ml\", line 29, characters 26-57"
       "Called from file \"result.ml\", line 105, characters 9-15")))

  after:

    ((monitor.ml.Error (Failure "something went wrong")
      ("Raised at file \"pervasives.ml\", line 30, characters 22-33"
       "Called from file \"a.ml\", line 16, characters 17-48"))
     (monitor.ml.Error (Failure "something went wrong")
      ("Raised at file \"pervasives.ml\", line 30, characters 22-33"
       "Called from file \"a.ml\", line 22, characters 26-57"))
     (monitor.ml.Error (Failure "something went wrong")
      ("Raised at file \"pervasives.ml\", line 30, characters 22-33"))
     (monitor.ml.Error (Failure "something went wrong")
      ("Raised at file \"pervasives.ml\", line 30, characters 22-33"
       "Called from file \"a.ml\", line 29, characters 26-57"
       "Called from file \"result.ml\", line 105, characters 9-15")))

## 113.33.00

- In `Pipe`, deprecated `read_at_most` and `read_now_at_most`, and
  replaced them with `read'` and `read_now'`, respectively.

- Reverted a recent feature that changed `Pipe`'s default
  `max_queue_length` from `Int.max_value` to `100`.  It looks like 100
  is to small to keep `Pipe` overhead to a minimum -- we saw a
  noticeable slowdown in Async.Log.  We're going to do some more
  benchmarking and try to reduce the overhead and choose a better
  number.

- Added `Async.Std.Let_syntax = Deferred.Let_syntax`, which makes it
  possible to use `%bind` and `%map` after `open Async.Std`.

  This required fixing a few places where the new `Let_syntax` in scope
  shadowed `Command.Param.Let_syntax`.

- Improve stacktrace for unhandle async exception in js\_of\_ocaml.

  Wrapping the unhandle exception with `Error.create` will serialize this exception
  and prevent us to have good error reporting in Js\_of\_ocaml.

- Reworked `Async_kernel`'s clock handling to make it possible to use a
  notion of time distinct from the wall-clock time used by `Clock_ns`
  and maintained by the Async scheduler.  There is now a self-contained
  `Time_source` module, a data structure that holds a timing-wheel.  All
  of the code that was in `Clock_ns` and implicitly used the Async
  scheduler and its timing wheel is now in `Time_source`, and is
  suitably parameterized to work on an arbitrary time source with an
  arbitrary timing wheel.  `Clock_ns` is now a wrapper around
  `Time_source` that implicitly uses the Async scheduler's time source.

  `Time_source` still uses the Async scheduler for running jobs that
  fire -- the new power comes from the user being able to advance time
  distinctly from the wall clock.

- Changed `Time_source.sexp_of_t` to display "<wall_clock>" for the
  wall-clock time source.  We don't want to display the events because
  that can be ridiculously large.

  And, for non-wall-clock time sources, don't show the `Job.t`s in
  events, because they are uninformative pool pointers.

- Added `Time_source.advance_by`.

- Added `Time_source.alarm_precision`.

- Added to `Async.Scheduler` `Time_ns` analogs of `cycle_start` and
  `cycle_times`:

      val cycle_start_ns : unit -> Time_ns.t
      val cycle_times_ns : unit -> Time_ns.Span.t Stream.t

- Add `Deferred.map_choice` for transforming the output of `Deferred.choice`.

- Remove an avoidable call to `is_determined` in `Deferred.value_exn`.  The patch is
  meant to preserve current error messages without adding any allocation overhead
  compared to the existing.

  Context: implementation of Eager_deferred in the works that essentially wants to
  redefine map, bind, etc, in the following way:

      let map t ~f =
        if Deferred.is_determined t
        then return (f (Deferred.value_exn t))
        else Deferred.map t ~f
      ;;

- Add `Pipe.read_choice` to encode the appropriate way of combining `values_available`
  and `read_now` to conditionally read from a pipe.

## 113.24.00

N.B. some interface change in Core (notably to `Hashtbl` and `Map`) implied some
interface change in this package as well, although they are not mentionned in
this changelog.

- Switched to ppx.

- Improved the Async scheduler's to allocate a `handle_fired` function
  once, rather than every time it calls `advance_clock`.

- Removed configurability of Monitor's `try_with`-ignored-exception
  handling, i.e. removed `Monitor.try_with_rest_handling` and
  `Monitor.try_with_ignored_exn_handling`.

  The behavior of exceptions raised to a monitor after it returns is
  unchanged -- i.e. they are logged, as they have been since 112.28.

  Changed `Monitor.try_with`'s `?rest` argument from:

    ?rest : ` `Ignore | `Raise `
    ?rest : ` `Log | `Raise `

  This naming reflects the fact that subsequent exceptions are logged,
  not ignored.

- In `Async_kernel`, moved `Scheduler.set_execution_context` from the
  `Types.Scheduler` module to its own file.  Because `Types` is a
  `module rec`, `set_execution_context` hadn't been inlined and was
  called via `caml_apply2`.  In its own file, it will be inlined.

  This release creates a new scheduler0.ml, and moves the old
  scheduler0.ml to scheduler1.ml.

- Fixed a space leak in `Pipe` due to a pipe holding pointers to its
  `upstream_flusheds` after they are closed.  The leak shows up in
  `Pipe.transfer` and related functions, e.g. with:

    Pipe.transfer temporary_pipe long_lived_pipe

  called repeatedly, in which `long_lived_pipe` would accumulate a large
  number of `upstream_flusheds`.

  The fix is to maintain `upstream_flusheds` as a `Bag.t`, and to remove
  an upstream pipe when it is closed.

- Implement `Pipe.of_sequence`

- Improved the error message when an exception is raised to a
  `Monitor.try_with` that has returned before Async has initialized
  `Monitor.try_with_log_exn`.

- Improved the implementation of `Monitor.get_next_error`, by replacing
  the monitor's list of handlers:

    ; mutable handlers_for_next_error : (exn -> unit) list

  with a single ivar:

    ; mutable next_error              : exn Ivar.t

  I think this wasn't done originally because of a dependency cycle.
  But now that we have types.ml, we can do the clear thing.

- Improved the implementation of Monitor exception handling,
  i.e. `detach_and_iter_errors`to make it clear that `Monitor.send_exn`
  does not run user code -- it only schedules jobs.

- Fix an error message in `Pipe` to match the condition that led to it.

- Add a new pipe constructor:

    val unfold : 'b -> f:('b -> ('a * 'b) option Deferred.t) -> 'a Reader.t

  `unfold` is more powerful than the combination of

  Useful for, e.g., creating a pipe of natural numbers:

    Pipe.unfold 0 ~f:(fun n -> return (Some (n, n+1)))

- Add `Deferred.Map.all` similar to `Deferred.List.all`.

  This does what you would expect:

    val all
      :  ('a, 'b Deferred.t, 'cmp) Map.t
      -> ('a, 'b, 'cmp) Map.t Deferred.t

- Added some simple functions that seem missing from `Deferred` and `Ivar`.

    val Ivar.peek : 'a t -> 'a option
    val Ivar.value_exn : 'a t -> 'a
    val Deferred.value_exn : 'a t -> 'a

- Add `Monitor.catch_error`, which provides error handling for
  processes/subsystems intended to run forever.

- Added to the Async scheduler a configurable:

    min_inter_cycle_timeout : Time_ns.Span.t

  When scheduler calls epoll(), it uses a timeout of at least
  `min_inter_cycle_timeout`.

  `min_inter_cycle_timeout` can be configured via `ASYNC_CONFIG` or via

    val Scheduler.set_min_inter_cycle_timeout : Time_ns.Span.t -> unit


  This allows one to tweak the scheduler to be more fair w.r.t. threads,
  e.g. with:

    Scheduler.set_min_inter_cycle_timeout <- Time_ns.Span.of_us 1.;

- Optimized `Scheduler.schedule'` to avoid a closure allocation.

- Removed `Monitor.kill`, which was unused internally. This removes the
  `kill_index` field from `Execution_context.t`, which saves us a word
  everytime we allocate or store an execution context.

- Assert that `Deferred.forever` never returns statically, rather than dynamically.

- Changed `Async.Std` to not include `Deferred.Monad_syntax`, so that
  one must explicitly opt in (via `open Deferred.Monad_syntax`) to use
  `let%bind` syntax with Async.

- Add `Pipe.to_sequence`


- Make `Stream.closed s` return immediately when `s` is already closed.

  Currently the following property holds:

     for any s, Deferred.peek (Stream.closed s) = None

- For `Pipe` functions that deal with batches of elements in a queue,
  added an optional argument:

    ?max_queue_length : int  (** default is `Int.max_value` *)

  This limits the size of the queue that is used, which can improve
  Async fairness.

  Affected functions are:

    filter_map
    filter_map'
    fold'
    iter'
    map'
    read'
    read_now'
    transfer'
    transfer_id

  This also obviates `read_at_most` and `read_now_at_most`, which we
  will deprecate in a later release.

  Removed a couple helper types, `iter` and `fold`, that had been used
  to express commonality among functions, but were becoming unwieldy due
  to differences.

- Changed `Pipe`'s default `max_queue_length` from `Int.max_value` to
  100.

- Added to `Pipe.iter_without_pushback` an optional argument:

    ?max_iterations_per_job : int  (** default is `Int.max_value` *)

  `iter_without_pushback` will not make more than
  `max_iterations_per_job` calls to `f` in a single Async_job; this can
  be used to increase Async-scheduling fairness.

- Added `Pipe.write_if_open` which does exactly what it says.  This is a
  common pattern.  Also added a pushback-oblivious variant
  `write_without_pushback_if_open`.

  Call sites for these two new functions were introduced wherever I
  found that doing so would not introduce any side effects (even
  counting allocation) in the case of a closed pipe.

## 113.00.00

- Switched `Lazy_deferred` to use `Or_error.t` rather than `('a, exn) Result.t`.

    Note: There is difference in the `run` argument between `Monitor.try_with`
    and `Monitor.try_with_or_error`.  In this module, the function is called
    already in a closure inside a bind, so that difference is acceptable.

- Made `Deferred` match `Invariant.S1`.

- Improved `Async_kernel.Scheduler.run_cycles_until_no_jobs_remain` to use
  `Timing_wheel.fire_past_alarms` rather than sleeping.

- Added `Quickcheck` module.

- Reworked the initialization of `Monitor.try_with_ignored_exn_handling` to log
  exceptions using `Async.Log` so that it doesn't rely on top-level effects,
  which may not happen without packed libraries.

## 112.35.00

- Added `Clock.Event.run_at` and `run_after`.
- Eliminated a space leak in `Clock.with_timeout`.

    Previously `Clock.with_timeout span d` created a deferred that
    waited for `span` even if `d` (and hence `with_timeout`) became
    determined sooner.   Now, `with_timeout` aborts the clock alarm if
    `d` becomes determined, which saves space in Async's timing wheel.

- Added `Clock.Event.fired`, and removed the `fired` value that was
  returned by `at` and `after`.

        val fired : t -> [ `Happened | `Aborted ] Deferred.t

- Added `Clock.Event.reschedule_{at,after}`.
- Fixed the space leak that caused nested `Monitor.try_with` to use
  linear space rather than constant space.

    Changed `Monitor.try_with_ignored_exn_handling` so that with
    ``Eprintf` or `` `Run f ``, the error processing runs in
    `Monitor.main` rather than in the monitor that called
    `Monitor.try_with`.  This avoids space leaks due to chains of
    monitors, e.g.:

        open! Core.Std
        open! Async.Std

        let () =
          Monitor.try_with_ignored_exn_handling := `Run ignore;
          let num_monitors = 10_000_000 in
          let num_remaining = ref num_monitors in
          let rec loop n : unit =
            if n > 0
            then
              upon
                (Monitor.try_with
                  (fun () ->
                      loop (n - 1);
                      return ()))
                (function
                  | Error _ -> assert false
                  | Ok () ->
                    decr num_remaining;
                    if !num_remaining = 0 then shutdown 0)
          in
          loop num_monitors;
          never_returns (Scheduler.go ());
        ;;

    Added a unit test to detect if nested monitors leak space.

- Removed `Lazy_deferred.follow`, which is not used in the tree anymore.

    Removing this function allows `Lazy_deferred.t` to be implemented as:

        type 'a t = 'a Deferred.t Lazy.t

    although that's not done yet.

- Added hooks to `Async_kernel.Scheduler` for `js_of_ocaml`.

    This hook aims to be called every time one add a job to the
    scheduler (enqueue + timing_wheel).

- Made `Async_kernel` not depend on thread.
- Added `Deferred.Memo`, which wraps functions in `Core.Memo` to
  correctly handle asynchronous exceptions.
- Renamed `Pipe` and `Thread_safe_pipe` functions that clear their input
  queue so that their name starts with `transfer_in`, to make it clearer
  that they side effect their input.

    | old name                  | new name                       |
    | ------------------------- | ------------------------------ |
    | `write'`                  | `transfer_in`                  |
    | `write_without_pushback'` | `transfer_in_without_pushback` |

- Added `Pipe.init_reader`, symmetric to `Pipe.init`.

        val init        : ('a Writer.t -> unit Deferred.t) -> 'a Reader.t
        val init_reader : ('a Reader.t -> unit Deferred.t) -> 'a Writer.t

- Changed `Async_kernel.Job_queue.run_jobs` to call `Exn.backtrace`
  immediately after catching an unhandled exception

    There should be no change in behavior.  This change was to make it
    more clear that there is no intervening code that interferes with
    the global backtrace state.

- Made `Deferred` functions that take an argument
  `?how:[ `Parallel | `Sequential ]` accept
  `` `Max_concurrent_jobs of int``, which operates in a sequence in
  parallel, limited via a throttle.

- Made `Deferred.Or_error` match `Applicative.S`.
- Fixed `Scheduler.run_cycles_until_no_jobs_remain` so that it continues
  running if one has done `Scheduler.yield`.
- Split the implementation of `Deferred` into a number of files, to
  solve some problems with circularities.

    Split into:

    - deferred.ml
    - deferred_sequence.ml
    - deferred_list.ml
    - deferred_array.ml
    - deferred_queue.ml
    - deferred_map.ml
    - deferred_result.ml
    - deferred_option.ml

    For a sequence of multiple modules used to construct a module,
    switched from the `Raw_*` prefix convention to the numeric suffix
    convention.  E.g. we now have `Deferred0`, `Deferred1`, `Deferred`.

## 112.24.00

- Now depends on `Core_kernel` instead of `Core`.

  `Async_kernel.Clock` uses `Core_kernel.Time_ns` and
  `Core_kernel.Timing_wheel_ns` rather than `Core.Time` and
  `Core.Timing_wheel_float`.

- Added `Async_kernel.Types` module to deal with the mutual recrsion of
  `Async_kernel`'s types.

  This should help eliminate the complexity and make it easier to make changes
  without running into as many constraints due to module/type ordering.

  Merged `Types.Jobs` into `Types.Scheduler`.

- Improved the performance of `Deferred.bind`, eliminating an allocation in
  `Ivar.connect`.

- Optimized `Deferred.bind`, removing a closure allocation by inlining
  `Deferred.create`.

- Added `Pipe.interleave_pipe`, which is like `interleave`, but takes a pipe
  rather than a list.

## 112.17.00

- Fixed a space leak in `Clock.Event.abort`, making it free the job
  that was created and stored in the timing wheel.
- Moved `Scheduler.yield` from `Async_unix`.
- Fixed a bug in `Scheduler.yield_every`, so that it doesn't
  initialize the scheduler until the staged function is called.
- Added `concat_map` function to `Monad_sequence` interface.
- Added `Shutdown.shutdown_on_unhandled_exn`.
- Added some functions to `Deferred.Or_error` to parallel
  `Core.Or_error`: `errorf`, `tag`, `tag_arg`.

  ```ocaml
  val errorf : ('a, unit, string, _ t) format4 -> 'a
  val tag : 'a t -> string -> 'a t
  val tag_arg : 'a t -> string -> 'b -> ('b -> Sexp.t) -> 'a t
  ```
- Added `Gc.Alarm`, an Async-friendly wrapper around
  `Core.Gc.Expert.Alarm`.
- Removed `Gc.Expert`, whose functions are superseded by
  Async-friendly functions in `Gc` proper.
- Added `Pipe.read_now_at_most`.
- Changed `Pipe.merge` to check whether its output is closed, and if
  so, stop rather than write to it (which raised).

  Also, made `Pipe.merge` close its inputs whenever its output is
  closed.
- Changed `Clock.at` to return `Deferred.unit` if it is supplied a
  time in the past.

  Previously, it would create an empty ivar and a job to fill it that
  would run in the next cycle.
- Changed `Clock.Event.status` to return ````Will_happen_at of
  Time.t``` rather than ````Waiting```, if applicable.
- Added `Ivar.create_full`.
- Moved the use of `Linux_ext` to `Async_unix`.

  This is one of the necessary steps in making `Async_kernel` depend
  on `Core_kernel` rather than `Core`.

## 112.06.00

- Added `Deferred.Sequence` module, analogous to `Deferred.List` but for
  `Core_kernel.Std.Sequence`.
- Modernized code style.

## 112.01.00

- Optimized `Monitor.try_with ~run:\`Now f` to return a determined
  deferred if `f ()` returns a determined deferred.

  Previously, `Monitor.try_with ~run:\`Now f` always introduced a
  `Deferred.map`, which made it impossible to do some optimizations
  that bypass the scheduler overhead.
- Added an `ASYNC_CONFIG` field that causes the program to dump core
  if Async jobs are delayed too much.

  The new field is `dump_core_on_job_delay`.
- Switched `Async_kernel` from using `Core.Sys` to `Pervasives.Sys`
  eliminating one of the dependencies on `Core`.

## 111.25.00

- Fixed `Clock.run_at_intervals` to make the initial callback at an
  interval multiple.

  Previously, if `start` was in the past, `f` would run immediately
  rather than waiting for an interval of the form `start + i * span`.
  Now it always waits for an interval, even the first time it runs.

## 111.17.00

- Renamed `Monitor.errors` to `Monitor.detach_and_get_error_stream`
  and `Monitor.error` as `Monitor.get_next_error`.

  The use of `detach` in the name is intended to make clear that
  errors do not propagate to the parent.

  Added several other non-stream =Monitor= functions to capture common
  use cases of `Monitor.detach_and_get_error_stream`:

  ```ocaml
  detach_and_get_next_error
  detach_and_iter_errors
  detach
  ```

## 111.11.00

- Added `Clock.run_at_intervals`, which runs a job at regular
  intervals.

## 111.08.00

- Changed low-level error messages to use `Sexp.to_string_hum` rather
  than `to_string_mach`.

## 111.06.00

- Improved the performance of `Pipe.filter_map` by using batching.

## 110.01.00

- Added `Deferred.Result.map_error`.

## 109.60.00

- Changed the scheduler to clear a job from its queue when it runs the
  job, eliminating a performance regression from 109.57.

    Clearing avoids spurious promotion of what would otherwise be dead
    data associated with already-executed jobs.

## 109.58.00

- Renamed the `Async_core` library as `Async_kernel`, to parallel
  `Core_kernel`.

  Someday `Async_core` will depend only on `Core_kernel`, but not yet.
- Added a thread-safe queue of "external actions" that is checked
  after each job.
- Fixed a race condition in `Clock.Event.abort`.

  Here is the race condition:

  * `Clock.Event.at` adds an alarm, its value is a job (let's call it
    job1) with this run function:

    ```ocaml
    let fire () =
      t := Happened;
      Ivar.fill ready `Happened;
    ```
  * later a job (let's call it job2) aborting the clock event is
    queued in the async scheduler
  * in the same cycle, the `Timing_wheel.advance_clock` fires the
    alarm and job1 scheduled
  * at this point:
      + job1 and job2 are still pending
      + the alarm was removed so it is invalid
      + the clock event is still in the state `Waiting`
  * job2 is executed before job1: the clock event is still in the
    `Waiting` state, so the abort tries to remove the alarm from the
    timing wheel: CRASH

    The bugfix is for `Clock.Event.abort` to check if the alarm has
    already been removed from the timing wheel and if so, don't remove
    it again.
- Changed `Monitor.try_with` when run with ``~rest:`Ignore``, the
  default, so that the created monitor is detached from the monitor
  tree.

  The detached monitor has no parent, rather than being a child of the
  current monitor.  This will eliminate recently observed space leaks
  in `Sequencer_table` and `Throttle`, like:

  ```ocaml
  let leak () =
    let seq = Throttle.Sequencer.create () in
    let rec loop n =
      Throttle.enqueue seq (fun () ->
        loop (n + 1);
        Deferred.unit
      )
      |> don't_wait_for
    in
    loop 0
  ```
- Changed Async's scheduler to pool jobs rather than heap allocate
  them, decreasing the cost of a job by 30-40%.

    Changed the main scheduler queue of jobs to be an `Obj_array.t` that
    is essentially a specialized `Flat_queue` (the specialization was
    necessary for speed).

    Also, cleaned up the scheduler run-job loop.

    With these changes, the cost of a simple job decreases significantly
    (30-40%), across a range of live data sizes.  Here are the
    nanoseconds-per-job numbers for a microbenchmark with the old and
    new approaches.

    | num live jobs | old ns/job | new ns/job |
    |---------------|------------|------------|
    |             1 |         74 |         53 |
    |             2 |         75 |         47 |
    |             4 |         76 |         41 |
    |             8 |         63 |         39 |
    |            16 |         62 |         38 |
    |            32 |         61 |         37 |
    |            64 |         61 |         37 |
    |           128 |         60 |         37 |
    |           256 |         60 |         38 |
    |           512 |         60 |         38 |
    |          1024 |         60 |         39 |
    |          2048 |         61 |         40 |
    |          4096 |         67 |         41 |
    |          8192 |         65 |         45 |
    |         16384 |         75 |         56 |
    |         32768 |        115 |         67 |
    |         65536 |        171 |        108 |
    |        131072 |        255 |        158 |
    |        262144 |        191 |        130 |
    |        524288 |        216 |        139 |
    |       1048576 |        238 |        152 |

    See async/bench/nanos\_per\_job.ml for the benchmark.
- Removed `debug_space_leaks` from Async's internals.  It hadn't been
  used in years.

## 109.52.00

- Changed `Pipe.iter_without_pushback` to never call `f` after
  `Pipe.close_read` has been called.

    The new behavior is like `Pipe.iter`.

- Changed the implementation of `Pipe.fold_gen` and `Pipe.transfer_gen`
  to be analogous to `Pipe.iter_without_pushback`, and in particular to
  process as many elements as possible before calling `values_available`.

## 109.47.00

- Fix a bug introduced in `Monitor.error` in 109.28, in which the error wasn't seen unless someone is listening to the monitor.

## 109.45.00

- Removed internal `Backpatched` module.

    Async used to use this module, but it doesn't anymore.

## 109.44.00

- Documented that `Throttle.enqueue t f` never runs `f` immediately,
  and added unit tests.

## 109.42.00

- In `ASYNC_CONFIG`, replaced `alarm_precision` and `timing_wheel_level_bits` with `timing_wheel_config`.

    This parallels the new `Timing_wheel.Config` module.

## 109.35.00

- Added new configuration options for Async, `max_inter_cycle_timeout`
  and `max_num_jobs_per_priority_per_cycle`.

    ```ocaml
    val max_inter_cycle_timeout : Time.Span.t
    val max_num_jobs_per_priority_per_cycle : int
    ```

    These are configurable as usual via `ASYNC_CONFIG`.

- Added an `ASYNC_CONFIG` option to debug the `Shutdown` module.
- Added `find` and `find_map` to `Deferred.Monad_sequence`.

## 109.34.00

- Added a function to `Pipe` that merges a list of sorted pipes

    `val Pipe.merge : 'a Reader.t list -> cmp:('a -> 'a -> int) -> 'a Reader.t`

- Improved the performance of `Ivar.squash` by removing the allocation of the list of ivars.

    Instead, `squash` does one loop to find the end of the chain and a
    second loop to set all the indirections.

- Changed `Scheduler.run_cycles_until_no_jobs_remain` to raise an exception if there is an unhandled exception when it finishes.

## 109.32.00

- Improved the batching of `Pipe.fold` and other `Pipe` functions that handle batches.

  Previously, such functions used a loop with `Pipe.read`.  This
  didn't batch as well as it might.  If values were put in the pipe
  after the `read` becomes determined but before the values are
  handled, then they wouldn't be handled until the next batch.  Now,
  batching functions use `values_available` and then pull elements out
  of the pipe synchronously after waking up.  This makes the batch as
  large as possible.

## 109.30.00

- Added function `Throttle.kill`.

  `Throttle.kill` aborts all jobs that have been enqueued but not
  started, and immediately aborts all jobs subsequently enqueued.

  Split out `Throttle` debugging and unit-testing code into their own
  modules.

- Changed the semantics of `Throttle.enqueue` on a dead throttle so that
  the exception is sent to the monitor rather than raised synchronously.

  This gives more uniform treatment to the race between enqueueing a
  job and an already running job raising.  Now the enqueued job is
  always aborted, whether enqueued before or after the raise.

- Added an ASYNC_CONFIG option to print debug messages when `Monitor.send_exn` is called.

  This is useful when one is debugging an application in which an
  exception is being unexpectedly swallowed.

- Allow one to dynamically configure the behavior of =Monitor.try_with=.

  This is to allow experimentation with different handling of
  asynchronous exceptions after `Monitor.try_with` has become
  determined.

## 109.28.00

- Eliminated a messy dependency cycle in `async_core`, so that
  `Monitor.t` no longer contains a `Tail.t`.

  `async_core` was messy because of cycle between the following types:

  ```
  Execution_context
  --> Scheduler
  --> Ivar
  --> Deferred
  --> Tail
  --> Monitor
  ```

  This messiness caused the need for the `Raw` signature, for the
  various `Scheduler_dependent` functors, for making various types
  polymorphic in `execution_context`, and then instantiating the
  polymorphism later.

  The cause of the problem was that `Monitor` contained a `Tail`.  We
  eliminated that, so that there is no longer a cycle, and defined
  things in order:

  ```
  Monitor
  Execution_context
  Job
  Scheduler
  Ivar
  Deferred
  Tail
  Stream
  ```

  Replaced the `errors` tail from the monitor type:

  ```ocaml
  errors : (exn, 'execution_context) Raw_tail.t;
  ```

  with a list of handlers:

  ```ocaml
  mutable error_handlers : (exn -> unit) list;
  ```

  Cleaned up all the messiness caused by the cycle -- eliminated the
  `Raw` signature, the `Scheduler_dependent` functors, and the
  unnecessary polymorphism.

  Cleaned up the long standing annoyance with `Async.Stream`, in which
  we couldn't expose the `next` sum type and people had to use
  annoying `Stream.of_raw` calls.  `Stream.of_raw` is now gone.

## 109.27.00

- Fixed `Monitor.catch_stream` to prevent missing a synchronous
  exception.

## 109.24.00

- Reworked the `Throttle` module.

  Made both `Throttle.t` and `Throttle.Sequencer.t` instances of the
  same type, using a phantom type to distinguish them.  Removed all
  `Throttle.Sequencer` functions -- one can now use the `Throttle`
  functions directly.

  Added new functions:

  ```ocaml
  (*** [max_concurrent_jobs t] returns the maximum number of jobs that [t] will run
       concurrently. *)
  val max_concurrent_jobs : (_, _) t_ -> int

  (*** [num_jobs_running t] returns the number of jobs that [t] is currently running.  It
       is guaranteed that if [num_jobs_running t < max_concurrent_jobs t] then
       [num_jobs_waiting_to_start t = 0].  That is, the throttle always uses its maximum
       concurrency if possible. *)
  val num_jobs_running : (_, _) t_ -> int

  (*** [num_jobs_waiting_to_start t] returns the number of jobs that have been [enqueue]d but
       have not yet started. *)
  val num_jobs_waiting_to_start : (_ , _) t_ -> int

  (*** [capacity_available t] becomes determined the next time that [t] has fewer than
       [max_concurrent_jobs t] running, and hence an [enqueue]d job would start
       immediately. *)
  val capacity_available : (_, _) t_ -> unit Deferred.t
  ```

  Replaced the `Pipe` used inside a `Throttle` with a `Queue`, and
  simplified the implementation.  This fixed a bug in
  `num_jobs_waiting_to_start`, which could have missed a job that was
  not in the pipe but had not started.

## 109.20.00

- Added the ability for a `Throttle` to have resources that are exclusively available to running jobs.

## 109.13.00

- Fixed `Pipe.iter`'s handling of a closed pipe.

  Fixed the handling by `Pipe.iter` and related foldy functions that
  handle one element at a time, which behaved surprisingly with a pipe
  whose read end has been closed.  These functions had worked by
  reading a queue as a batch and then applying the user function to
  each queue element.  But if the pipe's read end is closed during the
  processing of one queue element, no subsequent element should be
  processed.  Prior to this fix, the `iter` didn't notice the pipe was
  closed for read until it went to read the next batch.
- Renamed `Pipe.read_one` as `Pipe.read_one`', and added
  `Pipe.read_one` that reads a single element.

## 109.11.00

- Extended `Deferred.Or_error` to parallel almost all of the
  `Core.Or_error` interface.
- Improved the performance of `Clock.at`, and added a more efficient
  version, `Clock.run_at`.

  Reworked the async heap of clock alarms to use async jobs as alarms.

  Reworked `Clock.at` to use this and to not use abortable events,
  which is a performance improvement.

  Added a more efficient version of `Clock.at`, for the common
  situation when one doesn't need a deferred.

  ```ocaml
  (*** [run_at time ~f] is a more efficient version of [at time >>> f]. *)
  val run_at : Time.t -> f:(unit -> unit) -> unit
  ```

## 109.09.00

- Fixed bug in `Async.Throttle`, in which jobs weren't started in order.
