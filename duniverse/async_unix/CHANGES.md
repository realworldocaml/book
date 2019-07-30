## 113.43.00

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

- Switched `Async_unix` from `>>=` and `>>|` to `let%bind` and
  `let%map`, where appropriate.

- Fixed a race in `Thread_safe.block_on_async` that allowed Async code
  to run without the scheduler starting.  There is important
  initialization that happens when the scheduler starts, like handling
  SIGPIPE.

- Added `Scheduler.make_async_unusable`.

- Fixed a bug in `Thread_safe.block_on_async_exn`.

  The bug was that the execution context wasn't restored to the main
  execution context in the main thread after returning from the call to
  `Thread_safe.block_on_async`.  This leads to main-thread code running
  in some random execution context.  If that code then raises, all hell
  can break loose due to exceptions being sent to random monitors.

- Make the Check_buffer_age edge-triggered; the exception is send to
  the writer's monitor only when the state changes from not-backed-up to
  backed-up.

- Currently whether `phys_equal Writer.stdout Writer.stderr` when
  running inline tests depends on a few factors:

  - whether the  test is run by jenga or from the terminal
  - when was Writer.{stdout,stderr} first forced: inside a test or not

  This makes tests flaky and is confusing for users. This feature makes
  sure that we always have the equality of writers when running inline
  tests.

- Changed `Async.Process.collect_output_and_wait` to *not* force close
  on the process' stdin.  Maybe the process is just slow to consume its
  input.  An external process working for 5s is not uncommon or
  unreasonable at all and should be allowed.

- Added convenience functions for reading and writing bin_prot via
  Reader/Writer:

    Writer.save_bin_prot
    Reader.load_bin_prot
    Reader.load_bin_prot_exn

- Fixed a bug in Async's `Shutdown.shutdown`, which did not run
  at-shutdown handlers in a try-with, and behaved badly (did not
  shutdown) if an at-shutdown handler raised.

- Moves the `Time_source` unit tests from
  `async_unix/src/time_source_tests.ml` to
  `async/test/test_time_source.ml` converting them to expect tests.

- Make `write_gen` fail "gracefully" when `~length` or `~blit_to_bigstring` raise.

- Make `write_gen` fail "gracefully" when `~length` or `~blit_to_bigstring` raise.

- Fix a problem with `Writer.write_gen`.

  Context: The `write_gen` function creates generic writing functions based on a
  `blit_to_bigstring` function.  The blit function writes arbitrary portions of the input's
  binary representation into a bigstring.

  Problem: The current documentation claims that "If it is difficult to write only part of a
  value, one can choose to not support `?pos` and `?len`."  However, even if `~pos` and
  `~len` are never supplied, the implementation may choose to split the given value between
  two write buffers, thus requiring the user of `write_gen` to provide a blit function that
  writes only part of a value.

  Solution: Add a function `write_gen_whole` that never splits a value, at the cost of
  potentially wasting write buffer space.  Correct the documentation to refer users to the
  new function, instead of the suggestion "to not support `?pos` and `?len`."

- Reordered labeled arguments in some `Writer` code.

- We occasionally need to pass stdin to a process, and right now it's
  verbose to do because all of a sudden you can't use `run_lines_exn`
  and friends.  Passing all of stdin upfront is oftentimes enough.

- writer0 allocates a large chunk of memory at top level as a buffer for `write_sexp_internal`.
  Make this lazy for callers that don't writer sexps, and reduce the initial size of the buffer.

- Added module `Async.Std.Require_explicit_time_source`, so that one can
  require code to be explicit about what time source is used and not
  unintentionally use the wall clock.  The idiom is to do:

    open! Require_explicit_time_source

  or, in an import.ml:

    include Require_explicit_time_source

## 113.33.00

- expose a function to get the number of jobs that async has run since Scheduler.go

- In `Log` change `sexp` to be immediate rather than lazy.

  This changes the behavior the `sexp` function.  I've gone through the entire
  tree rewriting `sexp` to `create_s`, attempting to follow these guidelines:

  - If the logging is clearly intended to always happen (error logging,
  transaction log logging, etc.) I translated the call unguarded.

  - If the logging was already guarded with an if statetment I translated
  the call unguarded.

  - Otherwise, I added a call to `Log.would_log` as a guarding if.  This duplicates
  the behavior of `sexp`, which would avoid the sexp conversion if the log
  message was never going to be logged.


- `Writer.with_file_atomic` checks if destination file exists and then
  stats it to get existing permissions. File can go away before the call
  to stat, which would make the `with_file_atomic` to error out, which
  seems harsh. I think it is better to continue on in this case.
  Besides, `Async.file_exists` is essentially just another stat, so we might as
  well just do the stat without checking whether file exists or not.

- Moved various `let%test`'s in `Async_unix` library into their own
  module.  I'd like the `let%test`'s to be in files that depend on
  `Std`, so that they can pick up a top-level side effect that `Std`
  does, specifically the assignment of `Monitor.try_with_log_exn`.  The
  child feature of this feature makes the change to do that assignment.

- Changed the `Async_unix` library's assignment of
  `Monitor.try_with_log_exn` to be a top-level side effect that happens
  if `Async_unix.Std` is used.  This fixes a bug in which
  `Thread_safe.block_on_async` (and other bits of `Async_unix`) failed to
  to do the assignment.  The assignment was part of `Scheduler.go`,
  which isn't called by `Thread_safe.block_on_async`.  This in turn
  cause programs that use `Thread_safe.block_on_async` and do not use
  `Scheduler.go`, like inline tests, to raise if they needed to report
  an exception raised to `Monitor.try_with` that already returned.

- Deprecate the (blocking) `Sexp.save_*` functions in `Async_unix.Std`.

  Please do not switch from `Sexp.save*` to `Writer.save_sexp*` in this
  feature -- this feature is just for the deprecation.  You can do the
  switch in a subfeature or later feature.

  A note aboute `Core_extended.Sexp`
  ----------------------------------

  This feature causes surprising behavior in `Core_extended.Sexp`, where
  opening `Async.Std` undoes any previous open of `Core_extended.Std`:

       open Core.Std
       open Core_extended.Std

       type t = int Sexp.Comprehension.t `@@deriving sexp`

       open Async.Std

       type u = int Sexp.Comprehension.t `@@deriving sexp`

  The type `t` is fine but `u` fails to compile because at that point
  the module Sexp has no submodule Comprehension.

  But we already have the same problem with module `Unix` if you open
  `Async.Std` after `Core_extended.Std`, so I left the `Sexp` module as is.

- Added `Async.Unix.Stats.compare`.

- Changed Async's logging of exceptions raised to a `Monitor.try_with`
  that already returned to use `Log.Global.sexp` rather than
  `Log.global.error`.  For logs that use single-line sexps, that makes
  them much nicer.

- Added to `Async.Scheduler` `Time_ns` analogs of `cycle_start` and
  `cycle_times`:

      val cycle_start_ns : unit -> Time_ns.t
      val cycle_times_ns : unit -> Time_ns.Span.t Stream.t

- I'm tired of having to write `>>| ok_exn` after most calls to Process, let's just make
  Process provide these functions directly.
  Also you can't write `>>| ok_exn` nicely anyway when using the let%bind and friends.

- Added a test demonstrating that `Async.Process.create` returns `Ok`
  when `exec` fails.  And updated the `create`'s documentation.

## 113.24.00


- In the periodic check for a Writer buffer have too old data,
  eliminated allocation and generally improved performance.

  This eliminated a large source of allocation in a simple TCP pingpong
  benchmark `bench/pingpong`.

- Removed allocation in the Async scheduler's code that detects the
  thread-pool being stuck.  This involved switching it to use `Time_ns`
  rather than `Time`.

  This eliminates a relatively large source of allocation in a simple
  TCP-pingpong benchmark `bench/pingpong`.

- Switched to ppx.

- Improved the Async scheduler's to allocate a `handle_fired` function
  once, rather than every time it calls `advance_clock`.

- Added `Fd_by_descr.find_exn`, to avoid the option allocated by
  `Fd_by_descr.find`.  Used it to reduce allocation in the Async
  scheduler.

- Improved `Reader.load_sexp*` functions to behave better when loading
  from a non files, e.g. a pipe.  Previously, it produced an empty error
  message because it mistakenly attempted to read the sexp a second time
  in order to determine the error position.  Now it produces a good
  error message, but without the (impossible to obtain) error position.

- In `Async_unix.Syscall`, added a zero-allocation syscall interface,
  removing sources of allocation as observed when running a simple TCP
  pingpong benchmark (found in `bench/pingpong`).

- Added

    val time_spent_waiting_for_io : unit -> Time_ns.Span.t

  to `Scheduler` which returns the amount of time that the Async scheduler has
  spent in calls to `epoll_wait` (or `select`) since the start of the program.

- Changed `In_thread.Helper_thread.create` from:

    val create : ?priority:Priority.t -> ?name:string -> unit -> t Or_error.t

  to:

    val create : ?priority:Priority.t -> ?name:string -> unit -> t Deferred.t

  Kept around the prior function, renamed as `create_now`.

  Switching `create` to return a deferred allows it to, when there are
  no available threads, wait until one becomes available.  This, in
  turn, avoids rare nondeterminstic failures in programs that make heavy
  use of the thread pool and create a helper thread, when the creation
  happens at just the wrong time, when no thread is available.

  Split out `Thread_safe_ivar` from the internals of `Thread_pool`, so
  that it can be used in other tests, and in particular in a new
  unified test added by this feature.

- Make `Unix_syscalls.Stats` bin-io-able.

- Fixed a bug in `Thread_safe.block_on_async*`, in which the execution
  context wasn't properly restored before returning.

- Add a function in `Process` that expects empty output, mirroring `Shell.run`.

- Added `Reader.read_one_iobuf_at_a_time`, which is like
  `read_one_chunk_at_a_time`, except that the user-supplied
  `handle_chunk` function receives its data in an `Iobuf.t`, and uses
  the `Iobuf` position to communicate how much data was consumed.

  This facilitates using reader in scenarios (such as with protogen)
  where `Iobuf`s are expected (and presently allocated around the
  bigstring at each call) and the calculation of consumed bytes from the
  `Iobuf` is duplicated in few places.

- `Log.message` used to always logs the message, even if its log level was too low.
  This has been fixed.

- Add writer functions to schedule an iobuf to be written out.

- Add `Unix.Inet_addr.Stable`.

- Alter `Async.Std.Socket.Address.Inet.Blocking_sexp` to expose the
  polymorphic variant functions, so that you can include it in a
  separate polymorphic variant type.

  Also, expose `Async.Std.Socket.Address.Inet.__t_of_sexp__` to give a
  deprecation message, instead of a message about the function not
  existing.

- Fixed a bug in Async's `In_thread.Helper_thread`, which wasn't
  finalizing helper threads, due to a bug in `Thread_pool`, which wasn't
  finalizing helper threads.  The fix was to move the finalization out
  of `Thread_pool`, where we don't care about it, to
  `In_thread.Helper_thread`, where we do.

  Added `Scheduler.max_num_threads : unit -> int`.

- Make `Epoll_file_descr_watcher` trigger callbacks for error conditions such as closed pipes.

  Testing
  -------
  Three new unit tests, all validating appropriate behavior in the case that a Unix pipe is
  opened, then the read end is closed after reading only part of the data sent by the write
  end.

  1. A test in writer.ml verifying that `Writer.consumer_left` is triggered.  Before the fix
     to `Epoll_file_descr_watcher`, `Writer.consumer_left` would never become determined in
     this case.

  2. A test in fd.ml verifying that `Fd.ready_to` is triggered for the writing fd.  Before
     the fix to `Epoll_file_descr_watcher`, `Fd.ready_to` would never become determined in
     this case.

  3. A test in linux_ext.ml verifying that `Epoll.wait_timeout_after` is triggered.  This
     test shows that epoll reports the ERR flag for the file descriptor in this case, and
     therefore that `Epoll_file_descr_watcher` needs to pay attention to the ERR flag.

- Added to `Writer.write_sexp` an optional `?terminate_with` argument,
  that specifies how to terminate the string representation of the sexp.
  This also makes it clear that the default behavior,
  `~terminate_with:Space_if_needed`, might append a space to the sexp
  you are outputting if its representation is not enclosed in either ()
  or "" .  `Sexp.output_hum` an `Sexp.output_mach` do not have this
  behavior, so porting non-async code to async could introduce
  unexpected differences in the output.

- Add an Async wrapper for `Core.Std.Unix.getifaddrs`.

## 113.00.00

- Made Async dump core when it reports a "bug in async scheduler".

    There is no change for toplevel unhandled user exceptions, for which Async
    does not dump core.

- Added `Dump_core_on_job_delay.dump_core` function, which exposes the
  core-dumping functionality in the C stubs for `Dump_core_on_job_delay`.

- Made `Dump_core_on_job_delay.How_to_dump` an ordinary variant and moved it
  into `Async_kernel.Config`.

- Changed `Thread_safe_pipe` functions that write to the pipe to take an
  additional argument, an `If_closed.t`, that says how to behave if the pipe is
  closed.

    The previous behavior is achieved with `~if_closed:Raise`.  One can also
    now use `~if_closed:Return` to they return a variant reporting whether the
    pipe was closed, rather than raising.

    Returning a variant allows callers to distinguish the pipe-closed case from
    other errors.  This change also allows us to do a a single acquisition of
    the Async lock, with the pipe-closed check synchronously immediately
    preceding the operation, avoiding a race.

- Added `Fd.with_file_descr_deferred_exn`.

- Improved the performance of `Clock.every`, and in particular reduced its
  allocation.

    It now allocates much less, especially with `~continue_on_error:false`.

    Handled `Clock.every`'s `~stop` argument directly using timing-wheel
    alarms, rather than using `Deferred.choose`.

    Slightly changed the behavior of

      Clock.every' f ~continue_on_error:false

    in the corner case where `f` raises but its result also becomes
    determined.  Prior to this feature, iteration would stop.  After this
    feature, iteration will continue, because `~continue_on_error:false`
    just looks at the deferred resulting from `f`.  This doesn't affect:

      Clock.every f ~continue_on_error:false

    because if `f` raises, then there is no resulting deferred.

    Benchmark:

    +----------------------------------------------------+----------+------------+----------+----------+------------+
    | Name                                               | Time/Run |    mWd/Run | mjWd/Run | Prom/Run | Percentage |
    +----------------------------------------------------+----------+------------+----------+----------+------------+
    | [clock_ns.ml:Clock.every] ~continue-on-error:false |  54.21us |     91.03w |    0.36w |    0.36w |     22.06% |
    | [clock_ns.ml:Clock.every] ~continue_on_error:true  | 245.80us | 93_208.27w |    7.31w |    7.31w |    100.00% |
    +----------------------------------------------------+----------+------------+----------+----------+------------+

- Added to `Clock.Event.t` type parameters so that one can record a value in
  the event when it happens or is aborted, and read that value via
  `Event.status`.

    type ('a, 'h) t
    val status
      : ('a, 'h) t -> [ `Aborted      of 'a
                      | `Happened     of 'h
                      | `Scheduled_at of Time.t
                      ]
    val run_at : Time.t -> ('z -> 'h) -> 'z -> (_, 'h) t
    val abort : ('a, 'h) t -> 'a -> [ `Ok
                                    | `Previously_aborted  of 'a
                                    | `Previously_happened of 'h
                                    ]

- Fixed a (never observed) race in the Async scheduler's closing of file
  descriptors.

    Previously, when the number of active system calls on an `Fd.t`
    reached zero, the scheduler would call a closure that would
    immediately schedule the `close()` system call in a thread.  It was
    possible (albeit very unlikely) that that `close()` would run before
    the scheduler got a chance to update the epoll set, violating the
    invariant that `close` is only ever called on fds not in the epoll
    set.

    Now, the scheduler enqueues an ordinary Async job to do the `close`,
    and thus the `close` cannot happen until the next cycle, after the
    scheduler has updated the epoll set.

- Changed `Reader` to treat `read` returning `EPIPE` as end-of-file rather than
  fail, to deal with OpenOnload.

    This fixes an issue where reading from a TCP connection can return an
    `EPIPE` if the tcp connection is immediately closed.  This happens
    when the application is running with onload and when the tcp
    connection is closed immediately after creation.

- Reduced allocation of the Async scheduler's `File_descr_watcher`, by using
  callbacks to handle ready file descriptors.

- Fixed `In_thread.Helper_thread.create`'s error message if there are no
  available threads in the thread pool.

    The error message is now constructed eagerly.  It had been constructed
    lazily, so by the time it was rendered, the state might have changed,
    possibly making threads available.  This leads to a
    nonsensical-looking error message that claims that there are no
    available threads, immediately followed by a list of available
    threads.

- Moved `Log` from `Async_extra` to `Async_unix`, so that the scheduler can
  refer to it.

- When `Writer.with_file_atomic` is unable to clean up its temp file, raise
  synchronously rather than asynchronously.

    This eliminates complaints about an exception being thrown after a deferred
    has been computed.

- Added `Log.rotate` to force log rotation.

    val rotate : t -> unit Deferred.t.

- Fixed `Log` rotation to correctly reset the size and number of lines.

## 112.35.00

- Made `Unix.File_kind.t` be `Comparable`, so it can be used in
  `<:test_result< >>`.
- Reduced allocation in Async's scheduler in the common path.

    The allocation was in
    `Raw_scheduler.be_the_scheduler.compute_timeout`, which was
    (statistically, based on perf) the largest single allocator in one
    of our applications.  Now, it way down the list.

    Note that the application is not a typical Async app in that it does
    not sit in epoll very much, due to the way we do low-latency I/O.
    This change will benefit everyone, but only a tiny bit.

- Added `Writer.write_bin_prot_no_size_header`.

    This is needed for Async RPC as it writes a different size on
    its own.

- Fixed a bug in `Writer.transfer`, which didn't close the pipe when the
  consumer leaves.

    Simplified the implementation of `Writer.transfer`:

    - replaced the big loop by a simple iteration function on the pipe
      that just stop without filling its ivar when it sees the iteration
      should stop for other reason that `` `Eof `` on the pipe: writer
      closed, consumer left or stop requested by the user.

    - replaced the various `choose` by a single one and deal with the
      closing reason only at this point.

- Added `Writer.close_started`, symmetric to Writer.close_finished.

## 112.24.00

- Made `Process.env` type equal `Core.Std.Unix.env` type, effectively adding the
  ``Replace_raw` variant.
- Renamed `Process.wait` as `collect_output_and_wait`, and added a `wait`
  function that is a thin wrapper around `waitpid`.

  Also renamed:

      wait_stdout       --> collect_stdout_and_wait
      wait_stdout_lines --> collect_stdout_lines_and_wait

- Added `Unix.getgrouplist`, a wrapper around the eponymous function in core
- Change the Async scheduler to run external actions immediately upon
  dequeueing them, rather than first enqueueing them in the normal job queue.

  Also, made external actions be jobs rather than closures.

- Changed `Unix.Inet_addr.of_string_or_gethostbyname` to not use a sequencer.

  We had used a sequencer to workaround bugs in winbind, which we don't
  use anymore.

  Reported on github: https://github.com/janestreet/async_unix/issues/4

## 112.17.00

- Moved `Scheduler.yield` to `Async_kernel`.
- Added `Reader.load_annotated_sexp*` functions.

  These are like the existing `Reader.load_sexp*` functions, except
  they return annotated sexps rather than sexps.  Having annotated
  sexps is useful so that one can report error positions to the user
  when processing values built by the `t_of_sexp_` functions /after/
  they return.  I.e. when there aren't syntax errors in the sexps, but
  rather semantic errors detected later.
- Removed noise and redundancy from `Reader.load_sexp` error messages.
- Added `Writer.save_sexps`, analogous to `Reader.load_sexps`.
- Made `Writer` errors raised by the background flush job include the
  entire `Writer.t`, rather than just the `Fd.t`.
- Added to `Writer.transfer` an optional argument to limit the number
  of values read at once from the pipe.

  The old behavior is to have no limit and remains the default.
- Added to `Writer` some missing checks for functions that should
  ensure the input writer isn't closed.
- Changed `Scheduler.run_cycles_until_no_jobs_remain` to pause so that
  alarms scheduled to fire in the past actually fire.

  This is necessary because of the timing-wheel, which doesn't
  guarantee to fire an event until alarm-precision after it was
  scheduled.

  Without this change, some tests unexpectedly fail, due to jobs not
  running that should have.

## 112.06.00

- Added `Writer.behave_nicely_in_pipeline`, which makes a program behave
  nicely when used in a shell pipeline where the consumer goes away.
- Modernized code style.
- Removed spurious overrides in `Signal`: `set` and `signal`.

    These overrides are no longer necessary because the functions moved
    from `Core.Signal` to `Core.Signal.Expert`.

- Moved `async_extra`'s `Udp.bind_to_interface_exn` to `Unix`.

## 112.01.00

- Changed `Writer.transfer write pipe` to close `pipe` when the
  `writer`, is closed.

  Previously, `Writer.transfer` did not close the pipe when the
  underlying writer is closed.  This was strange because:

  1. Callers would have to consistently check for the writer being
     closed and close the `Pipe.Reader`t= themselves
  2. The analogous function `Pipe.transfer` closes the reader on
     similar circumstances.

  The absence of the close was noticed as a bug in `Rpc`, which
  assumed that `Writer.transfer` did the close.
- Fixed a bug in `Scheduler.yield` that caused it to pause for 50ms if
  there is no other pending work and no I/O.
- Exposed type equivalence between `Unix.Passwd.t` and
  `Core.Std.Unix.Passwd.t`.
- Changed `Writer.write_bin_prot` to use the new
  `Bigstring.write_bin_prot`.

## 111.28.00

- Added `Shutdown.set_default_force`, which allows one to change the
  default `force` value used by `shutdown`.

    This is useful for applications that call `shutdown` indirectly.

        val set_default_force : (unit -> unit Deferred.t) -> unit

## 111.25.00

- Added `Unix.Addr_info` and `Name_info`, which wrap `getaddrinfo` and
  `getnameinfo`.
- Improved the scheduler's error message when the thread pool is
  stuck.

## 111.21.00

- Added `Process.wait_stdout` and `wait_stdout_lines`, which are like
  `run` and `run_lines`, but take a `Process.t`.

## 111.13.00

- Added `Scheduler.yield_every`, which returns a function that calls
  `Scheduler.yield` every n-th call.

    This is useful in circumstances where you don't have strict control
    over where a deferred is examined, as in a `Deferred.List.iter`.

## 111.11.00

- Added `val Scheduler.yield : unit -> unit Deferred.t`, which becomes
  determined after the current cycle finishes.
- Improved the behavior of the scheduler's thread pool when
  `Thread.create` raises.

  With this improvement, when the thread pool is unable to create a
  thread, it presses on with the threads it has rather than raise.
  Subsequent attempts to add work to the thread pool will cause the
  pool to attempt to create the thread, as long as enough time has
  passed since the most recent thread-creation failure.

  Before this change, the thread pool wouldn't handle a
  `Thread.create` exception, and the exception would get raised to
  whatever code happened to be calling the `Thread_pool` function that
  tried to create a thread, e.g. `Thread_pool.add_work`.  This caused
  `In_thread.run` to unexpectedly raise, and in turn
  `In_thread.syscall` to unexpectedly raise, leading to:

  ```
  "Fd.syscall_in_thread bug -- should be impossible"
  ```

  Also, changed `should be impossible` text to `please report`, since
  there may be other lurking rare exceptions that `In_thread.syscall`
  can raise, and we'd like to hear about them.

  We rely on thread-pool-stuck detection to report problems where the
  inability to create threads causes the inability of the thread pool
  to make progress.  A tweak was needed to make that work -- now the
  thread-pool-stuck warning is based on whether the thread pool has
  unstarted work, rather than on whether the thread pool has an
  "available thread".  The latter would no longer work, since it is
  now possible for the thread pool to have unstarted work and to
  appear to have an available thread, i.e. `num_threads <
  max_num_threads`.

## 111.08.00

- Added `Sys.when_file_changes : string -> Time.t Pipe.Reader.t`.
- Added `Time.now ()` to some error messages.

## 111.06.00

- In the `Busy_pollers.t` record, made the `kernel_scheduler` field be
  `sexp_opaque`.

  Did this so that one doesn't get two copies of the kernel scheduler
  in sexps of the scheduler, which already has its own
  `kernel_scheduler` field.

## 111.03.00

- Improved `Socket.accept` to abort and return `` `Socket_closed`` when
  the file descriptor underlying the socket is closed.
- Added to `Socket.bind` a `?reuseaddr:bool` argument, preserving the
  default as `true`.
- Added `Fd.close_started`, which becomes determined when `close` is
  called on an `Fd.t`.

## 109.60.00

- Fixed a bug in detection of the thread pool being stuck that could
  overstate the amount of time the pool was stuck.

    It had been incorrectly reporting the duration of the thread pool
    being stuck if the pool had no work in it and then got enough jobs
    to be stuck.  In that situation, the duration included the time span
    before the pool got stuck.  If the pool had been idle long enough,
    this could even spuriously abort the program.

## 109.58.00

- Improved fairness of the async scheduler with respect to external
  threads, including I/O done in external threads.

  The change is to add a thread-safe queue of "external actions" that
  is checked after each job.

  Previously, when a job given to `In_thread.run` finished,
  `In_thread.run` would take the async lock, fill the result ivar and
  run a cycle.  The problem is that in some situations, due to poor OS
  scheduling, the helper thread never had a chance to grab the lock.
  Now, `In_thread.run` tries to take the lock:

  - if it can it does as before
  - if it can't it enqueues a thunk in the external actions queue and
    wakes up the scheduler

  With this change, the helper thread doing an `In_thread.run` will
  always quickly finish once the work is done, and the async scheduler
  will fill in the result ivar as soon as the current job finishes.
- Fixed `Epoll_file_descr_watcher.invariant` to deal with the timerfd,
  which has the edge-triggered flag set.
- Added `Writer.write_gen`, a generic functor for blitting directly to
  a writer's buffer.

## 109.55.00

- Fixed `Scheduler.is_running` to not initialize the scheduler.
- Added `Writer.make_write`, which is a general function for blitting
  directly to a writer's buffer.
- Added `Writer.transfer'`, which is like `Writer.transfer` but allows
  async actions in the callback.

  This was requested in pull request #1.
- Added `Writer.write_iobuf`, which blits the contents of an iobuf
  directly to a writer's buffer.

## 109.53.00

- Changed the scheduler to calibrate `Time_stamp_counter` every
  second.
- Improved error messages in the scheduler when `epoll` functions
  raise.
- Made `Scheduler.reset_in_forked_process` close the `epoll` file
  descriptor.

## 109.52.00

- Fixed a bug in `Unix.mkstemp`, which had a race because it used
  `Fd.create` in a thread.

  This bug affected `Writer.with_file_atomic`, `save`, `save_lines`,
  and `save_sexp`, and could cause corruption of the async scheduler
  data structures.
- Changed async to never do `set_nonblock` on `std{in,out,err}`, which
  allows `Core` I/O to use the standard file descriptors
  simultaneously with async.

  Before this change, the Core I/O libraries could (and sometimes did)
  fail due to `Sys_blocked_io`.
- Changed `Pipe.iter_without_pushback` to never call `f` after
  `Pipe.close_read` has been called.

  The new behavior is like `Pipe.iter`.

  Changed the implementation of `Pipe.fold_gen` and `Pipe.transfer_gen`
  to be analogous to `Pipe.iter_without_pushback`, and in particular to
  process as many elements as possible before calling `values_available`.
- Added `?expand_macros:bool` argument to `Reader.load_sexp*`
  functions, to support the new `Sexplib` macros.
- Added an optional argument to `Process.run` to accept nonzero exits
  as successful runs.

## 109.47.00

- Added `Socket.Address.Inet.to_host_and_port`.
- Changed `Fd_by_descr` so that it actually calls `Bounded_int_table.invariant`.

## 109.45.00

- Added `Fd.every_ready_to` `Fd.interruptible_every_ready_to` which
  register a callback to be called every time the fd becomes ready.

  These can significantly reduce allocation.
- Renamed `Fd.ready_to_interruptible` as `Fd.interruptible_ready_to`.
- Changed `Fd.ready_fold` to use `Fd.interruptible_ready_to`, to
  improve its performance.

## 109.42.00

- Added `Reader.drain`.

  ```ocaml
  val drain : t -> unit Deferred.t
  ```

- Added `Writer.with_close`.

  ```ocaml
  val with_close : t -> f:(unit -> 'a Deferred.t) -> 'a Deferred.t
  ```

## 109.41.00

- Changed `Reader` to treat more `errno` responses to a `read()` system call as `EOF` rather than raise.

  The following are now treated as `EOF`.

  ```
  ENETDOWN
  ENETRESET
  ENETUNREACH
  ETIMEDOUT
  ```

- Improved the error message that async prints when the thread pool is stuck, including the time of the error.

## 109.40.00

- Added value `Socket.Type.unix_dgram`, which represents a UNIX domain
  datagram socket.
- Added UDP socket functionality: `Socket.Opt.mcast_{loop,ttl}` and
  `Socket.mcast_{join,leave}`.
- Improved `Fd.ready_fold` to accept `?stop:(unit Deferred.t)` rather
  than `?stop:('a -> bool)`.

## 109.39.00

- Added "thread-local" storage, `Scheduler.{find,with}_local`, for
 `LWT` emulation.

  ```ocaml
  (** [with_local key value ~f] runs [f] right now with the binding [key ` value].  All
      calls to [find_local key] in [f] and computations started from [f] will return
      [value]. *)
  val with_local : 'a Univ_map.Key.t -> 'a option -> f:(unit -> 'b) -> 'b

  (** [find_local key] returns the value associated to [key] in the current execution
      context. *)
  val find_local : 'a Univ_map.Key.t -> 'a option
  ```

## 109.38.00

- Added `Reader.of_pipe` and `Writer.of_pipe`, for converting from `string Pipe.t`'s.

  These can be used to add arbitrary transformations (e.g. encryption, compression)
  to code that expects plain file- or socket-based readers and writers.

## 109.36.00

- Added `Process.run_lines`, which runs a process and returns stdout as a list of strings.

## 109.35.00

- Made some configuration possible via additional optional arguments
  to `go_main`.

  ```ocaml
  ?max_num_open_file_descrs:int
  ?max_num_threads:int
  ```
- Made some aspects of the async scheduler configurable via functions
  in `Scheduler`.

  ```ocaml
  val set_check_invariants                  : bool        -> unit
  val set_detect_invalid_access_from_thread : bool        -> unit
  val set_max_inter_cycle_timeout           : Time.Span.t -> unit
  val set_record_backtraces                 : bool        -> unit
  ```
- Added a dynamic check in `Pipe` that a consumer is used with the
  correct pipe.

  Specifically, check that a consumer supplied to a read operation on
  a `Pipe.Reader.t` was previously created by `add_consumer` with that
  same reader.
- Renamed `Pipe.fold` as `fold_without_pushback` and added `Pipe.fold`
  with an analogous type to `Pipe.iter`.
- Fixed a bug in `Pipe.merge`, which did not always close the
  resulting pipe when the merge was finished.

  This had prevented medusa regtests from working correctly.
- In `Writer`, changed the default `buffer_age_limit` for files to
  ``Unlimited`.

  THis was done for the same reason that we treat files specially in
  flush on close -- slowness will likely be resolved eventually with a
  file, unlike with a socket.

## 109.34.00

- Changed the scheduler to detect when the thread pool is stuck, i.e. when all threads are blocked and haven't made progress.

  Added default handlers for when the thread pool is stuck, and the
  ability for the user to configure their own handlers.

- Changed low-level I/O to not use nonblocking I/O for char devices.

  This fixes a problem due to `epoll` not working with `/dev/null`.  For
  example:

  ```ocaml
  let () =
    Reader.read_line (Lazy.force Reader.stdin)
    >>> fun _ ->
    shutdown 0
  ;;
  let () = never_returns (Scheduler.go ())
  ```

  had failed when run like:

  ```
  + ./z.exe </dev/null
  ("bug in async scheduler"
   ((Unix.Unix_error "Operation not permitted" epoll_ctl "")
  ...
  ```

- Made `Unix.openfile` use `fstat` to determine the kind of file for a file descriptor rather than assuming it's a regular file.

- Improved the `ASYNC_CONFIG` option `detect_invalid_access_from_thread` by having it include a backtrace in the error message.

## 109.31.00

- Renamed `Reader.read_one_chuck_at_a_time_until_eof` as `Reader.read_one_chunk_at_a_time`,
  and added a new case to the `handle_chunk` callback.

  The name change is to reflect that one can stop early, before EOF.

  The new `handle_chunk` case allows one to specify the number of bytes
  that were read in the `Stop` case.

  Also, changed `read_one_chunk_at_a_time` to use `do_read` instead of
  just locking the file without unlocking it.  This allows subsequent
  read operations to read from the file.

## 109.30.00

- Changed aync's scheduler to use `epoll` rather than `select` by default.

  This is based on a dynamic test to see whether `timerfd_create` works.

- Added support for "busy polling".

  This runs a thread that busy loops running user-supplied polling
  functions.  The busy-loop thread is distinct from async's scheduler
  thread, but it acquires the async lock so that the user-supplied
  function can do ordinary async operations, e.g. fill an ivar.

  Busy polling is useful for a situation like a shared-memory ringbuffer
  being used for IPC.  One can poll the ringbuffer with a busy poller,
  and then when data is detected, fill some ivar that causes async code
  to handle the data.

- Added `Async.Fd.clear_nonblocking`.

  This clears the nonblocking bit on the file descriptor underlying the
  fd, and causes async to treat the fd as though it doesn't support
  nonblocking I/O.

  This is useful for applications that want to share a file descriptor
  between async and non-async code and want to avoid `Sys_blocked_io`
  being seen by the non-async code.

## 109.27.00

- Fixed a performance problem in the scheduler due to repeated calls
  of `Timing_wheel.min_elt`.

  `Timing_wheel.min_elt` is an important part of async, since the
  scheduler calls it once per cycle to know when to timeout for
  `epoll` or `select`.  This causes a problem if `min_elt` is slow and
  called repeatedly, which happens in an application where the next
  clock event is a second out, and yet there are lots of cycles per
  second.

  `Timing_wheel.min_elt` now caches the minimum element, which
  eliminates the problem.
- Fixed async's clock to work on 32-bit machines.

  With the change to `Timing_wheel` in 109.22, async no longer worked
  on 32-bit machines, due to the clock overflowing.  This is because
  it is initialized to `Time.epoch`, and can only handle 6 days.

  The fix now in place is to start the clock at `Time.now ()` rather
  than `Time.epoch`.
- Added many functions to `Async.Sys` so that it looks more like
  `Core.Sys`.
- Changed `Reader.read_one_chunk_at_a_time_until_eof` to not destroy
  the reader buffer.

  Destroying the buffer failed if user code held on to the buffer.

## 109.24.00

- Changed `Reader.close` so that it frees the reader buffer using `Bigstring.unsafe_destroy`.

  This is an improvement over the previous situation, in which the
  buffer wasn't freed until its finalizer fired.

- Fixed a bug in `Reader.read_bin_prot`.

  It was missing a try-with that could cause it to raise without
  cleaning up the reader.

## 109.21.00

- Added `Unix.remove`.

## 109.20.00

- Set `close-on-exec` for both ends of the pipe used to wake up the scheduler.

## 109.19.00

- Reworked a number of `Reader` functions to improve performance by
  avoiding deferreds.

  This is a followup to the `Reader` improvements in 109.14, and
  eliminates some last vestiges of performance degradation that had
  been introduced in 109.04.
- Added function `Reader.lseek : t -> int64 -> mode:[< `Set | `End] ->
  int64 Deferred.t`.

  `lseek t offset ~mode` clears `t`'s buffer and calls `Unix.lseek` on
  `t`'s file descriptor.
- Added function `Writer.bytes_received : t -> int`.
- Added function `Unix.mkfifo : ?perm:file_perm -> string -> unit
  Deferred.t`, which was mistakenly missing.

  This is a simple wrapper around `Core.Unix.mkfifo`.

## 109.18.00

- added `Async.Unix.fcntl_{get,set}fl`.

  Made `Reader` and `Writer` detect if they are passed a file
  descriptor with incorrect permissions (`O_WRONLY` for `Reader`,
  `O_RDONLY` for `Writer`).

## 109.15.00

- The `epoll`-based scheduler now supports sub-millisecond timeouts,
  using `Linux_ext.Timerfd`.

  Async still uses the `select`-based scheduler by default.  We plan
  to switch the default to `epoll` in a few weeks, once we have done
  more testing.
- Eliminated module `Work_group`, which was for limiting the number of
  threads used by jobs.

  This was a little-used module that significantly complicated the
  implementation of the Async thread pool.

  One should consider using a `Throttle` instead.

  Along the way, fixed a bug in Async helper threads in which the
  finalizer could fire too early, causing an unhandled exception.  The
  fix involves relaxing the requirements on when
  `Thread_pool.finished_with_helper_thread` functions can be called,
  allowing it to be called while the helper thread still has work, but
  so long as no future work will be added.

## 109.14.00

- Fixed major performance degradation (since 109.04) in `Reader.read*`
  functions.
- Added function `Rpc.Implementation.map_inv`.

  ```ocaml
  val map_inv : 'a t -> f:('b -> 'a) -> 'b t
  ```
- Add functions `Reader.file_lines` and `Writer.save_lines`.

  These deal with files as lists of their lines.

  ```ocaml
  val Reader.file_lines : string -> string list Deferred.t
  val Writer.save_lines : string -> string list -> unit Deferred.t
  ```
- Added a `?wakeup_scheduler:bool` optional argument to functions in
  the `Thread_safe` module.

  The default is `true`, which continues the behavior that has been in place since 109.09.
  However, once can use `~wakeup_scheduler:false` to reduce CPU use, in return for increased
  latency (because the scheduler won't run a cycle immediately).

## 109.13.00

- Added `Writer.write_line`, which is `Writer.write` plus a newline at
  the end.
- Added `?close_on_exec:bool` argument to `{Reader,Writer}.open_file`
  and `Async.Unix.open_file`.

  Made the default `close_on_exec:true` for `Reader` and `Writer`.
- Added a `compare` function to `Socket.Address.Inet`.

## 109.12.00

- Fixed a bug in `Fd.syscall_in_thread`.

  The bug could cause:

  ```ocaml
  Fd.syscall_in_thread bug -- should be impossible
  ```

  The bug was that `syscall_in_thread` raised rather than returning `Error`.
- Changed `Tcp.connect` and `Tcp.with_connect` to also supply the connected socket.

  Supplying the connected socket makes it easy to call `Socket`
  functions, e.g.  to find out information about the connection with
  `Socket.get{peer,sock}name`.  This also gives information about the IP
  address *after* DNS, which wouldn't otherwise be available.

  One could reconstruct the socket by extracting the fd from the
  writer, and then calling `Socket.of_fd` with the correct
  `Socket.Type`.  But that is both error prone and not discoverable.
- Added `Writer.schedule_bigsubstring`, which parallels
  `Writer.schedule_bigstring`.

## 109.11.00

- Added a check to fail if `Scheduler.go` is called more than once.

## 109.10.00

- Added `Shutdown.do_not_finish_shutdown_before`.  This allows one to
  add `unit Deferred.t`'s that will delay the `shutdown` from
  finishing.  The implementation is more efficient than using
  `at_shutdown`.

## 109.09.00

- Added module `Thread_safe_pipe`, for streaming data outside async into async.
  This a more efficient and feature-ful way to send a sequence of values
  from outside async into async than `Thread_safe.pipe`, which has been
  eliminated.
- Changed functions in `Thread_safe` to always wake up the scheduler.
  Changed `Thread_safe.run_in_async{,_exn}` to not run a cycle, and
  instead rely on the scheduler to run the cycle.

## 109.08.00

- Added module `Async.Process`
  This is a new module for creating and dealing with child processes.
- For `Writer.save`, replaced the `temp_prefix` argument with `temp_file`.
- Added `Ivar.invariant` function.
- Added value `Scheduler.fold_fields`
  This lets one fold over the fields in the scheduler, eliminates an
  annoying place in catalog browser that reached into the internals of
  async to compute the sizes of the scheduler fields

## 109.07.00

- Changed the async scheduler so that if there are no upcoming events,
  it times out in 50ms rather than waiting forever.
- Improved `Reader.read_one_chunk_at_a_time_until_eof`:
  - the callback need not consume everything
  - add `\`Eof_with_unconsumed_data` as a possible result
  - grow internal buffer of the reader when needed
- Added `Shutdown.exit`, removed `Shutdown.shutdown_and_raise`.
- Added `Scheduler.force_current_cycle_to_end`.
