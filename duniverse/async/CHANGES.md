## git version

- Added a new (single-module) library `async.log_extended`, extending
  `Async.Log` with `Console` and `Syslog` submodules.

- Improved Async scheduler fairness by calling Thread.yield every cycle, which
  releases the OCaml lock if any other OCaml threads are waiting to acquire it.

- Added a new (single-module) library `async.unpack_sequence`, for efficiently
  unpacking a sequence of packed values coming from a `string Pipe.Reader.t` or
  a `Reader.t`.

- Increased Unix.listen's default backlog from 10 to 64, to reduce occurrences
  of TCP clients getting Unexpected EOF errors when the listening server is
  busy.

- Added an optional argument to Pipe functions fold and iter so they can
  consider a value flushed when it is processed by the supplied ~f rather than
  when it is read out of the pipe.

- `Weak_hashtbl` was moved into its own library `Weak_hashtbl_async`, which is
  released as part of the async package.

- Added function `Tcp.Server.close_finished_and_handlers_determined`.

## v0.10

- Moved `Persistent_connection` to `Async_kernel`, so that it can be used in
  javascript.

- Improved `Log.Output` to write, flush, rotate, or close all `Log.Output.t`s,
  even if one raises

- Added `Async.print_s` for pretty printing a sexp to stdout.

- Removed a per-cycle allocation from the scheduler.

- Fixed `Reader.load_bin_prot` to always return `Error` when there is an error,
  rather than return `Error` in some situations and raise in others.
- Improved the behavior of shutdown when there are errors.

- Added `Scheduler.may_sleep_for_thread_fairness`, an opt-in configuration that
  improves thread fairness.

- Added to `Sys.file_exists` function a `follow_symlinks` optional argument.

- Fixed the Async scheduler so that for top-level unhandled exceptions, it runs
  `at_exit` functions once, not twice.

- For `Writer.open_file`, exposed the syscall optional argument.

- Exposed `Async.ifprintf`, which had previously been mistakenly shadowed even
  though it doesn't block.

- Unified `Synchronous_time_source.t` and `Time_source.t`, into a single data
  structure, allowing one to convert between them as needed. This involved
  substantive changes to Async's clock.

- Added function `Time_source.timing_wheel_now`.

- Added stable types for `Log.Level` and `Log.Output.format`.

- Improved shutdown function so that when shutdown is forced (i.e. `at_shutdown`
  handlers time out), it calls `Pervasives.exit`, which runs `at_exit` handlers.
  This improves `Command.async` in this situation, due to its use of an
  `at_exit` handler to print errors.

- Improved `Process.run`'s error message when `working_dir` is missing.

- Fixed `Rpc.Connection.create` so that it doesn't raise on a failed handshake.

- Significantly improved the performance of `Log.printf` when the log message
  won't be printed, by using `ifprintf` to avoid constructing the message.

- Added `Socket.Address` functions `Inet.to_sockaddr` and `Unix.to_sockaddr`,
  the type specialized versions of `Socket.Address.to_sockaddr`.

- Added `Socket.bind_inet`, which is like bind, but restricted to Inet addresses
  and does not return a `Deferred.t`. Changed `Udp.bind` and `bind_any` to not
  return a `Deferred.t`.

- Added to `File_tail.create` an optional `?throttle` argument so that one can
  use a custom throttle and `max_concurrent_jobs` rather than a global throttle
  with a fixed `max_concurrent_jobs`.

- Renamed `Tcp` and `Rpc`'s `via_local_interface` and `via_local_port` arguments
  as `bind_to_address` and `bind_to_port`.

- Made `Tcp.Server.create` and `create_sock`'s `~on_handler_error` argument
  mandatory.

- In `Tcp.Server`, stopped calling `on_handler_error` for `Writer` error from
  `inner_monitor`, which only indicated that the client likely closed the
  connection before all the bytes could be written.

- Renamed `Command.async` as `async_spec` and `Command.async'` as `async`.  We
  want to encourage the use of `Command.Param` and discourage the use of
  `Command.Spec`.

- Changed `Async` so that in tests it uses synchronous output.

- Changed `Async`'s default max number of open file descriptors from `8_192` to
  the minimum of `32_768` and `ulimit -n -H`.

- In the Async scheduler's main loop, avoided calling `Time_ns.now` and
  `Linux_ext.Timerfd.set_after` unless they are needed. This saves about 50ns
  per cycle.

- Moved `Tcp` functions for specifying where to connect and where to listen into
  submodules: `Where_to_connect` and `Where_to_listen`

- Changed `Tcp.to_host_and_port` from taking a string and int to
  `Tcp.Where_to_connect.of_host_and_port`, taking a `Host_and_port.t`

- Changed `Rpc.Connection.client` to take a `Tcp.Where_to_connect.t` instead of
  `~host ~port`.

- Changed `Synchronous_time_source.Event.abort` to return a variant type, in the
  same style as `Time_source.Event.abort`. Added `abort_exn` and
  `abort_if_possible`, also in the same style as `Time_source.Event`.

- Added function `Scheduler.long_cycles`, which returns the stream of cycles
  whose duration exceeds a user-supplied time span. This is more efficient than
  `cycle_times`, because it only allocates a stream element when there is a long
  cycle, rather than on every cycle.

- Made internal libraries stdless: `Async_unix`, `Async_extra`.

- Changed `Udp.recvfrom_loop` and `read_loop` functions to return a variant
  `Closed | Stopped` rather than `unit`.

- Extended the `Unix.Inet_addr` module's interface to include
  `Core.Unix.Inet_addr`'s interface.

## v0.9

## 113.43.00

- Added some expect tests of `Monitor`, in particular
  `Monitor.handle_errors`.

- Added a benchmark of `Monitor.try_with`.

## 113.33.00

Keep up to date with interface changes in `Async_kernel`, `Async_extra` and
`Async_unix`.

## 113.24.00

Keep up to date with interface changes in `Async_kernel`, `Async_extra` and
`Async_unix`.

## 113.00.00

- Added `Async.Std.Printf` module so that one doesn't unintentionally use
  blocking `Core.Std.Printf` functions in an Async program.

    There was much pre-existing code that did this via:

    : open Core.Std
    : open Async.Std

    `Async.Std.Printf` defines blocking functions (e.g `printf`,
    `eprintf`) to cause a type error, but leaves Async-friendly functions
    (e.g. `sprintf`, `ksprintf`) untouched.

    Replaced uses of `Printf.*`, with `Core.Std.Printf.*` where needed.

## 112.35.00

- Include some previously-omitted benchmarks

## 112.24.00

Keep up to date with interface changes in `Async_kernel`, `Async_extra` and
`Async_unix`.

## 112.17.00

Added tests and updated examples

## 112.01.00

- update tests

## 111.25.00

- add a dns example

## 111.11.00

- Updated the sound.ml example

## 109.53.00

- Bump version number

## 109.14.00

- Added function `Monitor.kill`, which kills a monitor and all its
  descendants.

    This prevents any jobs from ever running in the monitor again.

## 109.09.00

- Switched `Async.Std`'s toplevel bindings for `Deferred.Or_error`'s `bind` and `map` to use
  `Deferred.Result`.
  This allows them to be used with any `'error` type, rather than just `Error.t`.

## 109.05.00

- Added `val _squelch_unused_module_warning_` to `Async.Std`.
