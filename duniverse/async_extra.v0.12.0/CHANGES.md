## v0.11

- Removed the following modules from `Async_extra`:
  + `Cpu_usage`
  + `Dynamic_port_writer`
  + `File_tail`
  + `Persistent_singleton`
  + `Persistent_connection_intf`
  + `Sequencer_table` (available at: http://github.com/janestreet/sequencer_table )
  + `Tcp_file`
  + `Unpack_sequence`
  + `Versioned_typed_tcp`

## 113.43.00

- Add the ability to specify the local source port to Async_extra.Tcp.

- Name the non-`t` arguments to `Error.tag` and similar functions to allow
  easier partial application.

- Name the non-`t` arguments to `Error.tag` and similar functions to allow
  easier partial application.

- Switched from:

    (** This signature is deliberately empty. *)

  to:

    (*_ This signature is deliberately empty. *)

  This causes the comment to *not* appear in documentation.  We did not use:

    (* This signature is deliberately empty. *)

  because single-star comments, `(* *)`, will be disallowed in signatures.
  We plan to disallow single-star comments to force the use of `(** *)` to
  indicate a doc comment and `(*_ *)` to indicate a non-doc comment.

- Update `Async.Std.Schedule` to allow use of `Time_source.t` other than `wall_clock ()`.

  `Time_source` is a new abstraction for time progression that provides `Clock` and `Time.now`
  functionality driven by a time source other than the wall-clock.  This can be useful for
  offline playback of historical events.

  `Schedule` was updatedso it can be used in these kinds of historical playback situations.

- Add functions to make it easy to start a tcp server that only listens on localhost
  The intention of this change is to make it both easy to bind to localhost and to make it clear in the mli
  that you bind to any with `on_port`.

- Allow passing in a socket to the tcp functions so that you can set socket options before a connection is established

- Make async_extra unit tests pass in 32bit

- Improve errors on exceptions raised inside `Command.async'`.

  A trivial use of Command.async' that raises:

  Before:

    (((pid 7781) (thread_id 0))
     ((human_readable 2016-04-22T19:11:26-0400)
      (int63_ns_since_epoch 1461366686545371364))
     "unhandled exception in Async scheduler"
     ("unhandled exception"
      ((monitor.ml.Error_
        ((exn (Failure "as;dfkj"))
         (backtrace
          ("Raised at file \"pervasives.ml\", line 30, characters 22-33"
           "Called from file \"deferred1.ml\", line 14, characters 63-68"
           "Called from file \"job_queue.ml\", line 160, characters 6-47" ""))
         (monitor
          (((name main) (here ()) (id 1) (has_seen_error true)
            (is_detached false))))))
       ((pid 7781) (thread_id 0)))))

  After:

    $ ./a.exe
    (monitor.ml.Error_
     ((exn (Failure "as;dfkj"))
      (backtrace
       ("Raised at file \"pervasives.ml\", line 30, characters 22-33"
        "Called from file \"monitor.ml\", line 214, characters 42-51"
        "Called from file \"job_queue.ml\", line 160, characters 6-47" ""))
      (monitor
       (((name Async.Std.Deferred.Or_error.try_with) (here ()) (id 2)
         (has_seen_error true) (is_detached true))))))

  After with `~extract_exn:true`:

    $ ./a.exe
    (Failure "as;dfkj")

  The default can be changed for a project this way:

    module Command = struct
      include Command
      let async' = async' ~extract_exn:true
      let async_or_error' = async_or_error' ~extract_exn:true
    end

- Added module `Async.Std.Require_explicit_time_source`, so that one can
  require code to be explicit about what time source is used and not
  unintentionally use the wall clock.  The idiom is to do:

    open! Require_explicit_time_source

  or, in an import.ml:

    include Require_explicit_time_source

- Don't wait for the full `retry_delay` before detecting `close`.

## 113.33.00

- Rename `Async.Std.Schedule.every_{enter,tag_change}` to
  `Async.Std.Schedule.every_{enter,tag_change}_without_pushback` and introduce
  `Async.Std.Schedule.every_{enter,tag_change}` with pushback behavior.

  The resulting functions are analogous to `Pipe.iter_without_pushback` and
  `Pipe.iter`.

- Replaced `Persistent_rpc_client` with a successor `Persistent_connection`
  for maintaining persistent connections to all manner of services, not
  just rpc servers.

- Make `Bus.pipe1_exn` take a `Source_position.t` to be more consistent with
  `first_exn` and `iter_exn`.  This also shows better debug sexps on `Bus` when
  multiple pipes are subscribed to the bus.

## 113.24.00

N.B. some changes happening for this release are not listed in this changelog
since they appear only as a consequence of changes in core or async\_kernel.

- When `Transfer.Writer.send*` raises, send an error to the client.

- Add a new rpc that enables a "push" rather than a "poll" model.

- Switched to PPX.

- For connected UDP sockets, expose `send` in the same fashion as `sendto`.

- `Tcp.Server` is documented to refuse excess connections beyond
  `max_connections + max_pending_connections`, but it treats them as
  pending connections in our standard OS configuration.  In fact,
  research indicates that the documented behavior is nearly impossible
  to obtain directly and consistently from `listen`.

  Clarify the name and role of the `backlog` argument to `listen` and
  rename and update documentation for `max_pending_connections` to
  clarify what it actually does, in light of some research:

      `listen` does not generally respect the backlog argument as an
      upper limit, but as a lower limit (mod `tcp_max_syn_backlog`) and,

      with `tcp_abort_on_overflow=0`, `listen` will ignore excess
      connections rather than actively refusing them.

      (With `syncookies=1`, this can look like an indefinite backlog.)

  Existing, working code can substitute `max_pending_connections ->
  backlog` and move on.  The behavior is not changed.

  When possible, consider architecting applications so the server can
  simply accept and close excess connections, rather than relying on the
  `listen` backlog to return an active indication to the client that
  they won't be serviced.  To make sure the client receives an RST
  rather than an orderly shutdown, you can set the linger time to 0
  before closing the socket.  (Added to unit tests.)

  Direct `Tcp.Server` support for this paradigm is left for future work.

- Make `Rpc_low_latency_transport` treat disconnections as eof, like
  `Async_unix.Reader` does.

- Add an implementation of Mvars to Async

- Allow custom handling of missed async_rpc heartbeats.

- adds a configuration limit on the number of tokens that can be in-flight

- Replace an `#include <sys/errno.h>` by `#include <errno.h>`.

  Fixes janestreet/async\_extra#4

- Added `Tcp.Server.sexp_of_t`

- Adds `Rpc.Pipe_rpc.dispatch_iter`, plus a bunch of additional types to support
  it. The main reason for this is to reduce space usage: `Pipe_rpc.dispatch`
  followed by `Pipe.iter_without_pushback` consumes ~105 words in the steady state
  (i.e., when no messages are coming in) while `dispatch_iter` consumes ~15. I'm
  sure `dispatch` can be improved a lot, but a pipe by itself is 46 words, so it
  can't possibly become as small as `dispatch_iter`.

  Both cases can be made smaller by making `Connection.response_handler` a GADT
  instead of a closure. I plan to do this later.

  One annoying property of the interface is that the only way to cancel
  a subscription is to use `Pipe_rpc.abort`, which has a terrible interface.
  The logical way to improve the interface is to return a record of
  a `Pipe_rpc.t`, a `Connection.t`, and a `Query_id.t`, which allocates an
  additional few words. I'd kind of like to do this but it seems counter to the
  goal of reducing space usage.

- Added `Tcp.Server.listening_on_address`, so that one can get the
  address a server is listening on, as compared with `listening_on`,
  which just returns the port.

- Marked Command.async_basic as deprecated using the appropriate ocaml attribute.

  `@@ocaml.deprecated`

  (http://caml.inria.fr/pub/docs/manual-ocaml/extn.html#sec241)

- Extend the interface of `Persistent_rpc_client` to make the "address"
  type - previously fixed as `Host_and_port.t` - abstract.  This is
  helpful for integrating with libraries that have a different notion of
  an address, e.g. `rpc_discovery_lib`.

- `Typed_tcp` mutated a Hashtbl while iterating over it when closing.

- Added `Async.Bus.first_exn`, which takes a bus and a function, and
  returns a deferred that becomes determined when the first event is
  published to the bus for which the function returns `Some`.

  This function is useful to reduce boilerplate for dealing with
  unsubscription.

- Reduced the number of threads required by tests in:

    async_extra/src/tcp.ml

- Added to the error message `Bus.subscribe_exn called after first write`
  the source-code position of the caller, in case there isn't a backtrace,
  to make the source of the problem clearer, and to avoid confusion with
  other source-code positions of subscribers already in the bus.

- Added to `Bus.first_exn` a `Source_code_position.t` argument, so that
  in the event of subscription failure, we can see who caused the
  subscription to the bus.


- Added to `Tcp.Server.close` an optional argument:

    ?close_existing_connections : bool

  This closes the sockets of all existing connections.

- Annotate errors returned by the async-rpc library with the name of the RPC for
  which the error was returned (if it's an rpc-level error) and a description of
  the remote side of the connection (the ip:host if connected via a network
  socket).

- Improved `Async.Udp.bind`'s error message when it fails to
  `mcast_join` a multicast group.

- Change `~callback` to `~f` throughout the `Bus` interface

## 113.00.00

- Added `Limiter` module.

    Implements an async aware throttling rate limiter on top of `Core.Limiter`.

- Generalized `Persistent_rpc_client` to supports RPC connection types with
  additional information besides the `Rpc.Connection.t` itself.

    For instance: `Persistent_rpc_client.Versioned` has
    `Versioned_rpc.Connection_with_menu.t` as its connection type.

- Changed the `Persistent_rpc_client.Make` functor to not erase the type `conn`
  from its output module's signature.

    This way, the output of `Make` can be fed to functors or functions
    that expect a module matching `Persistent_rpc_client.S`.

- Moved `Log` from `Async_extra` to `Async_unix`, so that the scheduler can
  refer to it.

- Fixed a bug where `Persistent_rpc_client.close` would hang waiting for a
  connection to close.

## 112.35.00

- Added to `Log` a better mechanism for catching and handling background
  errors, via `set_on_error` and an `on_error` argument to `create`.
- Added `Log.get_output : t -> Output.t list`.
- Changed `Monitor.try_with` so that errors after the initial return are
  written to the global error log, rather than ignored.
- Added `Monitor.try_with_or_error` and `try_with_join_or_error`.

    `try_with_or_error` is intended to someday be renamed as `try_with`.
    It also omits some of `try_with`'s optional arguments: `run` and
    `rest`. Different from `try_with`, `try_with_or_error` uses
    ``~run:`Now``, which we now believe is a more sensible behavior.

- Fixed a bug in `Versioned_typed_tcp` that causes spurious and repeated
  reconnects when user-level code disconnects.
- Added `Tcp.Server.create_sock`, to create TCP servers that don't use
  `Reader` and `Writer`.
- Changed `Log.Level.arg` to accept lowercase, uppercase, and
  capitalized words.
- Replaced `Unpack_sequence.unpack*` functions with `unpack_into_pipe`
  and `unpack_iter`, for reduced allocation.

        module Unpack_from : sig
          type t =
          | Pipe   of string Pipe.Reader.t
          | Reader of Reader.t
        end

        val unpack_into_pipe
          :  from  : Unpack_from.t
          -> using : ('a, 'b) Unpack_buffer.t
          -> 'a Pipe.Reader.t * ('a, 'b) Unpack_result.t Deferred.t

        val unpack_iter
          :  from  : Unpack_from.t
          -> using : ('a, 'b) Unpack_buffer.t
          -> f     : ('a -> unit)
          -> ('a, 'b) Unpack_iter_result.t Deferred.t

- Added to `Log` support for user-defined rotation schemes.
- Added `Log.is_closed`.
- Moved `Async_extra.Rpc` to its own library, `Async_kernel_rpc`, and
  abstracted its transport layer.

    `Async_kernel_rpc` depends only on `Async_kernel`.  This allows
    `Async_rpc` to be used in javascript or to try transports tuned for
    different use cases.  `Versioned_rpc` was moved to
    `Async_rpc_kernel` as well.

    `Async_extra` still provides an `Rpc` module with the Unix-dependent
    part:

    - the `Rpc.Transport` module is augmented with
    `Async_unix.{Reader,Writer}` based transports

    - the `Rpc.Connection` module is augmented with helpers for TCP
    based connections

- In sexp-formatted `Log` messages, output the sexp on a single line
  rather than in multi-line "hum"an format.

    This makes it possible to, among other things, easily grep
    such logs.

- Fixed a (very small) space leak in `Persistent_rpc_client`.

    The fix was to use `Deferred.choose` and `Deferred.choice` instead
    of `Deferred.any` and `>>|`.  The old implementation added
    a callback to the `t.close_started` ivar every time the connection
    transitioned from connected to disconnected.

- Added `Persistent_rpc_client.create_generic`, which is like `create`,
  but generic in the function used to connect.

- Fixed a race condition in the `Versioned_typed_tcp` interface that
  caused a worker to miss a `Connect` message if the box is under high
  load.

    `Query_client.create` is called from `Worker_impl.create` in
    a different async cycle than the following call to
    `Query_client.listen` (really, `Tail.collect` under the hood) which
    is made from `Worker_impl.run`.

    When the load on the box is heavy (many workers starting and
    connecting at the same time), the OS might take away the CPU from
    the worker process between the two async cycles.  The TCP socket
    gets connected while the process is still waiting for its turn, and
    eventually, when it's the worker's turn to grab the CPU, Async
    scheduler might process the TCP event earlier than
    `Worker_impl.run`.

- Improved `Udp.ready_iter` to avoid intermediate exceptions by using
  `Syscall_result`.

    UDP loops use that, so will benefit.

    Adjust the implementation slightly as well: made the inner loop
    always exit on `EAGAIN`/`EWOULDBLOCK` to wait until ready, and give
    other Async jobs a chance to run after `EAGAIN`/`EWOULDBLOCK` in the
    outer loop.

## 112.24.00

- Changed `Log` to not eagerly run the rotation loop when an
  `Output.Rotating_file` is created.
- Changed `Log.Output.combine` to write log outputs in sequence rather than
  parallel, to give the correct semantics when the same output is included
  multiple times in `Log.create`.

  This fixed a test that was failing in `lib_test/log_test.ml`.

- Remove `Log.Rotation.t_of_sexp`.
- Made `Command.async*` functions flush `stdout` and `stderr` before calling
  shutdown, to avoid timeouts causing data to be dropped.

  For now, we're making this change in `Command` rather than `Writer`.
  `Writer` already has `at_shutdown` handlers.  We've observed that they
  don't behave well for command-line programs w.r.t. stderr.  So, the
  thinking of this feature is to try out a different `at_shutdown`
  behavior, just for `Command` executables and just for `stdout` and
  `stderr`.  If it works out, maybe we move it into `Writer` proper.
  Putting the change in `Command` for now reduces the scope of what is
  affected by the experiment, and hopefully correlates well with where
  the change is likely to help.

- In `Rpc`, catch exceptions raised by blocking-RPC implementations.

- Added functionality to `Versioned_typed_tcp.Repeater`.

  Added to `create` an argument `is_client_allowed : Client_name.t -> bool`.

  Added to `start` an argument:

      on_connecting_error  : (client_name    : Client_name.t
                              -> server_name : Server_name.t
                              -> Error.t -> unit)

- Fixed a race in `Versioned_typed_tcp` in which a message can be dropped
  between `Server.create` and `Server.listen`

- Simplified the implementation of `Rpc`.

  Rpc has an internal Response_handler module, which is just a record containing
  a response-handling function and an `already_removed` boolean field.  It turns
  out that this is unnecessary:  `already_removed` is set to true when the
  function returns ``remove`, but if it returns ``remove` then it will also be
  removed from a hash table, and we only call the function immediately after
  looking it up in that hash table.

  This wasn't always pointless:  this function used to return deferred values and
  run inside a throttle.  The simplification is only possible because we made it
  synchronous a while ago.

- Added `Tcp.Server.num_connections` function.

- Added creation functions for `Versioned_rpc` menus, for use in clients of an
  RPC proxy.

  In `Menu`:

      val create : Implementation.Description.t list -> t

  In `Connection_with_menu`:
      val create_directly : Connection.t -> Menu.t -> t

  These are for use in clients of an RPC proxy, which can't use the
  regular menu mechanism since they each need to have many menus (one
  for each potential target) but should only need to have one connection
  (to the proxy).

- Added to `Rpc` expert submodules, `Implementations.Expert` and `Rpc.Expert`,
  with low-level access for implementing a proxy that can handle queries without
  knowing their names, types, etc. in advance.

- Renamed `Rpc.Implementation.Description` as `Rpc.Description`.

- Added `Rpc.{Rpc,Pipe_rpc,State_rpc}.description` accessor functions.

- Added `Rpc.Implementation.descriptions`, which returns all RPCs in an
  `Rpc.Implementations.t`.
  This was needed for the `rpc_discovery` library: given an `Implementations.t` we want to advertise at

      prefix/<rpc_name>/<rpc_version>/host_and_port = <host_and_port>

- Added combinators to `Rpc.Implementations`: `lift`, `add`, `add_exn`.

## 112.17.00

- Modernized code style in `Async_extra`.

  This was mostly whitespace changes, plus deletions of unneeded
  module paths.
- Added `with sexp_of` to `Tcp.Where_to_listen` for debugging.
- In `Versioned_typed_tcp`, check that the writer on the other side is
  not closed in the `Pass_on` case.
- Added a new way to implement an RPC, where the implementation
  doesn't return a deferred.

  This "blocking" rpc implementation guarantees that the rpc will in
  fact be fully dispatched by the time the implementation returns.

  This can be used to skip the deserialization of the query, and
  instead operate directly in the message contents as received.

  Also, fixed a bug in which the query handler (and therefore the
  connection state) was being called before the internal async rpc
  handshake was finished.
- Added an optional `job_tag` argument to `Sequencer_table.enqueue`,
  to display for debugging.
- Added an optional argument to TCP-connection functions to control
  the local interface used to connect.

  To implement this this, extended `Tcp.connect` to work on a bound socket.
- Added `with compare` to `Process.Output.t`.
- Added `Process.Output.Stable` module.
- Exposed concrete rpc in `Versioned_rpc.Both_convert`.
- Changed `Cpu_usage` to take its first sample after waiting, rather
  than immediately.

  This fixes a problem where the first sample could be `NAN` or `Inf`.
- Made `Log` buffer-age be unlimited, to avoid exceptions when log
  writes are blocked for long periods.
- Improved `Log.t_of_sexp`'s error message.
- Changed `Rpc.Connection.client` and `with_client` to raise some
  errors which had been dropped during RPC dispatch.

  Previously, errors dispatching `Rpc.Rpc.t`'s were handled correctly
  and returned or raised by the relevant dispatch functions.  However,
  errors that occurred in the middle of handling a `Rpc.Pipe_rpc.t` or
  `Rpc.State_rpc.t` were swallowed.  This is because they happen after
  the dispatch functions have returned, and the dispatch interface
  doesn't allow for errors to occur in the middle of the pipe -- they
  must be raised to the monitor in effect when the `Rpc.Connection.t`
  is created.  Errors could be raised to the effective monitor at the
  dispatch call, but the failure causes the entire connection to go
  into error, so the connection's monitor seems more appropriate.
  These errors weren't propagated to the caller because `client` and
  `with_client` both used `Monitor.try_with` without `rest` handling,
  causing /any/ errors caused while handling the connection (after the
  `Connection.t` has been returned to the user) to be dropped.
- In `Rpc`, exposed some optional parameters from the `Tcp` module:
  `?max_pending_connections` and `?buffer_age_limit`.

## 112.06.00

- In `Log`, exposed the raw message.
- Changed `Rpc` creators' `connection_state` to be a function that takes
  the connection and returns the state.

    This makes it possible for the connection state to actually get
    a handle on the connection itself, which simplifies a number of
    idioms for using RPC.  In particular, it makes it easier to respond
    with an RPC back to a client over client's own connection.

- Fixed some nondeterministically failing tests.
- In `Log`, made logs discard messages when their output list is empty.

    Also, removed redundant tracking of current level.

- Moved `Udp.bind_to_interface_exn` to `Unix` module in `async_unix`.
- Added `Versioned_typed_tcp.Repeater`.

    Repeater is used in the cases where we want to inspect and possibly
    alter the flow between a client and a server without having to
    change either the client or the server or the protocol between them.

## 112.01.00

- Changed `Persistent_rpc_client.connected` to avoid returning a
  connection that is closed at the time it was called.
- Optimized `Rpc.implement` so that if a server's implementation
  returns a determined deferred, then the output is immediately
  serialized and written out for the client.

  This reduces memory consumption, improves throughput and latency.
  Measurements with the `pipe_rpc_test program` showed that a server
  went from processing 600\_000 msg/sec, to 2\_200\_000 msg/sec before
  pegging the CPU.
- Changed `Log`'s output processor's batch size from `1_000` to `100`.
- Added `Persistent_rpc_client.close` and `close_finished`.
- In `Rpc.Connection.client` and `with_client`, used the
  `handshake_timeout` as the `timeout` passed to `Tcp.connect`.

  `handshake_timeout` was previously used only for the `Rpc` module's
  handshake timeout.
- Changed `Rpc.create`'s `on_unknown_rpc` argument, renaming
  `\`Ignore` as `\`Close_connection`, and requiring `\`Call` to return
  `\`Close_connection` or `\`Continue`.

  `\`Ignore` was renamed because it was a poor name, since in fact it
  closed the connection.

  Added a `\`Continue` option, whic allows one to keep the connection
  open.

  Changed `\`Call` to return `\`Continue` or `\`Close_connection`,
  where the old `unit` return value meant `\`Close_connection`.
- In `Versioned_typed_tcp`, enabled the use of "credentials" in the
  "Hello" message.

  Propagate credentials to the user code when it arrives on the wire.

## 111.28.00

- Added to `Versioned_rpc` a non-functor interface.
- Added `Log.level`, which returns the last level passed to `set_level`.
- Enabled Async-RPC pushback in the `Tcp_file` protocol.

## 111.25.00

- Removed `lazy` from the core of `Log`.
- Made `Log.Message.t` have a stable `bin_io`.

  The `Stable.V1` is the current serialization scheme, and `Stable.V0`
  is the serialization scheme in 111.18.00 and before, which is needed
  to talk to older systems.
- Changed `Rpc` to return `Connection_closed` if a connection ends
  before a response makes it to the caller.

  Previously, the dispatch output was never determined.

  Also, removed an unused field in one of the internal data structures
  of Async RPC.
- In `Versioned_rpc`, added `version:int` argument to `implement_multi` functions.
- In `Versioned_rpc`, the `Pipe_rpc.Make` functors now return an
  additional output functor.

  `Register'` is like `Register` but has in its input module:

  ```ocaml
  val response_of_model :
    Model.response Queue.t -> response Queue.t Deferred.t
  ```

  rather than

  ```ocaml
  val response_of_model : Model.response -> response
  ```

  This is analogous to `Pipe.map'` and `Pipe.map`.
- Added to `Log` a `V2` stable format and better readers for
  time-varying formats.
- In `Log`, added an optional `?time:Time.t` argument to allow callers
  to pass in the logged time of an event rather than relying on
  `Time.now ()`.

## 111.21.00

- Added `Sexp_hum` `Log.Output.format`, which is useful for making logs
  more human readable.
- Added `with compare` to `Rpc.Implementation.Description`.

## 111.17.00

- Added module `Persistent_rpc_client`, an RPC client that attempts to
  reconnect when the connection is lost, until a new connection is
  established.
- Significantly sped up the `Rpc` module by removing `Bigstring`
  serialization.

  Performance of the two implementations was tested by building a
  simple client/server executable that would count major cycles.
  Sending 100 byte messages at a rate of 50k/second shows (on both
  sides of the RPC):

  original:
  * ~160 major cycles in 30s
  * CPU usage around 60%

  new:
  * ~10 major cycles in 30s
  * CPU usage <= 2%
- Enabled a version of `Pipe_rpc` and `State_rpc` where the consumer
  can pushback on the producer if it can't consume the contents of the
  pipe fast enough.
- Added `Log.Level.arg : Log.Level.t Command.Spec.Arg_type.t` for
  defining command lines that accept (and autocomplete) log levels.
- Added `Command.async_or_error` and renamed `Command.async_basic` to
  `Command.async`, leaving `async_basic` a deprecated alias for the
  new name.

  `Command.async_or_error` is similar to `Command.basic` and
  `Command.async`, but accepts a `unit Or_error.t Deferred.t` type.
- Added `Persistent_rpc_connection.current_connection`, so that one
  can detect whether one is currently connected.

  ```ocaml
  val current_connection : t -> Rpc.Connection.t option
  ```

## 111.13.00

- For `Typed_tcp.create`, added a `Client_id.t` argument to the `auth`
  callback.

## 111.11.00

- Made `Log` more fair with respect to other Async jobs, by working on
  fixed-length groups of incoming log messages.

    Previously, `Log` had processed everything available.  The change
    gives other Async jobs more of a chance to run.

## 111.08.00

- Added `Log.Message.add_tags`, which extends a message with a list of
  key-value pairs.

        val add_tags : t -> (string * string) list -> t

## 111.06.00

- Added `?on_wouldblock:(unit -> unit)` callback to
  `Udp.recvmmsg_loop` and `recvmmsg_no_sources_loop`.
- For functions that create `Rpc` connections, added optional
  arguments: `?max_message_size:int` and
  `?handshake_timeout:Time.Span.t`.

    These arguments were already available to `Connection.create`, but
    are now uniformly available to all functions that create
    connections.

## 111.03.00

- Add `?max_connections:int` argument to `Rpc.Connection.serve`.

    `max_connections` is passed to `Tcp.Server.create`, and limits the
    number of connections that an Rpc server will accept.

- Improved `Log.Rotation`:

    - Made `Log.Rotation.t` abstract; use `create` rather than an
      explicit record.
    - Added a `` `Dated`` `naming_scheme`.
    - Add `Log.Rotation.default`, for getting a sensible default
      rotation scheme.
    - Added an optional (but discouraged) option to symlink the latest
      log file.
    - Every log rotation scheme has an associated `Time.Zone.t`.
    - Changed the internal representation of `Log.Rotation.t`, but
      `t_of_sexp` is backwards compatible, so existing config files will
      continue to work.

- Changed `Udp.bind_any` to use `Socket.bind ~reuseaddr:false`, to
  ensure a unique port.
- Added `Tcp.Server.listening_socket`, which returns the socket the
  server is listening on.

    Changed `Tcp.Server` so that if the listening socket is closed, the
    server is closed.

- Added to `Udp.Config.t` a `max_ready : int` field to prevent UDP
  receive loops from starving other async jobs.
- Improved `File_tail` to cut the number of `fstat` calls in half.

    `File_tail` uses a stat loop to monitor a file and continue reading
    it as it grows.  We had made two `fstat` invocations per loop
    iteration, using `Async.Std.Unix.with_file` which constructs an
    `Fd.t` and therefore does it own `fstat`.  Switching to
    `Core.Std.Unix.with_file` with `In_thread.run` eliminated the extra
    `fstat`.

## 110.01.00

- Added `Cpu_usage.Sampler` for directly sampling CPU usage.
- Fixed `Log.rotate` to never raise.
- Fixed two bugs in `Log` rotation.
    * Log rotation had used the wrong date when checking whether it
      should rotate.
    * Made `Rotation.keep = \`At_least` delete the oldest, rather than
      the newest, logs.

## 109.60.00

- Replaced `Tcp_file.serve`'s `~port:int` argument with
  `Tcp.Where_to_listen.inet`.

## 109.58.00

- Changed `Cpu_usage` to use `Core.Percent` instead of `float` where
  appropriate.
- Made `Bus.unsubscribe` check that the subscriber is subscribed to
  the given bus.
- Made `Log.t` support `with sexp_of`.
- Fixed `Tcp.on_port 0` to return the port actually being listened on,
  like `Tcp.on_port_chosen_by_os`.

    Previously, a serverlistening on `Tcp.on_port 0` would have its
    `Tcp.Server.listening_on` as `0`, which of course is not the port
    the server is listening on.

## 109.55.00

- Added `Udp.recvmmsg_no_sources_loop`, a specialization of
  `recvmmsg_loop` for improved performance.

    This improvement was driven by profiling at high message rates.

## 109.53.00

- Added module `Bus`, which is an intraprocess "broadcast"
  communication mechanism.
- Added `Tcp.to_inet_address` and `to_unix_address`.
- Added `Tcp.to_socket` which creates a `Tcp.where_to_connect` from a
  `Socket.Address.Inet.t`.
- Module `Weak_hashtbl` is now implemented as a wrapper around
  `Core.Weak_hashtbl`.

    No intended change in behavior.

## 109.52.00

- Added module `Cpu_usage`, which publishes CPU-usage statistics for
  the running process.
- Fixed `Sequencer_table.enqueue` so that there is no deferred between
  finding the state and calling the user function.

## 109.47.00

- Added `with sexp` to `Log.Output.machine_readable_format` and `format`.

## 109.45.00

- Added `?abort:unit Deferred.t` argument to
  `Lock_file.waiting_create`, `Lock_file.Nfs.waiting_create` and
  `critical_section`.

## 109.44.00

- Fixed a time-based race condition in `Log` rotation.

## 109.42.00

- Fixed `Log.Blocking` so that when async is running it writes the message in syslog before failing with an exception.

## 109.40.00

- Added to `Udp.Config` the ability to stop early, via `stop : unit Deferred.t`.

## 109.38.00

- In `Rpc`, exposed accessors for binary protocol values.

    For example, this allows one to write a wrapper for `Pipe_rpc` that
    allows for the easy re cording and replaying of values the come over
    the pipe.

## 109.35.00

- Added module `Async.Udp`, aimed at high-performance UDP
  applications.
- Added module `Lock_file.Nfs`, which wraps the functions in
  `Core.Std.Lock_file.Nfs`.

## 109.33.00

- Change `Log.Global` to by default send all output, including `` `Info ``,
  to `stderr`.

    Replaced `Log.Output.screen` with `Log.Output.stderr`.  There is now
    also and `Log.Output.stdout`.

## 109.32.00

- Added `Dynamic_port_writer`.

    `Dynamic_port_writer` solves the problem of communicating a
    dynamically selected tcp port from a child process to its parent.

## 109.28.00

- Fixed an error message in `Versioned_rpc` that was swapping which
  versions were supported by the caller and the callee.

## 109.27.00

- Added function `Versioned_typed_tcp.Client.shutdown`.
- Added new module `Sequencer_table`, which is a table of
  `Throttle.Sequencer`'s indexed by keys.

## 109.24.00

- Made the `Caller_converts` interface in `Versioned_rpc` use the
  `Connection_with_menu` idea introduced in `Both_convert`.

## 109.19.00

- Added function `Versioned_typed_tcp.Client.flushed : t ->
  [ `Flushed | `Pending of Time.t Deferred.t ]`.

    This exposes whether the underlying `Writer.t` has been flushed.

## 109.17.00

- Added an option to `Async.Log.Rotation` to include the date in
  logfile names.

    This is mostly for archiving purposes.
- Made `Versioned_rpc.Callee_converts.Pipe_rpc.implement_multi` agree
  with `Rpc.Pipe_rpc.implement` on the type of pipe rpc
  implementations.
- Improved the performance of `Versioned_typed_tcp`.

    Avoided creating deferreds while reading the incoming messages.

## 109.15.00

- In `Rpc.client` and `Rpc.with_client`, allowed the client to
  implement the rpcs.

    Added a new optional argument: `?implementations:_ Client_implementations.t`.
- Added new module `Versioned_rpc.Both_convert` to allow the caller
  and callee to independently upgrade to a new rpc.

    This is a new flavor of `Versioned_rpc` in which both sides do some
    type coercions.

## 109.12.00

- Made explicit the equivalence between type `Async.Command.t` and type `Core.Command.t`.

## 109.11.00

- Exposed a `version` function in `Pipe_rpc` and `State_rpc`.

## 109.10.00

- Fixed a race condition in `Pipe_rpc` and `State_rpc`.  This race
  could cause an exception to be raised on connection closing.

## 109.08.00

- Added module `Async.Command`
  This is `Core.Command` with additional async functions.  In particular
  it contains a function `async_basic` that is exactly the same as
  `Core.Command.basic`, except that the function it wraps returns
  `unit Deferred.t`, instead of `unit`.  `async_basic` will also start the
  async scheduler before the wrapped function is run, and will stop the
  scheduler when the wrapped function returns.
