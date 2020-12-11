(** A buffered FIFO communication channel.

    A pipe has a "writer" end and a "reader" end.  The intent is that a writer feeds
    values into the pipe and then waits until it is notified that it should put more data
    in (referred to as "pushback").

    Each pipe contains a buffer that is a queue of values that have been written to the
    pipe but not yet read from the pipe.  The length of the queue is not bounded; whenever
    the pipe is written to, values are immediately enqueued.  However, writers are
    supposed to respect pushback from readers, either via the [unit Deferred.t] returned
    by [write] calls or by explicitly calling [pushback].

    If a pipe is empty, then readers queue up, waiting for values to be written.  As soon
    as values are written, if a reader is available to consume them, the values will be
    handed to the reader.

    One can use [downstream_flushed] to get notified by a pipe when all prior writes have
    been consumed by a reader.

    There are distinct [Reader] and [Writer] modules and types, but all of the operations
    on readers and writers are available directly from the [Pipe] module.

    For debugging your pipe usage you can use [show_debug_messages], and also use
    [set_info] to attach some data to a pipe for identification purposes. *)

open! Core_kernel

type ('a, 'phantom) t [@@deriving sexp_of]
type ('a, 'phantom) pipe = ('a, 'phantom) t [@@deriving sexp_of]

(** {2 Reader and Writer modules} *)

(** These provide reader- and writer-specific types for the base pipe type. *)

module Writer : sig
  type phantom
  type 'a t = ('a, phantom) pipe [@@deriving sexp_of]


  val invariant : _ t -> unit
end

module Reader : sig
  type phantom
  type 'a t = ('a, phantom) pipe [@@deriving sexp_of]


  val invariant : _ t -> unit
end

(** {2 Creation} *)

(** [create_reader ~close_on_exception f] creates a new pipe, applies [f] to its writer
    end, and returns its reader end.  [create_reader] closes the writer end when the
    result of [f] becomes determined.  If [f] raises, then the exception is raised
    to the caller of [create_reader].  Whether or not [create_reader] closes the writer
    end upon [f] raising is determined by [close_on_exception].

    Choosing [~close_on_exception:false] is recommended, because normally closing the
    write end of a pipe is taken to mean that the writer completed successfully.  With
    [close_on_exception:true], the caller will both see the pipe closed and an exception
    will be raised to the monitor in effect when [create_reader] was called.  There is a
    race between those two actions, which can easily lead to confusion or bugs. *)
val create_reader
  :  close_on_exception:bool
  -> ('a Writer.t -> unit Deferred.t)
  -> 'a Reader.t

(** [create_writer] is symmetric with {{!create_reader}[create_reader]}.  It creates a new
    pipe, applies [f] to its reader end, and returns its writer end.  [create_writer]
    calls [close_read] when the result of [f] becomes determined.  If [f] raises,
    [create_writer] closes the pipe and raises the exception to the caller of
    [create_writer].  [create_writer] closes on exception, unlike [create_reader], because
    closing closing the read end of a pipe is a signal to the writer that the consumer has
    failed. *)
val create_writer : ('a Reader.t -> unit Deferred.t) -> 'a Writer.t

(** [create ()] creates a new pipe.  It is preferable to use [create_reader] or
    [create_writer] instead of [create], since they provide exception handling and
    automatic closing of the pipe.  [info] is an arbitrary sexp displayed by [sexp_of_t],
    for debugging purposes; see also [set_info]. *)
val create : ?info:Sexp.t -> unit -> 'a Reader.t * 'a Writer.t

(** [empty ()] returns a closed pipe reader with no contents. *)
val empty : unit -> _ Reader.t

(** [of_list l] returns a closed pipe reader filled with the contents of [l]. *)
val of_list : 'a list -> 'a Reader.t

(** [singleton x] returns a closed pipe reader filled with the single value [x]. *)
val singleton : 'a -> 'a Reader.t

(** [unfold ~init ~f] returns a pipe that it fills with ['a]s by repeatedly applying [f]
    to values of the state type ['s].  When [f] returns [None], the resulting pipe is
    closed.  [unfold] respects pushback on the resulting pipe.  If [f] raises, then the
    pipe is not closed.

    For example, to create a pipe of natural numbers:

    {[
      Pipe.unfold ~init:0 ~f:(fun n -> return (Some (n, n+1))) ]} *)
val unfold : init:'s -> f:('s -> ('a * 's) option Deferred.t) -> 'a Reader.t

(** [of_sequence sequence] returns a pipe reader that gets filled with the elements of
    [sequence].  [of_sequence] respects pushback on the resulting pipe. *)
val of_sequence : 'a Sequence.t -> 'a Reader.t

(** [to_sequence reader] returns a sequence that can be consumed to extract values from
    [reader].  If [Wait_for d] is returned, the consumer must wait for [d] to become
    determined before pulling the next value.  Repeatedly asking for the next value
    without waiting on [d] will infinite loop. *)
type 'a to_sequence_elt =
  | Value of 'a
  | Wait_for : _ Deferred.t -> _ to_sequence_elt

val to_sequence : 'a Reader.t -> 'a to_sequence_elt Sequence.t

(** {2 Closing} *)

(** [close t] closes the write end of the pipe:

    - Future write attempts will fail, raising an exception.

    - If, at the time of the close, there are reads blocked waiting for data, these reads
      will unblock, producing [`Eof].

    - Future read attempts will drain the data that was in the pipe at the time of the
      close, until the pipe's buffer has been exhausted; subsequent reads will immediately
      get [`Eof].

    Thus, after a pipe has been closed, reads never block.

    [close] is idempotent. *)
val close : _ Writer.t -> unit

(** [close_read t] closes both the read and write ends of the pipe.  It does everything
    [close] does, and in addition:

    - all pending flushes become determined with [`Reader_closed].
    - the pipe buffer is cleared.
    - all subsequent reads will get [`Eof]. *)
val close_read : _ Reader.t -> unit

(** [is_closed t] returns [true] iff [close t] or [close_read t] has been called. *)
val is_closed : (_, _) t -> bool

(** [closed t] returns a deferred that becomes determined when [close t] or [close_read t]
    is called. *)
val closed : (_, _) t -> unit Deferred.t

(** {2 Flushing} *)

module Flushed_result : sig
  type t =
    [ `Ok
    | `Reader_closed
    ]
  [@@deriving sexp_of]
end

(** Deferreds returned by [upstream_flushed] and [downstream_flushed] become determined
    when all values written prior to the call have been consumed, or if the reader end of
    the pipe is closed.  The difference between "upstream" and "downstream" comes if one
    has a chain of pipes that are linked (e.g., by [Pipe.map]):

    {v P1 --> P2 --> P3 v}

    Calling [downstream_flushed P2] ensures that everything in P2 has made it out of P3.
    Calling [upstream_flushed P2] ensures that everything in P1 has made it out of P3.
    More generally, [downstream_flushed] starts at the current pipe and follows the chain
    to the final downstream consumer(s).  [upstream_flushed] follows the chain to the
    initial upstream pipe(s), and then calls [downstream_flushed].

    For a pipe in isolation, "consumed" means "read from the pipe".  However, for pipes
    linked together with [transfer] or any function built from [transfer], "consumed"
    means "propagated all the way downstream through the chain and read from the final
    pipe in the chain".  Furthermore, for a pipe ultimately connected to an
    [Async.Writer], "consumed" means the OS write() system call has completed on the bytes
    read from the final pipe in the chain.

    The following [Pipe] functions automatically link their input and output pipes
    together so that [*_flushed] on upstream pipes will propagate to downstream pipes:
    [transfer*], [map*], [filter_map*], [filter], [interleave], [concat].  There is {e
    not} automatic linking with [iter*]; however, user code can customize the behavior of
    flush functions using {{!Consumer}[Consumer]}. *)

val upstream_flushed : (_, _) t -> Flushed_result.t Deferred.t
val downstream_flushed : (_, _) t -> Flushed_result.t Deferred.t

module Consumer : sig
  (** A [Consumer] is used to augment our notion of flushing ([Pipe.upstream_flushed] and
      [Pipe.downstream_flushed]) to include the time spent processing an element once it
      has been removed from the pipe.  It can be thought of as sitting at the end of a
      pipe, or between two pipes, and it provides more detailed feedback on the time an
      element spends outside of the pipe proper.  So we have the following two cases:

      {v
        Pipe --> Consumer
        Pipe --> Consumer --> Pipe --> ...
      v}


      The time outside of the pipe can be broken down into two parts: a part (probably
      short-lived) during which the consumer processes the elements in some way, and a
      downstream portion where the consumer acts as a sentinel to report when the element
      has been fully processed.

      For instance, consider the simple case of a pipe attached to an [Async.Writer]
      that is writing elements to disk.  Part one would be whatever transform the consumer
      applies to the elements in the pipe before it hands them off to the writer, and part
      two would be waiting for the writer to finish writing the transformed element to
      disk.  A more complex case is chaining two pipes together (maybe with a transform
      like [map]).  Part one in this case is the transform and the write to the downstream
      pipe, and part two is waiting for that pipe (and any further pipes in the chain) to
      flush.

      In each case the consumer is responsible for indicating when:

      - it has finished any local work (by attaching itself to elements via the ~consumer
        argument to [read] and [read']) and calling [values_sent_downstream] when it has
        sent the values downstream.

      - when any further processing has been completed (by providing an appropriate
        function to [~downstream_flushed] when [add_consumer] is called).

      If a reader does not use a consumer to do the reading then an element is considered
      flushed the moment it leaves the pipe.  This may lead to odd results as entire
      queues of elements are removed by a call to [read'] but are processed over a long
      period.  In particular, the [fold*] and [iter*] functions cause values to be flushed
      as soon as they are read, unless one passes [~flushed:When_value_processed]. *)

  type t

  val values_sent_downstream : t -> unit
end

(** [add_consumer reader ~downstream_flushed] creates a new consumer of [reader], and
    causes future calls to [flushed_downstream reader] to take this consumer into account.
    Thereafter, [Pipe.flushed_downstream reader] will first ensure that values previously
    written to [reader] have been read, then that they have been sent downstream by the
    consumer that read them, and finally that they have been flushed downstream.

    One should only supply the resulting consumer to read operations on [reader].  Using
    a consumer created from one reader with another reader will raise an exception. *)
val add_consumer
  :  _ Reader.t
  -> downstream_flushed:(unit -> Flushed_result.t Deferred.t)
  -> Consumer.t

(** {2 Generic pipe operations} *)

(** These operations apply to all values of type [(_, _) t], that is, both readers and
    writers. *)

(** [length t] returns the number of elements currently queued in [t]. *)
val length : (_, _) t -> int

(** [is_empty t] is true iff there are no values in the pipe. *)
val is_empty : (_, _) t -> bool

(** {2 Writing} *)

(** The write operations return a deferred value that is determined when either (1) it is
    OK to write again to the pipe or (2) the pipe has been closed.  This deferred is the
    data-producer's interface to the pipe pushback mechanism: it tells the producer when
    it should proceed after doing a write -- either to produce and write more data to the
    pipe, or to abandon production entirely.  The pushback mechanism is just advisory: a
    producer task can, but typically should not, dump arbitrary amounts of data into a
    pipe even if there is no consumer draining it.

    Producers that write a sequence of values to a pipe should be aware that the consumers
    who read from the pipe can close the pipe early -- that is, before the producer has
    finished doing all of its writes.  If this happens, further writes will raise an
    exception.  To avoid these errors, all writes must be atomically guarded by
    [is_closed] tests.  Thus, a typical writer loop should look like this:

    {[
      fun countup hi w = (* Send the ints in range \[0,hi) to writer W. *)
        let rec loop i =
          if i < hi and not (is_closed w) then (* Guard write w/closed test. *)
      write i w >>>            (* Do the write then block until datum     *)
      fun () -> loop (i+1)     (*   fits or the pipe is closed.           *)
      else close w (* No harm done if reader has already closed the pipe.*)
    in
    loop 0 ]}

    If the pipe's consumer stops reading early and closes the pipe, [countup] won't error
    out trying to write further values down the pipe: it will immediately wake up and
    exit. *)

(** [pushback writer] becomes determined when either [writer] has been closed or
    the pipe can accept a new write. *)
val pushback : 'a Writer.t -> unit Deferred.t

(** [write writer a] enqueues [a] in [writer], returning a pushback deferred, as described
    above.

    [transfer_in writer ~from:q] transfers the elements from [q] into [writer], leaving
    [q] empty, and returning a pushback deferred.

    [write_without_pushback] and [transfer_in_without_pushback] are alternatives to
    [transfer_in] and [write] that can be used when you don't care about the pushback
    deferred.  They add data to the pipe and return immediately.

    The following equivalences hold:

    - [write t a = write_without_pushback t a; pushback t]
    - [transfer_in t ~from = transfer_in_without_pushback t ~from; pushback t]

    If [is_closed writer], then all of these functions raise. *)
val write : 'a Writer.t -> 'a -> unit Deferred.t

val write_without_pushback : 'a Writer.t -> 'a -> unit
val transfer_in : 'a Writer.t -> from:'a Queue.t -> unit Deferred.t
val transfer_in_without_pushback : 'a Writer.t -> from:'a Queue.t -> unit

(** [write_when_ready writer ~f] waits until there is space available in the pipe, and
    then calls [f write], where [write] can be used by [f] to write a single value into
    the pipe at a time.  [write_when_ready] guarantees that the pipe is open when it calls
    [f], and hence that the writes will succeed, unless [f] itself closes the pipe. *)
val write_when_ready
  :  'a Writer.t
  -> f:(('a -> unit) -> 'b)
  -> [ `Closed | `Ok of 'b ] Deferred.t

(** [write_if_open w e] is equivalent to:

    {[
      let x = e in
      if not (is_closed w) then (write w x) else (return ()) ]}

    Note the difference in allocation and potential side effects when [w] is closed and
    [e] is a complex expression.

    [write_without_pushback_if_open] is the same as [write_if_open], except it calls
    [write_without_pushback] instead of [write]. *)
val write_if_open : 'a Writer.t -> 'a -> unit Deferred.t

val write_without_pushback_if_open : 'a Writer.t -> 'a -> unit

(** {2 Reading} *)

(** With two special exceptions, all read procedures have a best-effort/forward-progress
    semantics:

    - Best effort: When you do a read, you get what's available {e right now}, which might
      be less than you requested.

    - Forward progress: However, if {e nothing is available}, you block until some data
      comes in (unless you're at EOF, in which case there's obviously no point in waiting).
      So the only time you ever get an empty, 0-item read is when you're at EOF.

    The best-effort semantics allows you to program in a style that processes data in big
    slabs, yet also moves data through your processing in as timely a way as possible.

    The forward-progress semantics means that every call produces {e some} data, so you
    can process an n-element input with at most n reads; you cannot burn an unbounded
    number of cycles "spinning" doing an unbounded number of empty-result "polling" calls
    (which, in a non-preemptive system like Async could lock up the process).

    The two exceptions to best-effort/forward-progress semantics are [read_now], which
    polls for data, thus abandoning the forward-progress guarantee, and [read_exactly],
    which loops until it has read the entire amount requested (or encountered EOF), thus
    abandoning the best-effort guarantee of timeliness. *)

(** [read' pipe] reads values available in the pipe, as soon as any value becomes
    available.  The resulting queue will satisfy [0 < Queue.length q <= max_queue_length].
    [read'] raises if [max_queue_length <= 0].  The [consumer] is used to extend the
    meaning of values being flushed (see the [Consumer] module above). *)
val read'
  :  ?consumer:Consumer.t
  -> ?max_queue_length:int (** default is [Int.max_value] *)
  -> 'a Reader.t
  -> [ `Eof | `Ok of 'a Queue.t ] Deferred.t

(** [read pipe] reads a single value from the pipe.  The [consumer] is used to extend the
    meaning of values being flushed (see the [Consumer] module above). *)
val read : ?consumer:Consumer.t -> 'a Reader.t -> [ `Eof | `Ok of 'a ] Deferred.t


(** [read_exactly r ~num_values] reads exactly [num_values] items, unless EOF is
    encountered.  [read_exactly] performs a sequence of [read_at_most] operations, so
    there is no guarantee that the queue of values it returns comprise a contiguous
    segment of the written stream of values -- other readers might pick off elements
    in-between [read_exactly]'s atomic reads.  [read_exactly] raises if [num_values <= 0].
    The [consumer] is used to extend the meaning of values being flushed (see the
    [Consumer] module above). *)
val read_exactly
  :  ?consumer:Consumer.t
  -> 'a Reader.t
  -> num_values:int
  -> [ `Eof
     | `Fewer of 'a Queue.t (** [0 < Q.length q < num_values] *)
     | `Exactly of 'a Queue.t (** [Q.length q = num_values] *)
     ]
       Deferred.t

(** [read_now' reader] reads values from [reader] that are immediately available.  If
    [reader] is closed, [read_now'] returns [`Eof].  If [reader] is empty, [read_now']
    returns [`Nothing_available].  Otherwise, [`Ok q] is returned, and the resulting queue
    will satisfy [0 < Q.length q <= max_queue_length].  The [consumer] is used to extend
    the meaning of values being flushed (see the [Consumer] module above). *)
val read_now'
  :  ?consumer:Consumer.t
  -> ?max_queue_length:int (** default is [Int.max_value] *)
  -> 'a Reader.t
  -> [ `Eof | `Nothing_available | `Ok of 'a Queue.t ]

(** [read_now] is like [read_now'], except that it reads a single value rather than
    everything that is available. *)
val read_now
  :  ?consumer:Consumer.t
  -> 'a Reader.t
  -> [ `Eof | `Nothing_available | `Ok of 'a ]


val peek : 'a Reader.t -> 'a option

(** [clear reader] consumes all of the values currently in [reader], and all blocked
    flushes become determined with [`Ok]. *)
val clear : 'a Reader.t -> unit

(** [read_all reader] reads all the values from the pipe until it is closed.  An
    alternative name might be [Reader.to_queue]. *)
val read_all : 'a Reader.t -> 'a Queue.t Deferred.t


(** [values_available reader] returns a deferred that becomes determined when there are
    values in the pipe.  If there are multiple readers (a rare situation), there is no
    guarantee that some other reader hasn't become active because of ordinary Async
    scheduling and removed some or all of the values between the time the result of
    [values_available] becomes determined and the time something waiting [upon] that
    result runs.

    [values_available] is useful when one wants to [choose] on values being available in a
    pipe, so that one can be sure and not remove values and drop them on the floor.

    [values_available] is roughly equivalent to [read' ~max_queue_length:0]. *)
val values_available : _ Reader.t -> [ `Eof | `Ok ] Deferred.t

(** [read_choice reader] is:

    {[
      choice
        (values_available reader)
        (fun (_ : [ `Ok | `Eof ]) -> read_now reader) ]}

    [read_choice] consumes a value from [reader] iff the choice is taken.  [read_choice]
    exists to discourage the broken idiom:

    {[
      choice (read reader) (fun ...) ]}

    which is broken because it reads from [reader] even if the choice isn't taken.
    [`Nothing_available] can only be returned if there is a race condition with one or
    more other consumers.

    [read_choice_single_consumer_exn reader [%here]] is like [read_choice reader], but it
    raises in the case of [`Nothing_available].  It is intended to be used when [reader]
    has no other consumers. *)
val read_choice
  :  'a Reader.t
  -> [ `Eof | `Ok of 'a | `Nothing_available ] Deferred.Choice.t

val read_choice_single_consumer_exn
  :  'a Reader.t
  -> Source_code_position.t
  -> [ `Eof | `Ok of 'a ] Deferred.Choice.t

(** {2 Sequence functions} *)

module Flushed : sig

  type t =
    | Consumer of Consumer.t
    | When_value_processed
    | When_value_read
  [@@deriving sexp_of]
end

(** Issues: {ul
    {- Scalar & batch sequence processing:

    Each of the sequence functions ([fold], [iter], [transfer], [map]) comes in two
    versions: "scalar" and "batch" processing.  The scalar version has the ordinary type
    for [f], which handles an element at a time in a non-deferred way.  In the batch
    version, [f] deals with a queue of elements from the pipe at a time, and can block,
    which will cause pushback on writers due to elements not being consumed.}

    {- Early-close and functions that copy between pipes:

    Some functions ([transfer], [map], [filter_map], [filter], [interleave], [concat], and
    their primed, batch-processing variants) spawn a background task that copies data from
    some upstream pipe to some downstream pipe, perhaps with some processing inserted
    in between.  These copying tasks finish under two circumstances.  The standard,
    "normal" case is when the copying task gets EOF from the upstream pipe -- there is no
    more data to copy.  In this case, the copying task closes the downstream pipe, if
    necessary, and exits.

    Somewhat less common is when the downstream consumer decides to stop reading early,
    while the upstream producer is still sending data to the copy task.  (E.g., perhaps
    the consumer was searching its incoming stream for some value, and it found that
    value, so there's no need to search further.)  In this case, the consumer closes its
    pipe to indicate it's done reading values.  When the copy task discovers that its
    downstream pipe is closed, it propagates the close to the upstream producer by closing
    its pipe and stops processing. } } *)

(** [fold' reader ~init ~f] reads a batch of elements from [reader], supplies them to [f],
    waits for [f] to finish, and then repeats.  [fold'] finishes when the call to [f] on
    the final batch of elements from [reader] finishes. *)
val fold'
  :  ?flushed:Flushed.t (** default is [When_value_read] *)
  -> ?max_queue_length:int (** default is [Int.max_value] *)
  -> 'a Reader.t
  -> init:'accum
  -> f:('accum -> 'a Queue.t -> 'accum Deferred.t)
  -> 'accum Deferred.t


(** [fold reader ~init ~f] folds over the elements of [reader], consuming them as they
    come in.  [fold] finishes when the final call to [f] returns. *)

val fold
  :  ?flushed:Flushed.t (** default is [When_value_read] *)
  -> 'a Reader.t
  -> init:'accum
  -> f:('accum -> 'a -> 'accum Deferred.t)
  -> 'accum Deferred.t

val fold_without_pushback
  :  ?consumer:Consumer.t
  -> 'a Reader.t
  -> init:'accum
  -> f:('accum -> 'a -> 'accum)
  -> 'accum Deferred.t

(** [iter' reader ~f ] repeatedly applies [f] to batches of elements of [reader], waiting
    for each call to [f] to finish before continuing.  The deferred returned by [iter']
    becomes determined when the call to [f] on the final batch of elements finishes.
    [~continue_on_error:true] causes the iteration to continue even if [f] raises.

    [~flushed:When_value_processed] means values in batch [b] are flushed only after [f
    b] is filled. *)
val iter'
  :  ?continue_on_error:bool (** default is [false]           *)
  -> ?flushed:Flushed.t (** default is [When_value_read] *)
  -> ?max_queue_length:int (** default is [Int.max_value]   *)
  -> 'a Reader.t
  -> f:('a Queue.t -> unit Deferred.t)
  -> unit Deferred.t

(** [iter t f] is a specialization of [iter'] that applies the [f] to each element in the
    batch, waiting for one call to [f] to finish before making the next call to [f]. *)
val iter
  :  ?continue_on_error:bool (** default is [false]           *)
  -> ?flushed:Flushed.t (** default is [When_value_read] *)
  -> 'a Reader.t
  -> f:('a -> unit Deferred.t)
  -> unit Deferred.t

(** [iter_without_pushback t ~f] applies [f] to each element in [t], without giving [f] a
    chance to pushback on the iteration continuing.  If [f] raises on some element of [t],
    [iter_without_pushback] will not consume any further elements.
    [iter_without_pushback] will not make more than [max_iterations_per_job] calls to [f]
    in a single Async_job; this can be used to increase Async-scheduling fairness. *)
val iter_without_pushback
  :  ?consumer:Consumer.t
  -> ?continue_on_error:bool (** default is [false] *)
  -> ?max_iterations_per_job:int (** default is [Int.max_value] *)
  -> 'a Reader.t
  -> f:('a -> unit)
  -> unit Deferred.t


(** [transfer' input output ~f] repeatedly reads a batch of elements from [input], applies
    [f] to the batch, writes the result as a batch to [output], and then waits on
    [pushback] in [output] before continuing.  [transfer'] finishes if [input] is closed
    or [output] is closed.  If [output] is closed, then [transfer'] closes [input].  Use
    [~max_queue_length:1] to cause elements to appear on the output pipe as soon as they
    are processed, without having to wait for the entire queue. *)
val transfer'
  :  ?max_queue_length:int (** default is [Int.max_value] *)
  -> 'a Reader.t
  -> 'b Writer.t
  -> f:('a Queue.t -> 'b Queue.t Deferred.t)
  -> unit Deferred.t

(** [transfer] is like [transfer'], except that it processes one element at a time. *)
val transfer : 'a Reader.t -> 'b Writer.t -> f:('a -> 'b) -> unit Deferred.t

(** [transfer_id] is a specialization of [transfer'] with [f = Fn.id]. *)
val transfer_id
  :  ?max_queue_length:int (** default is [Int.max_value] *)
  -> 'a Reader.t
  -> 'a Writer.t
  -> unit Deferred.t

(** [map' input ~f] returns a reader, [output], and repeatedly applies [f] to batches of
    elements from [input], with the results appearing in [output].  If values are not
    being consumed from [output], [map'] will pushback and stop consuming values from
    [input]. If [output] is closed, then [map'] will close [input].  Use
    [~max_queue_length:1] to cause elements to appear on the output pipe as soon as they
    are processed, without having to wait for the entire queue. *)
val map'
  :  ?max_queue_length:int (** default is [Int.max_value] *)
  -> 'a Reader.t
  -> f:('a Queue.t -> 'b Queue.t Deferred.t)
  -> 'b Reader.t

(** [map] is like [map'], except that it processes one element at a time. *)
val map : 'a Reader.t -> f:('a -> 'b) -> 'b Reader.t

(** [folding_map] is a version of [map] that threads an accumulator through calls to [f].
*)
val folding_map
  :  ?max_queue_length:int (** default is [Int.max_value] *)
  -> 'a Reader.t
  -> init:'accum
  -> f:('accum -> 'a -> 'accum * 'b)
  -> 'b Reader.t

(** [filter_map' input ~f] returns a reader, [output], and repeatedly applies [f] to
    elements from [input], with the results that aren't [None] appearing in [output].  If
    values are not being consumed from [output], [filter_map'] will pushback and stop
    consuming values from [input].  If [output] is closed, then [filter_map'] will close
    [input].  [filter_map'] processes elements in batches as per [max_queue_length]; in a
    single batch, all outputs will propagate to the result only when all inputs have been
    processed. *)
val filter_map'
  :  ?max_queue_length:int (** default is [Int.max_value] *)
  -> 'a Reader.t
  -> f:('a -> 'b option Deferred.t)
  -> 'b Reader.t

(** [filter_map] is a specialized version of [filter_map']. *)
val filter_map
  :  ?max_queue_length:int (** default is [Int.max_value] *)
  -> 'a Reader.t
  -> f:('a -> 'b option)
  -> 'b Reader.t

(** [folding_filter_map'] is a version of [filter_map'] that threads an accumulator
    through calls to [f].  Like [filter_map'], [folding_filter_map'] processes elements in
    batches as per [max_queue_length]; in a single batch, all outputs will propagate to
    the result only when all inputs have been processed. *)
val folding_filter_map'
  :  ?max_queue_length:int (** default is [Int.max_value] *)
  -> 'a Reader.t
  -> init:'accum
  -> f:('accum -> 'a -> ('accum * 'b option) Deferred.t)
  -> 'b Reader.t

(** [folding_filter_map] is a specialized version of [folding_filter_map']. *)
val folding_filter_map
  :  ?max_queue_length:int (** default is [Int.max_value] *)
  -> 'a Reader.t
  -> init:'accum
  -> f:('accum -> 'a -> 'accum * 'b option)
  -> 'b Reader.t

(** [filter input ~f] returns a reader, [output], and copies to [output] each element from
    [input] that satisfies the predicate [f].  If [output] is closed, then [filter] closes
    [input]. *)
val filter : 'a Reader.t -> f:('a -> bool) -> 'a Reader.t

(** [interleave inputs] returns a reader, [output], and, for each input, transfers batches
    of values from that input to [output], using [transfer_id].  Each input is transferred
    to [output] independently.  So, batches of values from different inputs can be in
    flight to [output] simultaneously, but at most one batch at a time from any particular
    input.  The operation is complete when either all the [inputs] produce EOF, or when
    [output] is closed by the downstream consumer (in which case [interleave] closes all
    the [inputs]). *)
val interleave : 'a Reader.t list -> 'a Reader.t

val interleave_pipe : 'a Reader.t Reader.t -> 'a Reader.t

(** [merge inputs ~compare] returns a reader, [output], that merges all the inputs.
    Assuming that for each input, values are sorted according to the comparison function
    [compare], values for each input will be transfered to [output] and the values
    returned by [output] will be sorted according to [compare]. *)
val merge : 'a Reader.t list -> compare:('a -> 'a -> int) -> 'a Reader.t

(** [concat inputs] return a reader, [output], with the values from each pipe in [inputs]
    in sequence.  [concat] closes [output] once it reaches EOF on the final input.
    If [output] is closed, then [concat] closes all its inputs. *)
val concat : 'a Reader.t list -> 'a Reader.t

(** [concat_pipe] is like [concat], but it takes a pipe of inputs instead of
    a list, and closes the input pipe when the output pipe is closed. *)
val concat_pipe : 'a Reader.t Reader.t -> 'a Reader.t

(** [fork input] returns a pair of readers and transfers each of the values in [input]
    into both of the returned readers.  It closes [input] early if both of the readers are
    closed early.

    If [pushback_uses = `Both_consumers], then [fork] waits for [pushback] on both readers
    when writing.  If one of the readers is not read from or is slow to be read from, it
    may block the other from receiving data.  Beware of possible deadlocks in downstream
    code due to blocking on reading too many elements from one before reading the other.

    If [pushback_uses = `Fast_consumer_only], then [fork] waits for [pushback] only on the
    faster of the two readers when writing.  In this case the slow reader cannot block the
    faster one, but [fork] could be forced to buffer arbitrarily many elements.  Beware of
    unbounded resource usage in downstream code where one reader might fall behind.

    Note that {!upstream_flushed} will not work with the pipes returned by [fork]. *)
val fork
  :  'a Reader.t
  -> pushback_uses:[ `Both_consumers | `Fast_consumer_only ]
  -> 'a Reader.t * 'a Reader.t

(** [to_stream_deprecated reader] returns a stream that reads everything from the pipe.
    This function is deprecated because one should change the code that is consuming
    a stream to instead consume from a pipe reader. *)
val to_stream_deprecated : 'a Reader.t -> 'a Async_stream.t

(** [of_stream_deprecated reader] returns a pipe that has one element for every element on
    the stream.  This function is deprecated because one should change the code that is
    producing a stream to instead produce a pipe reader. *)
val of_stream_deprecated : 'a Async_stream.t -> 'a Reader.t

(** [drain reader] repeatedly reads values from [reader] and throws them away.

    [drain_and_count] is like [drain], except it also counts the number of values it
    has read. *)
val drain : 'a Reader.t -> unit Deferred.t

val drain_and_count : 'a Reader.t -> int Deferred.t

(** [to_list input] reads everything from [input]; on EOF, it produces the accumulated
    list of these values. *)
val to_list : 'a Reader.t -> 'a list Deferred.t

(** {2 Miscellaneous} *)

(** [hash] is a hash function based on the internal id of the pipe. *)
val hash : (_, _) t -> int

(** [equal] on pipes is physical equality. *)
val equal : ('a, 'b) t -> ('a, 'b) t -> bool

(** [compare] on pipes is based on the internal id of the pipe. *)
val compare : (_, _) t -> (_, _) t -> int

(** {2 Size budget} *)

(** Every pipe has a "size budget", which governs the pushback that is used to discourage
    writers from enqueueing arbitrarily large amounts of data.  As long as the length of
    the pipe exceeds the size budget, writers will not be notified to do further writing.
    Whenever the length is less than or equal to the size budget, writers will be notified
    to continue.

    Every pipe's initial size budget is zero. *)
val size_budget : (_, _) t -> int

(** [set_size_budget t i] changes the size budget of [t] to [i].  Any nonnegative value is
    allowed. *)
val set_size_budget : (_, _) t -> int -> unit

(** {2 Debugging} *)

(** [show_debug_messages], if true, will cause a message to be printed at the start of
    each operation, showing the pipe and other arguments. *)
val show_debug_messages : bool ref

(** [check_invariant], if true, will cause pipes' invariants to be checked at the start of
    each operation. *)
val check_invariant : bool ref

(** [set_info] updates [t]'s [info] field, which is displayed by [sexp_of_t], and thus in
    debugging messages. *)
val set_info : (_, _) t -> Sexp.t -> unit
