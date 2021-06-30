(** [Writer] is Async's main API for output to a file descriptor.  It is the analog of
    [Core.Out_channel].

    Each writer has an internal buffer, to which [Writer.write*] adds data. Each writer
    uses an Async cooperative thread that makes [write()] system calls to move the data
    from the writer's buffer to an OS buffer via the file descriptor.

    There is no guarantee that the data sync on the other side of the writer can keep up
    with the rate at which you are writing. If it cannot, the OS buffer will fill up and
    the writer's cooperative thread will be unable to send any bytes. In that case, calls
    to [Writer.write*] will grow the writer's buffer without bound, as long as your
    program produces data. One solution to this problem is to call [Writer.flushed] and
    not continue until that becomes determined, which will only happen once the bytes in
    the writer's buffer have been successfully transferred to the OS buffer. Another
    solution is to check [Writer.bytes_to_write] and not produce any more data if that is
    beyond some bound.

    There are two kinds of errors that one can handle with writers. First, a writer can be
    [close]d, which will cause future [write]s (and other operations) to synchronously
    raise an exception. Second, the writer's cooperative thread can fail due to a
    [write()] system call failing. This will cause an exception to be sent to the writer's
    monitor, which will be a child of the monitor in effect when the writer is created.
    One can deal with such asynchronous exceptions in the usual way, by handling the
    stream returned by [Monitor.detach_and_get_error_stream (Writer.monitor writer)]. *)

open! Core
open! Import
module Id : Unique_id

module Line_ending : sig
  type t =
    | Dos
    | Unix
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

include Invariant.S with type t := t

(** Overall IO statistics for all writers. *)
val io_stats : Io_stats.t

(** [stdout] and [stderr] are writers for file descriptors 1 and 2.  They are lazy because
    we don't want to create them in all programs that happen to link with Async.

    When either [stdout] or [stderr] is created, they both are created.  Furthermore, if
    they point to the same inode, then they will be the same writer to [Fd.stdout].  This
    can be confusing, because [fd (force stderr)] will be [Fd.stdout], not [Fd.stderr].
    And subsequent modifications of [Fd.stderr] will have no effect on [Writer.stderr].

    Unfortunately, the sharing is necessary because Async uses OS threads to do [write()]
    syscalls using the writer buffer.  When calling a program that redirects stdout and
    stderr to the same file, as in:

    {v
      foo.exe >/tmp/z.file 2>&1
    v}

    if [Writer.stdout] and [Writer.stderr] weren't the same writer, then they could have
    threads simultaneously writing to the same file, which could easily cause data
    loss. *)
val stdout : t Lazy.t

val stderr : t Lazy.t

type buffer_age_limit =
  [ `At_most of Time.Span.t
  | `Unlimited
  ]
[@@deriving bin_io, sexp]


(** [create ?buf_len ?syscall ?buffer_age_limit fd] creates a new writer.  The file
    descriptor [fd] should not be in use for writing by anything else.

    By default, a write system call occurs at the end of a cycle in which bytes were
    written.  One can supply [~syscall:(`Periodic span)] to get better performance.  This
    batches writes together, doing the write system call periodically according to the
    supplied span.

    A writer can asynchronously fail if the underlying write syscall returns an error,
    e.g., [EBADF], [EPIPE], [ECONNRESET], ....

    [buffer_age_limit] specifies how backed up you can get before raising an exception.
    The default is [`Unlimited] for files, and 2 minutes for other kinds of file
    descriptors.  You can supply [`Unlimited] to turn off buffer-age checks.

    [raise_when_consumer_leaves] specifies whether the writer should raise an exception
    when the consumer receiving bytes from the writer leaves, i.e., in Unix, the write
    syscall returns [EPIPE] or [ECONNRESET].  If [not raise_when_consumer_leaves], then
    the writer will silently drop all writes after the consumer leaves, and the writer
    will eventually fail with a writer-buffer-older-than error if the application remains
    open long enough.

    [line_ending] determines how [newline] and [write_line] terminate lines by default.
    If [line_ending = Unix] then end of line is ["\n"]; if [line_ending = Dos] then end of
    line is ["\r\n"].  Note that [line_ending = Dos] is not equivalent to opening the file
    in text mode because any "\n" characters being printed by other means (e.g., [write
    "\n"]) are still written verbatim (in Unix style).

    [time_source] is useful in tests to trigger [buffer_age_limit]-related conditions, or
    simply to have the result of (for example) [flushed_time_ns] agree with your test's
    synthetic time.  It is also used to schedule the [`Periodic] syscalls. *)
val create
  :  ?buf_len:int
  -> ?syscall:[ `Per_cycle | `Periodic of Time.Span.t ]
  -> ?buffer_age_limit:buffer_age_limit
  -> ?raise_when_consumer_leaves:bool (** default is [true] *)
  -> ?line_ending:Line_ending.t (** default is [Unix] *)
  -> ?time_source:[> read ] Time_source.T1.t
  (** default is [Time_source.wall_clock ()] *)
  -> Fd.t
  -> t

val raise_when_consumer_leaves : t -> bool

(** [set_raise_when_consumer_leaves t bool] sets the [raise_when_consumer_leaves] flag of
    [t], which determies how [t] responds to a write system call raising [EPIPE] and
    [ECONNRESET] (see [create]). *)
val set_raise_when_consumer_leaves : t -> bool -> unit

(** [set_buffer_age_limit t buffer_age_limit] replaces the existing buffer age limit with
    the new one.  This is useful for stdout and stderr, which are lazily created in a
    context that does not allow applications to specify [buffer_age_limit]. *)
val set_buffer_age_limit : t -> buffer_age_limit -> unit

(** [consumer_left t] returns a deferred that becomes determined when [t] attempts to
    write to a pipe that broke because the consumer on the other side left. *)
val consumer_left : t -> unit Deferred.t

val of_out_channel : Out_channel.t -> Fd.Kind.t -> t

(** [open_file file] opens [file] for writing and returns a writer for it.  It uses
    [Unix_syscalls.openfile] to open the file. *)
val open_file
  :  ?append:bool (** default is [false], meaning truncate instead *)
  -> ?buf_len:int
  -> ?syscall:[ `Per_cycle | `Periodic of Time.Span.t ]
  -> ?perm:int (** default is [0o666] *)
  -> ?line_ending:Line_ending.t (** default is [Unix] *)
  -> ?time_source:[> read ] Time_source.T1.t
  (** default is [Time_source.wall_clock ()] *)
  -> string
  -> t Deferred.t

(** [with_file ~file f] opens [file] for writing, creates a writer [t], and runs [f t] to
    obtain a deferred [d].  When [d] becomes determined, the writer is closed.  When the
    close completes, the result of [with_file] becomes determined with the value of [d].

    There is no need to call [Writer.flushed] to ensure that [with_file] waits for the
    writer to be flushed before closing it.  [Writer.close] will already wait for the
    flush. *)
val with_file
  :  ?perm:int (** default is [0o666] *)
  -> ?append:bool (** default is [false], meaning truncate instead *)
  -> ?syscall:[ `Per_cycle | `Periodic of Time.Span.t ]
  -> ?exclusive:bool (** default is [false] *)
  -> ?line_ending:Line_ending.t (** default is [Unix] *)
  -> ?time_source:[> read ] Time_source.T1.t
  (** default is [Time_source.wall_clock ()] *)
  -> string
  -> f:(t -> 'a Deferred.t)
  -> 'a Deferred.t

(** [id] returns an id for this writer that is unique among all other writers. *)
val id : t -> Id.t

(** [fd] returns the [Fd.t] used to create this writer. *)
val fd : t -> Fd.t

(** [set_fd t fd] sets the [fd] used by [t] for its underlying system calls.  It first
    waits until everything being sent to the current [fd] is flushed.  Of course, one must
    understand how the writer works and what one is doing to use this. *)
val set_fd : t -> Fd.t -> unit Deferred.t

(** [write_gen t a] writes [a] to writer [t], with [length] specifying the number of bytes
    needed and [blit_to_bigstring] blitting [a] directly into the [t]'s buffer.  If one
    has a type that has [length] and [blit_to_bigstring] functions, like:

    {[
      module A : sig
        type t
        val length : t -> int
        val blit_to_bigstring : (t, Bigstring.t) Blit.blit
      end ]}

    then one can use [write_gen] to implement a custom analog of [Writer.write], like:

    {[
      module Write_a : sig
        val write : ?pos:int -> ?len:int -> A.t -> Writer.t -> unit
      end = struct
        let write ?pos ?len a writer =
          Writer.write_gen
            ~length:A.length
            ~blit_to_bigstring:A.blit_to_bigstring
            ?pos ?len writer a
      end ]}

    In some cases it may be difficult to write only part of a value:

    {[
      module B : sig
        type t
        val length : t -> int
        val blit_to_bigstring : t -> Bigstring.t -> pos:int -> unit
      end ]}

    In these cases, use [write_gen_whole] instead.  It never requires writing only part of
    a value, although it is potentially less space-efficient.  It may waste portions of
    previously-allocated write buffers if they are too small.

    {[
      module Write_b : sig
        val write : B.t -> Writer.t -> unit
      end = struct
        let write b writer =
          Writer.write_gen_whole
            ~length:B.length
            ~blit_to_bigstring:B.blit_to_bigstring
            writer b
      end ]}

    Note: [write_gen] and [write_gen_whole] give you access to the writer's internal
    buffer.  You should not capture it; doing so might lead to errors of the segfault
    kind. *)
val write_gen
  :  ?pos:int
  -> ?len:int
  -> t
  -> 'a
  -> blit_to_bigstring:('a, Bigstring.t) Blit.blit
  -> length:('a -> int)
  -> unit

val write_gen_whole
  :  t
  -> 'a
  -> blit_to_bigstring:('a -> Bigstring.t -> pos:int -> unit)
  -> length:('a -> int)
  -> unit

(** [write_direct t ~f] gives [t]'s internal buffer to [f].  [pos] and [len] define the
    portion of the buffer that can be filled.  [f] must return a pair [(x, written)] where
    [written] is the number of bytes written to the buffer at [pos].  [write_direct]
    raises if [written < 0 || written > len].  [write_direct] returns [Some x], or [None]
    if the writer is stopped.  By using [write_direct] only, one can ensure that the
    writer's internal buffer never grows.  Look at the [write_direct] expect tests for an
    example of how this can be used to construct a [write_string] like function that never
    grows the internal buffer. *)
val write_direct : t -> f:(Bigstring.t -> pos:int -> len:int -> 'a * int) -> 'a option

(** [write ?pos ?len t s] adds a job to the writer's queue of pending writes.  The
    contents of the string are copied to an internal buffer before [write] returns, so
    clients can do whatever they want with [s] after that. *)
val write_bytes : ?pos:int -> ?len:int -> t -> Bytes.t -> unit

val write : ?pos:int -> ?len:int -> t -> string -> unit
val write_bigstring : ?pos:int -> ?len:int -> t -> Bigstring.t -> unit
val write_iobuf : ?pos:int -> ?len:int -> t -> ([> read ], _) Iobuf.t -> unit
val write_substring : t -> Substring.t -> unit
val write_bigsubstring : t -> Bigsubstring.t -> unit
val writef : t -> ('a, unit, string, unit) format4 -> 'a

(** [to_formatter] returns an OCaml-formatter that one can print to using
    {!Format.fprintf}.  Note that flushing the formatter will only submit all buffered
    data to the writer, but does {e not} guarantee flushing to the operating system. *)
val to_formatter : t -> Format.formatter

(** [write_char t c] writes the character. *)
val write_char : t -> char -> unit

(** [newline t] writes the end-of-line terminator.  [line_ending] can override [t]'s
    [line_ending]. *)
val newline : ?line_ending:Line_ending.t -> t -> unit

(** [write_line t s ?line_ending] is [write t s; newline t ?line_ending]. *)
val write_line : ?line_ending:Line_ending.t -> t -> string -> unit

(** [write_byte t i] writes one 8-bit integer (as the single character with that code).
    The given integer is taken modulo 256. *)
val write_byte : t -> int -> unit

module Terminate_with : sig
  type t =
    | Newline
    | Space_if_needed
  [@@deriving sexp_of]
end

(** [write_sexp t sexp] writes to [t] the string representation of [sexp], possibly
    followed by a terminating character as per [Terminate_with].  With
    [~terminate_with:Newline], the terminating character is a newline.  With
    [~terminate_with:Space_if_needed], if a space is needed to ensure that the sexp reader
    knows that it has reached the end of the sexp, then the terminating character will be
    a space; otherwise, no terminating character is added.  A terminating space is needed
    if the string representation doesn't end in [')'] or ['"']. *)
val write_sexp
  :  ?hum:bool (** default is [false] *)
  -> ?terminate_with:Terminate_with.t (** default is [Space_if_needed] *)
  -> t
  -> Sexp.t
  -> unit

(** [write_bin_prot] writes out a value using its bin_prot sizer/writer pair.  The format
    is the "size-prefixed binary protocol", in which the length of the data is written
    before the data itself.  This is the format that [Reader.read_bin_prot] reads. *)
val write_bin_prot : t -> 'a Bin_prot.Type_class.writer -> 'a -> unit

(** Writes out a value using its bin_prot writer.  Unlike [write_bin_prot], this doesn't
    prefix the output with the size of the bin_prot blob.  [size] is the expected size.
    This function will raise if the bin_prot writer writes an amount other than [size]
    bytes. *)
val write_bin_prot_no_size_header
  :  t
  -> size:int
  -> 'a Bin_prot.Write.writer
  -> 'a
  -> unit

(** Unlike the [write_] functions, all functions starting with [schedule_] require
    flushing or closing of the writer after returning before it is safe to modify the
    bigstrings which were directly or indirectly passed to these functions.  The reason is
    that these bigstrings will be read from directly when writing; their contents is not
    copied to internal buffers.

    This is important if users need to send the same large data string to a huge number of
    clients simultaneously (e.g., on a cluster), because these functions then avoid
    needlessly exhausting memory by sharing the data. *)

(** [schedule_bigstring t bstr] schedules a write of bigstring [bstr].  It is not safe to
    change the bigstring until the writer has been successfully flushed or closed after
    this operation. *)
val schedule_bigstring : t -> ?pos:int -> ?len:int -> Bigstring.t -> unit

val schedule_bigsubstring : t -> Bigsubstring.t -> unit

(** [schedule_iobuf_peek] is like [schedule_bigstring], but for an iobuf.  It is not safe
    to change the iobuf until the writer has been successfully flushed or closed after
    this operation. *)
val schedule_iobuf_peek : t -> ?pos:int -> ?len:int -> ([> read ], _) Iobuf.t -> unit

(** [schedule_iobuf_consume] is like [schedule_iobuf_peek], and additionally advances the
    iobuf beyond the portion that has been written.  Until the result is determined, it is
    not safe to assume whether the iobuf has been advanced yet or not. *)
val schedule_iobuf_consume
  :  t
  -> ?len:int
  -> ([> read ], Iobuf.seek) Iobuf.t
  -> unit Deferred.t

module Destroy_or_keep : sig
  type t =
    | Destroy
    | Keep
  [@@deriving sexp_of]
end

(** [schedule_iovec t iovec] schedules a write of I/O-vector [iovec].  It is not safe to
    change the bigstrings underlying the I/O-vector until the writer has been successfully
    flushed or closed after this operation. *)
val schedule_iovec
  :  ?destroy_or_keep:Destroy_or_keep.t (** default is [Keep] *)
  -> t
  -> Bigstring.t Unix.IOVec.t
  -> unit

(** [schedule_iovecs t iovecs] like {!schedule_iovec}, but takes a whole queue [iovecs] of
    I/O-vectors as argument.  The queue is guaranteed to be empty when this function
    returns and can be modified.  It is not safe to change the bigstrings underlying the
    I/O-vectors until the writer has been successfully flushed or closed after this
    operation. *)
val schedule_iovecs : t -> Bigstring.t Unix.IOVec.t Queue.t -> unit

module Flush_result : sig
  type t =
    | Error
    (** [Error] is accompanied by a detailed error being sent to the writer's monitor. *)
    | Consumer_left
    (** [Consumer_left] is returned when the consumer leaves (see {!consumer_left}) and
        {!raise_when_consumer_leaves} is set to [false]. If that flag is set to [true],
        then you get an [Error] instead. *)
    | Flushed of Time_ns.t
    (** The time just after the [write()] system call returned or
        the time [flushed_*] was called if all the writes were already flushed by then. *)
  [@@deriving sexp_of]
end

(** [flushed_or_failed_with_result t] returns a deferred that will become determined when
    all prior writes complete (i.e. the [write()] system call returns), or when any of
    them fail.

    Handling the [Error] case can be tricky due to the following race: the result gets
    determined concurrently with the exception propagation through the writer's monitor.
    The caller needs to make sure that the program behavior does not depend on which
    signal propagates first.
*)
val flushed_or_failed_with_result : t -> Flush_result.t Deferred.t

(** [flushed_or_failed_unit t] returns a deferred that will become
    determined when all prior writes complete, or when any of them fail.

    Unlike {!flushed_or_failed_with_result}, its return value gives you no indication of
    which happened. In the [Error] case, the result will be determined in parallel with
    the error propagating to the writer's monitor. The caller should robustly handle
    either side winning that race.
*)
val flushed_or_failed_unit : t -> unit Deferred.t

(** [flushed t] returns a deferred that will become determined when all prior writes
    complete (i.e. the [write()] system call returns).  If a prior write fails, then the
    deferred will never become determined.

    It is OK to call [flushed t] after [t] has been closed. *)
val flushed : t -> unit Deferred.t

val flushed_time : t -> Time.t Deferred.t
val flushed_time_ns : t -> Time_ns.t Deferred.t
val fsync : t -> unit Deferred.t
val fdatasync : t -> unit Deferred.t

(** [send] writes a string to the writer that can be read back using [Reader.recv]. *)
val send : t -> string -> unit

(** [monitor t] returns the writer's monitor. *)
val monitor : t -> Monitor.t

(** [close ?force_close t] waits for the writer to be flushed, and then calls [Unix.close]
    on the underlying file descriptor.  [force_close] causes the [Unix.close] to happen
    even if the flush hangs.  By default [force_close] is [Deferred.never ()] for files
    and [after (sec 5)] for other types of file descriptors (e.g., sockets).  If the close
    is forced, data in the writer's buffer may not be written to the file descriptor.  You
    can check this by calling [bytes_to_write] after [close] finishes.

    WARNING: [force_close] will not reliably stop any write that is in progress.
    If there are any in-flight system calls, it will wait for them to finish, which
    includes [writev], which can legitimately block forever.

    [close] will raise an exception if the [Unix.close] on the underlying file descriptor
    fails.

    You must call [close] on a writer in order to close the underlying file descriptor.
    Not doing so will cause a file descriptor leak.  It also will cause a space leak,
    because until the writer is closed, it is held on to in order to flush the writer on
    shutdown.

    It is an error to call other operations on [t] after [close t] has been called, except
    that calls of [close] subsequent to the original call to [close] will return the same
    deferred as the original call.

    [close_started  t] becomes determined as soon as [close] is called.

    [close_finished t] becomes determined after [t]'s underlying file descriptor has been
    closed, i.e., it is the same as the result of [close].  [close_finished] differs from
    [close] in that it does not have the side effect of initiating a close.

    [is_closed t] returns [true] iff [close t] has been called.

    [is_open t] is [not (is_closed t)]

    [with_close t ~f] runs [f ()], and closes [t] after [f] finishes or raises. *)
val close : ?force_close:unit Deferred.t -> t -> unit Deferred.t

val close_started : t -> unit Deferred.t
val close_finished : t -> unit Deferred.t
val is_closed : t -> bool
val is_open : t -> bool
val with_close : t -> f:(unit -> 'a Deferred.t) -> 'a Deferred.t

(** [can_write t] returns [true] if calls to [write*] functions on [t] are allowed.  If
    [is_open t] then [can_write t].  But one can have [is_closed t] and [can_write t],
    during the time after [close t] before closing has finished. *)
val can_write : t -> bool

(** Errors raised within the writer can stop the background job that flushes out the
    writer's buffers. [is_stopped_permanently] returns [true] when the background job has
    stopped. [stopped_permanently] becomes determined when the background job has
    stopped. *)
val is_stopped_permanently : t -> bool

val stopped_permanently : t -> unit Deferred.t

(** In addition to flushing its internal buffer prior to closing, a writer keeps track of
    producers that are feeding it data, so that when [Writer.close] is called, it does the
    following:

    + requests that the writer's producers flush their data to it
    + flushes the writer's internal buffer
    + calls [Unix.close] on the writer's underlying file descriptor

    [with_flushed_at_close t ~flushed ~f] calls [f] and adds [flushed] to the set of
    producers that should be flushed-at-close, for the duration of [f]. *)
val with_flushed_at_close
  :  t
  -> flushed:(unit -> unit Deferred.t)
  -> f:(unit -> 'a Deferred.t)
  -> 'a Deferred.t

(** [bytes_to_write t] returns how many bytes have been requested to write but have not
    yet been written. *)
val bytes_to_write : t -> int

(** [bytes_written t] returns how many bytes have been written. *)
val bytes_written : t -> Int63.t

(** [bytes_received t] returns how many bytes have been received by the writer.  As long
    as the writer is running, [bytes_received = bytes_written + bytes_to_write]. *)
val bytes_received : t -> Int63.t


(** [with_file_atomic ?temp_file ?perm ?fsync file ~f] creates a writer to a temp file,
    feeds that writer to [f], and when the result of [f] becomes determined, atomically
    moves (using [Unix.rename]) the temp file to [file].  If [file] currently exists, it
    will be replaced, even if it is read-only.  The temp file will be [file] (or
    [temp_file] if supplied) suffixed by a unique random sequence of six characters.  The
    temp file may need to be removed in case of a crash so it may be prudent to choose a
    temp file that can be easily found by cleanup tools.

    If [fsync] is [true], the temp file will be flushed to disk before it takes the place
    of the target file, thus guaranteeing that the target file will always be in a sound
    state, even after a machine crash.  Since synchronization is extremely slow, this is
    not the default.  Think carefully about the event of machine crashes and whether you
    may need this option!

    We intend for [with_file_atomic] to mimic the behavior of the [open] system call, so
    if [file] does not exist, we will apply the current umask to [perm] (the effective
    permissions become [perm land lnot umask], see [man 2 open]). However, if [file] does
    exist and [perm] is specified, we do something different from [open] system call: we
    override the permission with [perm], ignoring the umask.  This means that if you
    create and then immediately overwrite the file with [with_file_atomic ~perm], then the
    umask will be honored the first time and ignored the second time. If [perm] is not
    specified, then any existing file permissions are preserved.

    If [f] closes the writer passed to it, [with_file_atomic] raises and does not create
    [file].
*)
val with_file_atomic
  :  ?temp_file:string
  -> ?perm:Unix.file_perm
  -> ?fsync:bool (** default is [false] *)
  -> ?time_source:[> read ] Time_source.T1.t
  (** default is [Time_source.wall_clock ()] *)
  -> string
  -> f:(t -> 'a Deferred.t)
  -> 'a Deferred.t

(** [save] is a special case of [with_file_atomic] that atomically writes the given
    string to the specified file. *)
val save
  :  ?temp_file:string
  -> ?perm:Unix.file_perm
  -> ?fsync:bool (** default is [false] *)
  -> string
  -> contents:string
  -> unit Deferred.t

(** [save_lines file lines] writes all lines in [lines] to [file], with each line followed
    by a newline. *)
val save_lines
  :  ?temp_file:string
  -> ?perm:Unix.file_perm
  -> ?fsync:bool (** default is [false] *)
  -> string
  -> string list
  -> unit Deferred.t

(** [save_sexp] is a special case of [with_file_atomic] that atomically writes the
    given sexp to the specified file.

    [save_sexp t sexp] writes [sexp] to [t], followed by a newline.  To read a file
    produced using [save_sexp], one would typically use [Reader.load_sexp], which deals
    with the additional whitespace and works nicely with converting the sexp to a
    value. *)
val save_sexp
  :  ?temp_file:string
  -> ?perm:Unix.file_perm
  -> ?fsync:bool (** default is [false] *)
  -> ?hum:bool (** default is [true] *)
  -> string
  -> Sexp.t
  -> unit Deferred.t

(** [save_sexps] works similarly to [save_sexp], but saves a sequence of sexps instead,
    separated by newlines.  There is a corresponding [Reader.load_sexps] for reading back
    in. *)
val save_sexps
  :  ?temp_file:string
  -> ?perm:Unix.file_perm
  -> ?fsync:bool (** default is [false] *)
  -> ?hum:bool (** default is [true] *)
  -> string
  -> Sexp.t list
  -> unit Deferred.t

(** [save_bin_prot t bin_writer 'a] is a special case of [with_file_atomic] that writes
    ['a] to [t] using its bin_writer, in the
    size-prefixed format, like [write_bin_prot].  To read a file produced using
    [save_bin_prot], one would typically use [Reader.load_bin_prot]. *)
val save_bin_prot
  :  ?temp_file:string
  -> ?perm:Unix.file_perm
  -> ?fsync:bool (** default is [false] *)
  -> string
  -> 'a Bin_prot.Type_class.writer
  -> 'a
  -> unit Deferred.t

(** [transfer' t pipe_r f] repeatedly reads values from [pipe_r] and feeds them to [f],
    which should in turn write them to [t].  It provides pushback to [pipe_r] by not
    reading when [t] cannot keep up with the data being pushed in.

    By default, each read from [pipe_r] reads all the values in [pipe_r].  One can supply
    [max_num_values_per_read] to limit the number of values per read.

    The [transfer'] stops and the result becomes determined when [stop] becomes
    determined, when [pipe_r] reaches its EOF, when [t] is closed, or when [t]'s consumer
    leaves.  In the latter two cases, [transfer'] closes [pipe_r].

    [transfer'] causes [Pipe.flushed] on [pipe_r]'s writer to ensure that the bytes have
    been flushed to [t] before returning.  It also waits on [Pipe.upstream_flushed] at
    shutdown.

    [transfer t pipe_r f] is equivalent to:

    {[
      transfer' t pipe_r (fun q -> Queue.iter q ~f; return ()) ]} *)
val transfer'
  :  ?stop:unit Deferred.t
  -> ?max_num_values_per_read:int
  -> t
  -> 'a Pipe.Reader.t
  -> ('a Queue.t -> unit Deferred.t)
  -> unit Deferred.t

val transfer
  :  ?stop:unit Deferred.t
  -> ?max_num_values_per_read:int
  -> t
  -> 'a Pipe.Reader.t
  -> ('a -> unit)
  -> unit Deferred.t

(** [pipe t] returns the writing end of a pipe attached to [t] that pushes back when [t]
    cannot keep up with the data being pushed in.  Closing the pipe does not close [t]. *)
val pipe : t -> string Pipe.Writer.t

(** [of_pipe info pipe_w] returns a writer [t] such that data written to [t] will appear
    on [pipe_w].  If either [t] or [pipe_w] are closed, the other is closed as well.

    [of_pipe] is implemented by attaching [t] to the write-end of a Unix pipe, and
    shuttling bytes from the read-end of the Unix pipe to [pipe_w]. *)
val of_pipe
  :  ?time_source:[> read ] Time_source.T1.t (** default is [Time_source.wall_clock ()] *)
  -> Info.t
  -> string Pipe.Writer.t
  -> (t * [ `Closed_and_flushed_downstream of unit Deferred.t ]) Deferred.t

(** [behave_nicely_in_pipeline ~writers ()] causes the program to silently exit with
    status 0 if any of the consumers of [writers] go away.  It also sets the buffer age to
    unlimited, in case there is a human (e.g., using [less]) on the other side of the
    pipeline. *)
val behave_nicely_in_pipeline
  :  ?writers:t list (** defaults to [stdout; stderr] *)
  -> unit
  -> unit

(** [set_synchronous_out_channel t out_channel] waits until [byte_to_write t = 0], and
    then mutates [t] so that all future writes to [t] synchronously call
    [Out_channel.output*] functions to send data to the OS immediately.

    [set_synchronous_out_channel] is used by expect tests to ensure that the interleaving
    between calls to [Core.printf] (and similar IO functions) and [Async.printf] generates
    output with the same interleaving.  [set_synchronous_out_channel] is idempotent. *)
val set_synchronous_out_channel : t -> Out_channel.t -> unit Deferred.t

(** [using_synchronous_backing_out_channel t = true] if writes to [t] are being done
    synchronously, e.g., due to [set_synchronous_out_channel],
    [set_synchronous_backing_out_channel], [use_synchronous_stdout_and_stderr]. *)
val using_synchronous_backing_out_channel : t -> bool

(** [clear_synchronous_out_channel t] restores [t] to its normal state, with the
    background writer asynchronously feeding data to the OS.
    [clear_synchronous_out_channel] is idempotent. *)
val clear_synchronous_out_channel : t -> unit

val with_synchronous_out_channel
  :  t
  -> Out_channel.t
  -> f:(unit -> 'a Deferred.t)
  -> 'a Deferred.t

(** [use_synchronous_stdout_and_stderr ()] causes all subsequent writes to
    stdout and stderr to occur synchronously (after any pending writes have
    flushed).

    This ensures [printf]-family writes happen immediately, which avoids two
    common sources of confusion:

    {ul
    {li unexpected interleaving of [Core.printf] and [Async.printf] calls; and}
    {li [Async.printf] calls that don't get flushed before an application exits}}

    The disadvantages are:

    {ul
    {li this makes writes blocking, which can delay unrelated asynchronous jobs until
    the consumer stops pushing back; and}
    {li the errors raised by write are different and it won't respect
    {!behave_nicely_in_pipeline} anymore}} *)
val use_synchronous_stdout_and_stderr : unit -> unit Deferred.t

(** [Backing_out_channel] generalizes [Out_channel] to a narrow interface that can be used
    to collect strings, etc. *)
module Backing_out_channel : sig
  type t [@@deriving sexp_of]

  val create
    :  output_char:(char -> unit)
    -> output_chars:(bigstring -> len:int -> unit)
    -> flush:(unit -> unit)
    -> sexp:(unit -> Sexp.t)
    -> t

  val of_out_channel : Out_channel.t -> t
  val of_output_char : (char -> unit) -> t
end

val set_synchronous_backing_out_channel : t -> Backing_out_channel.t -> unit Deferred.t

val with_synchronous_backing_out_channel
  :  t
  -> Backing_out_channel.t
  -> f:(unit -> 'a Deferred.t)
  -> 'a Deferred.t

(**/**)

module Private : sig
  val set_bytes_received : t -> Int63.t -> unit
  val set_bytes_written : t -> Int63.t -> unit

  module Check_buffer_age : sig
    module Internal_for_unit_test : sig
      val check_now : check_invariants:bool -> time_source:Time_source.t -> unit
      val num_active_checks_for : Time_source.t -> int option
    end
  end
end
