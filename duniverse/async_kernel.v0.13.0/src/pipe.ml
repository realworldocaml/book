open Core_kernel
open Import
open Deferred_std
module Stream = Async_stream
module Q = Queue

let show_debug_messages = ref false
let check_invariant = ref false

module Flushed_result = struct
  type t =
    [ `Ok
    | `Reader_closed
    ]
  [@@deriving compare, sexp_of]

  let equal = [%compare.equal: t]

  let combine (l : t Deferred.t list) =
    let%map l = Deferred.all l in
    match List.mem l `Reader_closed ~equal with
    | true -> `Reader_closed
    | false -> `Ok
  ;;
end

(* A [Consumer.t] acts as the monitor of some process that reads values from a pipe and
   processes them, allowing that process:
   - to communicate that it has taken responsibility for the values
   - to signal when it has finished with the values to interested parties (via
     [downstream_flushed])

   It is used in two steps:

   1. calling [Consumer.start] at the point where the consumer takes values out of the
   Pipe via [read] or [read'].

   2. calling [Consumer.values_sent_downstream].

   By calling [values_sent_downstream] one asserts that the [downstream_flushed] function
   supplied to [create] will now wait for this value.

   If no [Consumer.t] is supplied when a value is read then the value is defined to be
   flushed at that time. *)
module Consumer : sig
  type t [@@deriving sexp_of]

  include Invariant.S with type t := t

  val create
    :  pipe_id:int
    -> downstream_flushed:(unit -> Flushed_result.t Deferred.t)
    -> t

  val pipe_id : t -> int
  val start : t -> unit
  val values_sent_downstream : t -> unit
  val values_sent_downstream_and_flushed : t -> Flushed_result.t Deferred.t
end = struct
  type t =
    { pipe_id : int
    ; (* [values_read] reflects whether values the consumer has read from the pipe have been
         sent downstream or if not, holds an ivar that is to be filled when they are. *)
      mutable values_read :
        [ `Have_been_sent_downstream | `Have_not_been_sent_downstream of unit Ivar.t ]
    ; (* [downstream_flushed ()] returns when all prior values that the consumer has
         passed downstream have been flushed all the way down the chain of pipes. *)
      downstream_flushed : unit -> Flushed_result.t Deferred.t
    }
  [@@deriving fields, sexp_of]

  let invariant t : unit =
    try
      let check f field = f (Field.get field t) in
      Fields.iter
        ~pipe_id:ignore
        ~values_read:
          (check (function
             | `Have_been_sent_downstream -> ()
             | `Have_not_been_sent_downstream ivar -> assert (Ivar.is_empty ivar)))
        ~downstream_flushed:ignore
    with
    | exn ->
      raise_s [%message "Pipe.Consumer.invariant failed" (exn : exn) ~pipe:(t : t)]
  ;;

  let create ~pipe_id ~downstream_flushed =
    { pipe_id; values_read = `Have_been_sent_downstream; downstream_flushed }
  ;;

  let start t =
    match t.values_read with
    | `Have_not_been_sent_downstream _ -> ()
    | `Have_been_sent_downstream ->
      t.values_read <- `Have_not_been_sent_downstream (Ivar.create ())
  ;;

  let values_sent_downstream t =
    match t.values_read with
    | `Have_been_sent_downstream -> ()
    | `Have_not_been_sent_downstream ivar ->
      Ivar.fill ivar ();
      t.values_read <- `Have_been_sent_downstream
  ;;

  let values_sent_downstream_and_flushed t =
    match t.values_read with
    | `Have_been_sent_downstream -> t.downstream_flushed ()
    | `Have_not_been_sent_downstream when_sent_downstream ->
      let%bind () = Ivar.read when_sent_downstream in
      t.downstream_flushed ()
  ;;
end

module Blocked_read = struct
  (* A [Blocked_read.t] represents a blocked read attempt.  If someone reads from an empty
     pipe, they enqueue a [Blocked_read.t] in the queue of [blocked_reads].  Later, when
     values are written to a pipe, that will cause some number of blocked reads to be
     filled, first come first serve.  The blocked-read constructor specifies how many
     values a read should consume from the pipe when it gets its turn.

     If a pipe is closed, then all blocked reads will be filled with [`Eof]. *)
  type 'a wants =
    | Zero of [ `Eof | `Ok ] Ivar.t
    | One of [ `Eof | `Ok of 'a ] Ivar.t
    | At_most of int * [ `Eof | `Ok of 'a Q.t ] Ivar.t
  [@@deriving sexp_of]

  type 'a t =
    { wants : 'a wants
    ; consumer : Consumer.t option
    }
  [@@deriving fields, sexp_of]

  let invariant t : unit =
    try
      let check f field = f (Field.get field t) in
      Fields.iter
        ~wants:
          (check (function
             | Zero _ | One _ -> ()
             | At_most (i, _) -> assert (i > 0)))
        ~consumer:
          (check (function
             | None -> ()
             | Some consumer -> Consumer.invariant consumer))
    with
    | exn ->
      raise_s [%message "Pipe.Blocked_read.invariant failed" (exn : exn) ~pipe:(t : _ t)]
  ;;

  let create wants consumer = { wants; consumer }

  let is_empty t =
    match t.wants with
    | Zero i -> Ivar.is_empty i
    | One i -> Ivar.is_empty i
    | At_most (_, i) -> Ivar.is_empty i
  ;;

  let fill_with_eof t =
    match t.wants with
    | Zero i -> Ivar.fill i `Eof
    | One i -> Ivar.fill i `Eof
    | At_most (_, i) -> Ivar.fill i `Eof
  ;;
end

module Blocked_flush = struct
  (* A [Blocked_flush.t] represents a blocked flush operation, which can be enabled by a
     future read.  If someone does [flushed p] on a pipe, that blocks until everything
     that's currently in the pipe at that point has drained out of the pipe.  When we call
     [flushed], it records the total amount of data that has been written so far in
     [fill_when_num_values_read].  We fill the [Flush.t] with [`Ok] when this amount of
     data has been read from the pipe.

     A [Blocked_flush.t] can also be filled with [`Reader_closed], which happens when the
     reader end of the pipe is closed, and we are thus sure that the unread elements
     preceding the flush will never be read. *)
  type t =
    { fill_when_num_values_read : int
    ; ready : [ `Ok | `Reader_closed ] Ivar.t
    }
  [@@deriving fields, sexp_of]

  let fill t v = Ivar.fill t.ready v
end

type ('a, 'phantom) t =
  { (* [id] is an integer used to distinguish pipes when debugging. *)
    id : int (* [buffer] holds values written to the pipe that have not yet been read. *)
  ; mutable buffer : 'a Q.t
  ; (* [size_budget] governs pushback on writers to the pipe.

       There is *no* invariant that [Q.length buffer <= size_budget].  There is no hard
       upper bound on the number of elements that can be stuffed into the [buffer].  This
       is due to the way we handle writes.  When we do a write, all of the values written
       are immediately enqueued into [buffer].  After the write, if [Q.length buffer <=
       t.size_budget], then the writer will be notified to continue writing.  After the
       write, if [length t > t.size_budget], then the write will block until the pipe is
       under budget. *)
    mutable size_budget : int
  ; (* [pushback] is used to give feedback to writers about whether they should write to
       the pipe.  [pushback] is full iff [length t <= t.size_budget || is_closed t]. *)
    mutable pushback : unit Ivar.t
  ; (* [num_values_read] keeps track of the total number of values that have been read
       from the pipe.  We do not have to worry about overflow in [num_values_read].  You'd
       need to write 2^62 elements to the pipe, which would take about 146 years, at a
       flow rate of 1 size-unit/nanosecond. *)
    mutable num_values_read : int
  ; (* [blocked_flushes] holds flushes whose preceding elements have not been completely
       read.  For each blocked flush, the number of elements that need to be read from the
       pipe in order to fill the flush is                        :

       fill_when_num_values_read - num_values_read

       Keeping the data in this form allows us to change a single field(num_values_read)
       when we consume values instead of having to iterate over the whole queue of
       flushes. *)
    blocked_flushes : Blocked_flush.t Q.t
  ; (* [blocked_reads] holds reads that are waiting on data to be written to the pipe. *)
    blocked_reads : 'a Blocked_read.t Q.t
  ; (* [closed] is filled when we close the write end of the pipe. *)
    closed : unit Ivar.t
  ; (* [read_closed] is filled when we close the read end of the pipe. *)
    read_closed : unit Ivar.t
  ; (* [consumers] is a list of all consumers that may be handling values read from the
       pipe. *)
    mutable consumers : Consumer.t list
  ; (* [upstream_flusheds] has a function for each pipe immediately upstream of this one.
       That function walks to the head(s) of the upstream pipe, and calls
       [downstream_flushed] on the head(s).  See the definition of [upstream_flushed]
       below. *)
    upstream_flusheds : (unit -> Flushed_result.t Deferred.t) Bag.t
  }
[@@deriving fields, sexp_of]

type ('a, 'phantom) pipe = ('a, 'phantom) t [@@deriving sexp_of]

let hash t = Hashtbl.hash t.id
let equal (t1 : (_, _) t) t2 = phys_equal t1 t2
let compare t1 t2 = Int.compare t1.id t2.id
let is_closed t = Ivar.is_full t.closed
let is_read_closed t = Ivar.is_full t.read_closed
let closed t = Ivar.read t.closed
let pushback t = Ivar.read t.pushback
let length t = Q.length t.buffer
let is_empty t = length t = 0

let invariant t : unit =
  try
    let check f field = f (Field.get field t) in
    Fields.iter
      ~id:ignore
      ~buffer:ignore
      ~size_budget:(check (fun size_budget -> assert (size_budget >= 0)))
      ~pushback:
        (check (fun pushback ->
           assert (
             Bool.equal
               (Ivar.is_full pushback)
               (length t <= t.size_budget || is_closed t))))
      ~num_values_read:ignore
      ~blocked_flushes:
        (check (fun blocked_flushes ->
           Q.iter blocked_flushes ~f:(fun (f : Blocked_flush.t) ->
             assert (f.fill_when_num_values_read > t.num_values_read));
           assert (
             List.is_sorted
               ~compare:Int.compare
               (List.map
                  (Q.to_list blocked_flushes)
                  ~f:Blocked_flush.fill_when_num_values_read));
           if is_empty t then assert (Q.is_empty blocked_flushes)))
      ~blocked_reads:
        (check (fun blocked_reads ->
           (* If data is available, no one is waiting for it.  This would need to change if
              we ever implement [read_exactly] as an atomic operation. *)
           if not (is_empty t) then assert (Q.is_empty blocked_reads);
           Q.iter blocked_reads ~f:(fun read ->
             Blocked_read.invariant read;
             assert (Blocked_read.is_empty read));
           (* You never block trying to read a closed pipe. *)
           if is_closed t then assert (Q.is_empty blocked_reads)))
      ~closed:ignore
      ~read_closed:ignore
      ~consumers:
        (check (fun l ->
           List.iter l ~f:(fun consumer ->
             Consumer.invariant consumer;
             assert (Consumer.pipe_id consumer = t.id))))
      ~upstream_flusheds:ignore
  with
  | exn -> raise_s [%message "Pipe.invariant failed" (exn : exn) ~pipe:(t : (_, _) t)]
;;

module Reader = struct
  type phantom [@@deriving sexp_of]
  type 'a t = ('a, phantom) pipe [@@deriving sexp_of]

  let invariant = invariant
end

module Writer = struct
  type phantom [@@deriving sexp_of]
  type 'a t = ('a, phantom) pipe [@@deriving sexp_of]

  let invariant = invariant
end

let id_ref = ref 0

let create_internal ~initial_buffer =
  incr id_ref;
  let t =
    { id = !id_ref
    ; closed = Ivar.create ()
    ; read_closed = Ivar.create ()
    ;
      size_budget = 0
    ; pushback = Ivar.create ()
    ; buffer = initial_buffer
    ; num_values_read = 0
    ; blocked_flushes = Q.create ()
    ; blocked_reads = Q.create ()
    ; consumers = []
    ; upstream_flusheds = Bag.create ()
    }
  in
  t
;;

let create () =
  let t = create_internal ~initial_buffer:(Q.create ()) in
  (* initially, the pipe does not pushback *)
  Ivar.fill t.pushback ();
  if !check_invariant then invariant t;
  t, t
;;

let update_pushback t =
  if length t <= t.size_budget || is_closed t
  then Ivar.fill_if_empty t.pushback ()
  else if Ivar.is_full t.pushback
  then t.pushback <- Ivar.create ()
;;

let close t =
  if !show_debug_messages then eprints "close" t [%sexp_of: (_, _) t];
  if !check_invariant then invariant t;
  if not (is_closed t)
  then (
    Ivar.fill t.closed ();
    if is_empty t
    then (
      Q.iter t.blocked_reads ~f:Blocked_read.fill_with_eof;
      Q.clear t.blocked_reads);
    update_pushback t)
;;

let close_read t =
  if !show_debug_messages then eprints "close_read" t [%sexp_of: (_, _) t];
  if !check_invariant then invariant t;
  if not (is_read_closed t)
  then (
    Ivar.fill t.read_closed ();
    Q.iter t.blocked_flushes ~f:(fun flush -> Blocked_flush.fill flush `Reader_closed);
    Q.clear t.blocked_flushes;
    Q.clear t.buffer;
    update_pushback t;
    (* we just cleared the buffer, so may need to fill [t.pushback] *)
    close t)
;;

let create_reader ~close_on_exception f =
  let r, w = create () in
  if not close_on_exception
  then upon (f w) (fun () -> close w)
  else
    don't_wait_for
      (Monitor.protect
         (fun () -> f w)
         ~finally:(fun () ->
           close w;
           return ()));
  r
;;

let init f = create_reader ~close_on_exception:true f

let create_writer f =
  let r, w = create () in
  don't_wait_for
    (Monitor.protect
       (fun () -> f r)
       ~finally:(fun () ->
         close_read r;
         return ()));
  w
;;

let values_were_read t consumer =
  Option.iter consumer ~f:Consumer.start;
  let rec loop () =
    match Q.peek t.blocked_flushes with
    | None -> ()
    | Some flush ->
      if t.num_values_read >= flush.fill_when_num_values_read
      then (
        ignore (Q.dequeue_exn t.blocked_flushes : Blocked_flush.t);
        (match consumer with
         | None -> Blocked_flush.fill flush `Ok
         | Some consumer ->
           upon
             (Consumer.values_sent_downstream_and_flushed consumer)
             (fun flush_result -> Blocked_flush.fill flush flush_result));
        loop ())
  in
  loop ()
;;

(* [consume_all t] reads all the elements in [t]. *)
let consume_all t consumer =
  let result = t.buffer in
  t.buffer <- Q.create ();
  t.num_values_read <- t.num_values_read + Q.length result;
  values_were_read t consumer;
  update_pushback t;
  result
;;

let consume_one t consumer =
  assert (length t >= 1);
  let result = Q.dequeue_exn t.buffer in
  t.num_values_read <- t.num_values_read + 1;
  values_were_read t consumer;
  update_pushback t;
  result
;;

let consume t ~max_queue_length consumer =
  assert (max_queue_length >= 0);
  if max_queue_length >= length t
  then consume_all t consumer
  else (
    t.num_values_read <- t.num_values_read + max_queue_length;
    values_were_read t consumer;
    let result = Q.create ~capacity:max_queue_length () in
    Q.blit_transfer ~src:t.buffer ~dst:result ~len:max_queue_length ();
    update_pushback t;
    result)
;;

let set_size_budget t size_budget =
  if size_budget < 0 then raise_s [%message "negative size_budget" (size_budget : int)];
  t.size_budget <- size_budget;
  update_pushback t
;;

let fill_blocked_reads t =
  while (not (Q.is_empty t.blocked_reads)) && not (is_empty t) do
    let blocked_read = Q.dequeue_exn t.blocked_reads in
    let consumer = blocked_read.consumer in
    match blocked_read.wants with
    | Zero ivar -> Ivar.fill ivar `Ok
    | One ivar -> Ivar.fill ivar (`Ok (consume_one t consumer))
    | At_most (max_queue_length, ivar) ->
      Ivar.fill ivar (`Ok (consume t ~max_queue_length consumer))
  done
;;

(* checks all invariants, calls a passed in f to handle a write, then updates reads and
   pushback *)
let start_write t =
  if !show_debug_messages then eprints "write" t [%sexp_of: (_, _) t];
  if !check_invariant then invariant t;
  if is_closed t then raise_s [%message "write to closed pipe" ~pipe:(t : (_, _) t)]
;;

let finish_write t =
  fill_blocked_reads t;
  update_pushback t
;;

let transfer_in_without_pushback t ~from =
  start_write t;
  Q.blit_transfer ~src:from ~dst:t.buffer ();
  finish_write t
;;

let transfer_in t ~from =
  transfer_in_without_pushback t ~from;
  pushback t
;;

let copy_in_without_pushback t ~from =
  start_write t;
  Queue.iter from ~f:(fun x -> Queue.enqueue t.buffer x);
  finish_write t
;;

(* [write'] is used internally *)
let write' t q = transfer_in t ~from:q

let write_without_pushback t value =
  start_write t;
  Q.enqueue t.buffer value;
  finish_write t
;;

let write t value =
  write_without_pushback t value;
  pushback t
;;

let write_when_ready t ~f =
  let%map () = pushback t in
  if is_closed t then `Closed else `Ok (f (fun x -> write_without_pushback t x))
;;

let write_if_open t x = if not (is_closed t) then write t x else return ()

let write_without_pushback_if_open t x =
  if not (is_closed t) then write_without_pushback t x
;;

let ensure_consumer_matches ?consumer t =
  match consumer with
  | None -> ()
  | Some consumer ->
    if t.id <> Consumer.pipe_id consumer
    then
      raise_s
        [%message
          "Attempt to use consumer with wrong pipe"
            (consumer : Consumer.t)
            ~pipe:(t : _ Reader.t)]
;;

let start_read ?consumer t label =
  if !show_debug_messages then eprints label t [%sexp_of: (_, _) t];
  if !check_invariant then invariant t;
  ensure_consumer_matches t ?consumer
;;

let gen_read_now ?consumer t consume =
  start_read t "read_now" ?consumer;
  if is_empty t
  then if is_closed t then `Eof else `Nothing_available
  else (
    assert (Q.is_empty t.blocked_reads);
    (* from [invariant] and [not (is_empty t)] *)
    `Ok (consume t consumer))
;;

let get_max_queue_length ~max_queue_length =
  match max_queue_length with
  | None -> Int.max_value
  | Some max_queue_length ->
    if max_queue_length <= 0
    then raise_s [%message "max_queue_length <= 0" (max_queue_length : int)];
    max_queue_length
;;

let read_now' ?consumer ?max_queue_length t =
  let max_queue_length = get_max_queue_length ~max_queue_length in
  gen_read_now t ?consumer (fun t consumer -> consume t ~max_queue_length consumer)
;;

let read_now ?consumer t = gen_read_now t ?consumer consume_one

let read_now_at_most ?consumer t ~num_values =
  read_now' t ?consumer ~max_queue_length:num_values
;;

let peek t = Queue.peek t.buffer

let clear t =
  match read_now' t with
  | `Eof | `Nothing_available | `Ok _ -> ()
;;

let read' ?consumer ?max_queue_length t =
  let max_queue_length = get_max_queue_length ~max_queue_length in
  start_read t "read'" ?consumer;
  match read_now' t ?consumer ~max_queue_length with
  | (`Ok _ | `Eof) as r -> return r
  | `Nothing_available ->
    Deferred.create (fun ivar ->
      Q.enqueue
        t.blocked_reads
        (Blocked_read.create (At_most (max_queue_length, ivar)) consumer))
;;

let read ?consumer t =
  start_read t "read" ?consumer;
  if is_empty t
  then
    if is_closed t
    then return `Eof
    else
      Deferred.create (fun ivar ->
        Q.enqueue t.blocked_reads (Blocked_read.(create (One ivar)) consumer))
  else (
    assert (Q.is_empty t.blocked_reads);
    return (`Ok (consume_one t consumer)))
;;

let read_at_most ?consumer t ~num_values = read' t ?consumer ~max_queue_length:num_values

let values_available t =
  start_read t "values_available";
  if not (is_empty t)
  then return `Ok
  else if is_closed t
  then return `Eof
  else (
    match Queue.last t.blocked_reads with
    | Some { consumer = None; wants = Zero ivar } ->
      (* This case is an optimization for multiple calls to [values_available] in
         sequence.  It causes them to all share the same ivar, rather than allocate
         an ivar per call. *)
      Ivar.read ivar
    | _ ->
      Deferred.create (fun ivar ->
        Q.enqueue t.blocked_reads (Blocked_read.(create (Zero ivar)) None)))
;;

let read_choice t = choice (values_available t) (fun (_ : [ `Ok | `Eof ]) -> read_now t)

let read_choice_single_consumer_exn t here =
  Deferred.Choice.map (read_choice t) ~f:(function
    | (`Ok _ | `Eof) as x -> x
    | `Nothing_available ->
      raise_s
        [%message
          "Pipe.read_choice_single_consumer_exn: choice was enabled but pipe is \
           empty; this is likely due to a race condition with one or more other \
           consumers"
            (here : Source_code_position.t)])
;;

(* [read_exactly t ~num_values] loops, getting you all [num_values] items, up
   to EOF. *)
let read_exactly ?consumer t ~num_values =
  start_read t "read_exactly" ?consumer;
  if num_values <= 0
  then raise_s [%message "Pipe.read_exactly got num_values <= 0" (num_values : int)];
  Deferred.create (fun finish ->
    let result = Q.create () in
    let rec loop () =
      let already_read = Q.length result in
      assert (already_read <= num_values);
      if already_read = num_values
      then Ivar.fill finish (`Exactly result)
      else
        read' ?consumer t ~max_queue_length:(num_values - already_read)
        >>> function
        | `Eof -> Ivar.fill finish (if already_read = 0 then `Eof else `Fewer result)
        | `Ok q ->
          Q.blit_transfer ~src:q ~dst:result ();
          loop ()
    in
    loop ())
;;

let downstream_flushed t =
  if is_empty t
  then
    if List.is_empty t.consumers
    then return `Ok
    else
      Flushed_result.combine
        (List.map t.consumers ~f:Consumer.values_sent_downstream_and_flushed)
  else
    (* [t] might be closed.  But the read end can't be closed, because if it were, then
       [t] would be empty.  If the write end is closed but not the read end, then we want
       to enqueue a blocked flush because the enqueued values may get read. *)
    Deferred.create (fun ready ->
      Q.enqueue
        t.blocked_flushes
        { fill_when_num_values_read = t.num_values_read + length t; ready })
;;

(* In practice, along with [Link.create] and [add_upstream_flushed], [upstream_flushed]
   traverses the graph of linked pipes up to the heads and then calls [downstream_flushed]
   on them. *)
let upstream_flushed t =
  if Bag.is_empty t.upstream_flusheds
  then downstream_flushed t
  else
    Bag.to_list t.upstream_flusheds
    |> List.map ~f:(fun f -> f ())
    |> Flushed_result.combine
;;

let add_upstream_flushed t upstream_flushed =
  Bag.add t.upstream_flusheds upstream_flushed
;;

let add_consumer t ~downstream_flushed =
  let consumer = Consumer.create ~pipe_id:t.id ~downstream_flushed in
  t.consumers <- consumer :: t.consumers;
  consumer
;;

(* A [Link.t] links flushing of two pipes together. *)
module Link : sig
  type t

  val create : upstream:(_, _) pipe -> downstream:(_, _) pipe -> t
  val consumer : t -> Consumer.t

  (* [unlink_upstream] removes downstream's reference to upstream. *)

  val unlink_upstream : t -> unit
end = struct
  type ('a, 'b) unpacked =
    { downstream : ('a, 'b) t
    ; consumer : Consumer.t
    ; upstream_flusheds_bag_elt : (unit -> Flushed_result.t Deferred.t) Bag.Elt.t
    }

  type t = T : (_, _) unpacked -> t

  let consumer (T t) = t.consumer

  let create ~upstream ~downstream =
    T
      { downstream
      ; consumer =
          add_consumer upstream ~downstream_flushed:(fun () ->
            downstream_flushed downstream)
      ; upstream_flusheds_bag_elt =
          add_upstream_flushed downstream (fun () -> upstream_flushed upstream)
      }
  ;;

  let unlink_upstream (T t) =
    Bag.remove t.downstream.upstream_flusheds t.upstream_flusheds_bag_elt
  ;;
end

module Flushed = struct
  type t =
    | Consumer of Consumer.t
    | When_value_processed
    | When_value_read
  [@@deriving sexp_of]
end

let fold_gen
      (read_now : ?consumer:Consumer.t -> _ Reader.t -> _)
      ?(flushed = Flushed.When_value_read)
      t
      ~init
      ~f
  =
  let consumer =
    match flushed with
    | When_value_read -> None
    | Consumer consumer -> Some consumer
    | When_value_processed ->
      (* The fact that "no consumer" behaves different from "trivial consumer" is weird,
         but that's how the consumer machinery works. *)
      Some (add_consumer t ~downstream_flushed:(fun () -> return `Ok))
  in
  if !check_invariant then invariant t;
  ensure_consumer_matches t ?consumer;
  Deferred.create (fun finished ->
    (* We do [return () >>>] to ensure that [f] is only called asynchronously. *)
    return ()
    >>> fun () ->
    let rec loop b =
      match read_now t ?consumer with
      | `Eof -> Ivar.fill finished b
      | `Ok v -> f b v continue
      | `Nothing_available -> values_available t >>> fun _ -> loop b
    and continue b =
      Option.iter consumer ~f:Consumer.values_sent_downstream;
      loop b
    in
    loop init)
;;

let fold' ?flushed ?max_queue_length t ~init ~f =
  fold_gen (read_now' ?max_queue_length) ?flushed t ~init ~f:(fun b q loop ->
    f b q >>> loop)
;;

let fold ?flushed t ~init ~f =
  fold_gen read_now ?flushed t ~init ~f:(fun b a loop -> f b a >>> loop)
;;

let fold_without_pushback ?consumer t ~init ~f =
  fold_gen
    read_now
    t
    ~init
    ~f:(fun b a loop -> loop (f b a))
    ?flushed:
      (match consumer with
       | None -> None
       | Some c -> Some (Consumer c))
;;

let with_error_to_current_monitor ?(continue_on_error = false) f a =
  if not continue_on_error
  then f a
  else (
    match%map Monitor.try_with (fun () -> f a) with
    | Ok () -> ()
    | Error exn -> Monitor.send_exn (Monitor.current ()) (Monitor.extract_exn exn))
;;

let iter' ?continue_on_error ?flushed ?max_queue_length t ~f =
  fold' ?max_queue_length ?flushed t ~init:() ~f:(fun () q ->
    with_error_to_current_monitor ?continue_on_error f q)
;;

let iter ?continue_on_error ?flushed t ~f =
  fold_gen read_now ?flushed t ~init:() ~f:(fun () a loop ->
    with_error_to_current_monitor ?continue_on_error f a >>> fun () -> loop ())
;;

(* [iter_without_pushback] is a common case, so we implement it in an optimized manner,
   rather than via [iter].  The implementation reads only one element at a time, so that
   if [f] closes [t] or raises, no more elements will be read. *)
let iter_without_pushback
      ?consumer
      ?(continue_on_error = false)
      ?max_iterations_per_job
      t
      ~f
  =
  ensure_consumer_matches t ?consumer;
  let max_iterations_per_job =
    match max_iterations_per_job with
    | None -> Int.max_value
    | Some max_iterations_per_job ->
      if max_iterations_per_job <= 0
      then
        raise_s
          [%message
            "iter_without_pushback got non-positive max_iterations_per_job"
              (max_iterations_per_job : int)];
      max_iterations_per_job
  in
  let f =
    if not continue_on_error
    then f
    else
      fun a ->
        try f a with
        | exn -> Monitor.send_exn (Monitor.current ()) exn
  in
  Deferred.create (fun finished ->
    (* We do [return () >>>] to ensure that [f] is only called asynchronously. *)
    return ()
    >>> fun () ->
    let rec start () = loop ~remaining:max_iterations_per_job
    and loop ~remaining =
      if remaining = 0
      then return () >>> fun () -> start ()
      else (
        match read_now t ?consumer with
        | `Eof -> Ivar.fill finished ()
        | `Ok a ->
          f a;
          loop ~remaining:(remaining - 1)
        | `Nothing_available -> values_available t >>> fun _ -> start ())
    in
    start ())
;;

let drain t = iter' t ~f:(fun _ -> return ())
let drain_and_count t = fold' t ~init:0 ~f:(fun sum q -> return (sum + Q.length q))

let read_all input =
  let result = Q.create () in
  let%map () =
    iter' input ~f:(fun q ->
      Q.blit_transfer ~src:q ~dst:result ();
      return ())
  in
  result
;;

let to_list r = read_all r >>| Q.to_list

let to_stream_deprecated t =
  Stream.create (fun tail ->
    iter_without_pushback t ~f:(fun x -> Tail.extend tail x)
    >>> fun () -> Tail.close_exn tail)
;;

(* The implementation of [of_stream_deprecated] does as much batching as possible.  It
   grabs as many items as are available into an internal queue.  Once it has grabbed
   everything, it writes it to the pipe and then blocks waiting for the next element from
   the stream.

   There's no possibility that we'll starve the pipe reading an endless stream, just
   accumulating the elements into our private queue forever without ever writing them
   downstream to the pipe.  Why? because while we're running, the stream-producer *isn't*
   running -- there are no Async block points in the queue-accumulator loop.  So the
   queue-accumulator loop will eventually catch up to the current stream tail, at which
   point we'll do the pipe-write and then block on the stream... thus giving the
   stream-producer a chance to make more elements.

   One can't implement [of_stream] using [Stream.iter] or [Stream.iter'] because you
   need to be able to stop early when the consumer closes the pipe.  Also, using either
   of those would entail significantly more deferred overhead, whereas the below
   implementation uses a deferred only when it needs to wait for data from the stream. *)
let of_stream_deprecated s =
  let r, w = create () in
  let q = Q.create () in
  let transfer () =
    if not (Q.is_empty q)
    then
      (* Can not pushback on the stream, so ignore the pushback on the pipe. *)
      don't_wait_for (write' w q)
  in
  let rec loop s =
    assert (not (is_closed w));
    let next_deferred = Stream.next s in
    match Deferred.peek next_deferred with
    | Some next -> loop_next next
    | None ->
      transfer ();
      upon next_deferred check_closed_loop_next
  and check_closed_loop_next next = if not (is_closed w) then loop_next next
  and loop_next = function
    | Nil ->
      transfer ();
      close w
    | Cons (x, s) ->
      Q.enqueue q x;
      loop s
  in
  loop s;
  r
;;

let transfer_gen
      (read_now : ?consumer:Consumer.t -> _ Reader.t -> _)
      write
      input
      output
      ~f
  =
  if !check_invariant
  then (
    invariant input;
    invariant output);
  let link = Link.create ~upstream:input ~downstream:output in
  let consumer = Link.consumer link in
  Monitor.protect
    (* When result is filled, we're done with [input].  We unlink to remove pointers from
       [output] to [input], which would cause a space leak if we had single long-lived
       output into which we transfer lots of short-lived inputs. *)
    ~finally:(fun () ->
      Link.unlink_upstream link;
      return ())
    (fun () ->
       Deferred.create (fun result ->
         (* We do [return () >>>] to ensure that [f] is only called asynchronously. *)
         return ()
         >>> fun () ->
         let output_closed () =
           close_read input;
           Ivar.fill result ()
         in
         let rec loop () =
           if is_closed output
           then output_closed ()
           else (
             match read_now input ~consumer with
             | `Eof -> Ivar.fill result ()
             | `Ok x -> f x continue
             | `Nothing_available ->
               choose
                 [ choice (values_available input) ignore
                 ; choice (closed output) ignore
                 ]
               >>> fun () -> loop ())
         and continue y =
           if is_closed output
           then output_closed ()
           else (
             let pushback = write output y in
             Consumer.values_sent_downstream consumer;
             pushback >>> fun () -> loop ())
         in
         loop ()))
;;

let transfer' ?max_queue_length input output ~f =
  transfer_gen (read_now' ?max_queue_length) write' input output ~f:(fun q k ->
    f q >>> k)
;;

let transfer input output ~f =
  transfer_gen read_now write input output ~f:(fun a k -> k (f a))
;;

let transfer_id ?max_queue_length input output =
  transfer_gen (read_now' ?max_queue_length) write' input output ~f:(fun q k -> k q)
;;

let map_gen read write input ~f =
  let result, output = create () in
  upon (transfer_gen read write input output ~f) (fun () -> close output);
  result
;;

let map' ?max_queue_length input ~f =
  map_gen (read_now' ?max_queue_length) write' input ~f:(fun q k -> f q >>> k)
;;

let map input ~f = map_gen read_now write input ~f:(fun a k -> k (f a))

let filter_map' ?max_queue_length input ~f =
  map' ?max_queue_length input ~f:(fun q -> Deferred.Queue.filter_map q ~f)
;;

let filter_map ?max_queue_length input ~f =
  map_gen (read_now' ?max_queue_length) write' input ~f:(fun q k ->
    k (Queue.filter_map q ~f:(fun x -> if is_read_closed input then None else f x)))
;;

let folding_filter_map ?max_queue_length input ~init ~f =
  let accum = ref init in
  filter_map ?max_queue_length input ~f:(fun x ->
    let a, x = f !accum x in
    accum := a;
    x)
;;

let fold_filter_map = folding_filter_map

let folding_map ?max_queue_length input ~init ~f =
  fold_filter_map ?max_queue_length input ~init ~f:(fun accum a ->
    let accum, b = f accum a in
    accum, Some b)
;;

let fold_map = folding_map
let filter input ~f = filter_map input ~f:(fun x -> if f x then Some x else None)

let of_list l =
  let t = create_internal ~initial_buffer:(Q.of_list l) in
  Ivar.fill t.closed ();
  update_pushback t;
  t
;;

let singleton x =
  let reader, writer = create () in
  write_without_pushback writer x;
  close writer;
  reader
;;

let unfold ~init:s ~f =
  (* To get some batching, we run the continuation immediately if the deferred is
     determined.  However, we always check for pushback.  Because size budget can't be
     infinite, the below loop is guaranteed to eventually yield to the scheduler. *)
  let ( >>=~ ) d f =
    match Deferred.peek d with
    | None -> d >>= f
    | Some x -> f x
  in
  create_reader ~close_on_exception:false (fun writer ->
    let rec loop s =
      f s
      >>=~ function
      | None -> return ()
      | Some (a, s) ->
        if is_closed writer then return () else write writer a >>=~ fun () -> loop s
    in
    loop s)
;;

let of_sequence sequence =
  create_reader ~close_on_exception:false (fun writer ->
    let rec enqueue_n sequence i =
      if i <= 0
      then sequence
      else (
        match Sequence.next sequence with
        | None -> sequence
        | Some (a, sequence) ->
          Queue.enqueue writer.buffer a;
          enqueue_n sequence (i - 1))
    in
    let rec loop sequence =
      if is_closed writer || Sequence.is_empty sequence
      then return ()
      else (
        start_write writer;
        let sequence = enqueue_n sequence (1 + writer.size_budget - length writer) in
        finish_write writer;
        let%bind () = pushback writer in
        loop sequence)
    in
    loop sequence)
;;

type 'a to_sequence_elt =
  | Value of 'a
  | Wait_for : _ Deferred.t -> _ to_sequence_elt

let to_sequence t =
  Sequence.unfold ~init:() ~f:(fun () ->
    match read_now t with
    | `Eof -> None
    | `Ok a -> Some (Value a, ())
    | `Nothing_available -> Some (Wait_for (values_available t), ()))
;;

let interleave_pipe inputs =
  let output, output_writer = create () in
  (* We keep a reference count of all the pipes that [interleave_pipe] is managing;
     [inputs] counts as one.  When the reference count drops to zero, we know that all
     pipes are closed and we can close [output_writer]. *)
  let num_pipes_remaining = ref 1 in
  let decr_num_pipes_remaining () =
    decr num_pipes_remaining;
    if !num_pipes_remaining = 0 then close output_writer
  in
  don't_wait_for
    (let%map () =
       iter_without_pushback inputs ~f:(fun input ->
         incr num_pipes_remaining;
         don't_wait_for
           (let%map () = transfer_id input output_writer in
            decr_num_pipes_remaining ()))
     in
     decr_num_pipes_remaining ());
  (* for [inputs] *)
  output
;;

let interleave inputs =
  if !check_invariant then List.iter inputs ~f:invariant;
  interleave_pipe (of_list inputs)
;;

let merge inputs ~compare =
  let module Heap = Pairing_heap in
  let r, w = create () in
  upon (closed w) (fun () -> List.iter inputs ~f:close_read);
  let heap = Heap.create ~cmp:(fun (a1, _) (a2, _) -> compare a1 a2) () in
  let handle_read input eof_or_ok =
    match eof_or_ok with
    | `Eof -> ()
    | `Ok v -> Heap.add heap (v, input)
  in
  let rec pop_heap_and_loop () =
    (* At this point, all inputs not at Eof occur in [heap] exactly once, so we know what
       the next output element is.  [pop_heap_and_loop] repeatedly takes elements from the
       inputs as long as it has one from each input.  This is done synchronously to avoid
       the cost of a deferred for each element of the output -- there's no need to
       pushback since that is only moving elements from one pipe to another.  As soon as
       [pop_heap_and_loop] can't get an element from some input, it waits on pushback from
       the output, since it has to wait on the input anyway.  This also prevents [merge]
       from consuming inputs at a rate faster than its output is consumed. *)
    match Heap.pop heap with
    | None -> close w
    | Some (v, input) ->
      if not (is_closed w)
      then (
        write_without_pushback w v;
        if Heap.length heap = 0
        then upon (transfer_id input w) (fun () -> close w)
        else (
          match read_now input with
          | (`Eof | `Ok _) as x ->
            handle_read input x;
            pop_heap_and_loop ()
          | `Nothing_available ->
            pushback w
            >>> fun () ->
            read input
            >>> fun x ->
            handle_read input x;
            pop_heap_and_loop ()))
  in
  let initial_push =
    Deferred.List.iter inputs ~f:(fun input ->
      let%map x = read input in
      handle_read input x)
  in
  upon initial_push pop_heap_and_loop;
  r
;;

let concat inputs =
  let r, w = create () in
  upon
    (Deferred.List.iter inputs ~f:(fun input -> transfer_id input w))
    (fun () -> close w);
  r
;;

let fork t ~pushback_uses =
  let reader0, writer0 = create () in
  let reader1, writer1 = create () in
  let some_reader_was_closed = ref false in
  let consumer =
    add_consumer t ~downstream_flushed:(fun () ->
      let some_reader_was_closed = !some_reader_was_closed in
      match%map
        Flushed_result.combine
          [ downstream_flushed writer0; downstream_flushed writer1 ]
      with
      | `Reader_closed -> `Reader_closed
      | `Ok ->
        (* In this case, there could have been no pending items in [writer0] nor in
           [writer1], in which case we could have had a closed pipe that missed some
           writes, but [Flushed_result.combine] would still have returned [`Ok] *)
        if some_reader_was_closed then `Reader_closed else `Ok)
  in
  don't_wait_for
    (let still_open = [ writer0; writer1 ] in
     let filter_open still_open =
       (* Only call [filter] and reallocate list if something will get filtered *)
       if not (List.exists still_open ~f:is_closed)
       then still_open
       else (
         some_reader_was_closed := true;
         let still_open = List.filter still_open ~f:(fun w -> not (is_closed w)) in
         if List.is_empty still_open then close t;
         still_open)
     in
     let%bind still_open =
       fold' t ~flushed:(Consumer consumer) ~init:still_open ~f:(fun still_open queue ->
         let still_open = filter_open still_open in
         if List.is_empty still_open
         then return []
         else (
           let%map () =
             match pushback_uses with
             | `Fast_consumer_only -> Deferred.any (List.map still_open ~f:pushback)
             | `Both_consumers -> Deferred.all_unit (List.map still_open ~f:pushback)
           in
           let still_open = filter_open still_open in
           List.iter still_open ~f:(fun w -> copy_in_without_pushback w ~from:queue);
           still_open))
     in
     List.iter still_open ~f:close;
     return ());
  reader0, reader1
;;
