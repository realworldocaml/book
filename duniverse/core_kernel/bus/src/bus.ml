open! Core_kernel

module State = struct
  type t =
    | Closed
    | Write_in_progress
    | Ok_to_write
  [@@deriving sexp_of]

  let is_closed = function
    | Closed -> true
    | Write_in_progress -> false
    | Ok_to_write -> false
  ;;
end

module Callback_arity = struct
  type _ t =
    | Arity1 : ('a -> unit) t
    | Arity2 : ('a -> 'b -> unit) t
    | Arity3 : ('a -> 'b -> 'c -> unit) t
    | Arity4 : ('a -> 'b -> 'c -> 'd -> unit) t
    | Arity5 : ('a -> 'b -> 'c -> 'd -> 'e -> unit) t
  [@@deriving sexp_of]
end

module On_subscription_after_first_write = struct
  type t =
    | Allow
    | Allow_and_send_last_value
    | Raise
  [@@deriving sexp_of]

  let allow_subscription_after_first_write = function
    | Allow -> true
    | Allow_and_send_last_value -> true
    | Raise -> false
  ;;
end

module Last_value : sig
  type 'callback t

  val create : 'callback Callback_arity.t -> 'callback t
  val set1 : ('a -> unit) t -> 'a -> unit
  val set2 : ('a -> 'b -> unit) t -> 'a -> 'b -> unit
  val set3 : ('a -> 'b -> 'c -> unit) t -> 'a -> 'b -> 'c -> unit
  val set4 : ('a -> 'b -> 'c -> 'd -> unit) t -> 'a -> 'b -> 'c -> 'd -> unit
  val set5 : ('a -> 'b -> 'c -> 'd -> 'e -> unit) t -> 'a -> 'b -> 'c -> 'd -> 'e -> unit
  val send : 'callback t -> 'callback -> unit
end = struct
  type _ tuple =
    | Tuple1 : { mutable arg1 : 'a } -> ('a -> unit) tuple
    | Tuple2 :
        { mutable arg1 : 'a
        ; mutable arg2 : 'b
        }
        -> ('a -> 'b -> unit) tuple
    | Tuple3 :
        { mutable arg1 : 'a
        ; mutable arg2 : 'b
        ; mutable arg3 : 'c
        }
        -> ('a -> 'b -> 'c -> unit) tuple
    | Tuple4 :
        { mutable arg1 : 'a
        ; mutable arg2 : 'b
        ; mutable arg3 : 'c
        ; mutable arg4 : 'd
        }
        -> ('a -> 'b -> 'c -> 'd -> unit) tuple
    | Tuple5 :
        { mutable arg1 : 'a
        ; mutable arg2 : 'b
        ; mutable arg3 : 'c
        ; mutable arg4 : 'd
        ; mutable arg5 : 'e
        }
        -> ('a -> 'b -> 'c -> 'd -> 'e -> unit) tuple

  type 'callback t = 'callback tuple option ref

  let create (type callback) (_arity : callback Callback_arity.t) : callback t = ref None

  let set1 t a =
    match !t with
    | None -> t := Some (Tuple1 { arg1 = a })
    | Some (Tuple1 args) -> args.arg1 <- a
  ;;

  let set2 t a b =
    match !t with
    | None -> t := Some (Tuple2 { arg1 = a; arg2 = b })
    | Some (Tuple2 args) ->
      args.arg1 <- a;
      args.arg2 <- b
  ;;

  let set3 t a b c =
    match !t with
    | None -> t := Some (Tuple3 { arg1 = a; arg2 = b; arg3 = c })
    | Some (Tuple3 args) ->
      args.arg1 <- a;
      args.arg2 <- b;
      args.arg3 <- c
  ;;

  let set4 t a b c d =
    match !t with
    | None -> t := Some (Tuple4 { arg1 = a; arg2 = b; arg3 = c; arg4 = d })
    | Some (Tuple4 args) ->
      args.arg1 <- a;
      args.arg2 <- b;
      args.arg3 <- c;
      args.arg4 <- d
  ;;

  let set5 t arg1 arg2 arg3 arg4 arg5 =
    match !t with
    | None -> t := Some (Tuple5 { arg1; arg2; arg3; arg4; arg5 })
    | Some (Tuple5 args) ->
      args.arg1 <- arg1;
      args.arg2 <- arg2;
      args.arg3 <- arg3;
      args.arg4 <- arg4;
      args.arg5 <- arg5
  ;;

  let send (type callback) (t : callback t) (callback : callback) : unit =
    match !t with
    | None -> ()
    | Some (Tuple1 { arg1 }) -> callback arg1
    | Some (Tuple2 { arg1; arg2 }) -> callback arg1 arg2
    | Some (Tuple3 { arg1; arg2; arg3 }) -> callback arg1 arg2 arg3
    | Some (Tuple4 { arg1; arg2; arg3; arg4 }) -> callback arg1 arg2 arg3 arg4
    | Some (Tuple5 { arg1; arg2; arg3; arg4; arg5 }) -> callback arg1 arg2 arg3 arg4 arg5
  ;;
end

module Bus_id = Unique_id.Int63 ()

module Subscriber = struct
  type 'callback t =
    { bus_id : Bus_id.t
    ; callback : 'callback
    ; extract_exn : bool
    ; (* [subscribers_index] is the index of this subscriber in the bus's [subscribers]
         array.  [-1] indicates that this subscriber is not subscribed. *)
      mutable subscribers_index : int
    ; on_callback_raise : (Error.t -> unit) option
    ; on_close : (unit -> unit) option
    ; subscribed_from : Source_code_position.t
    }
  [@@deriving fields]

  let is_subscribed t ~to_ = t.subscribers_index >= 0 && Bus_id.equal t.bus_id to_

  let sexp_of_t
        _
        { callback = _
        ; bus_id = _
        ; extract_exn
        ; subscribers_index
        ; on_callback_raise
        ; on_close = _
        ; subscribed_from
        }
    : Sexp.t
    =
    List
      [ Atom "Bus.Subscriber.t"
      ; [%message
        ""
          ~subscribers_index:
            (if am_running_inline_test then None else Some subscribers_index
                                                      : (int option[@sexp.option]))
          (on_callback_raise : ((Error.t -> unit) option[@sexp.option]))
          ~extract_exn:
            (if extract_exn then Some true else None : (bool option[@sexp.option]))
          (subscribed_from : Source_code_position.t)]
      ]
  ;;

  let invariant invariant_a t =
    Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
      let check f = Invariant.check_field t f in
      Fields.iter
        ~bus_id:ignore
        ~callback:(check invariant_a)
        ~extract_exn:ignore
        ~subscribers_index:ignore
        ~on_callback_raise:ignore
        ~on_close:ignore
        ~subscribed_from:ignore)
  ;;

  let create
        subscribed_from
        ~callback
        ~bus_id
        ~extract_exn
        ~subscribers_index
        ~on_callback_raise
        ~on_close
    =
    { bus_id
    ; callback
    ; extract_exn
    ; subscribers_index
    ; on_callback_raise
    ; on_close
    ; subscribed_from
    }
  ;;
end

type ('callback, 'phantom) t =
  { bus_id : Bus_id.t
  ; name : Info.t option
  ; callback_arity : 'callback Callback_arity.t
  ; created_from : Source_code_position.t
  ; on_subscription_after_first_write : On_subscription_after_first_write.t
  ; on_callback_raise : Error.t -> unit
  ; last_value : 'callback Last_value.t option
  ; mutable state : State.t
  ; mutable write_ever_called : bool
  ; mutable num_subscribers : int
  ; (* [subscribers] contains all subscribers to the bus, in a contiguous prefix from
       index [0] to [num_subscribers - 1]. *)
    mutable subscribers : 'callback Subscriber.t Option_array.t
  ; (* [callbacks] holds the callbacks of the corresponding entries of [subscribers]. *)
    mutable callbacks : 'callback Option_array.t
  ; mutable unsubscribes_during_write : 'callback Subscriber.t list
  }
[@@deriving fields]

let sexp_of_t
      _
      _
      { bus_id = _
      ; callback_arity
      ; callbacks = _
      ; created_from
      ; last_value = _
      ; name
      ; num_subscribers
      ; on_subscription_after_first_write
      ; on_callback_raise = _
      ; state
      ; subscribers
      ; write_ever_called
      ; unsubscribes_during_write = _
      }
  =
  let subscribers =
    Array.init num_subscribers ~f:(fun i -> Option_array.get_some_exn subscribers i)
  in
  [%message
    ""
      (name : (Info.t option[@sexp.option]))
      (callback_arity : _ Callback_arity.t)
      (created_from : Source_code_position.t)
      (on_subscription_after_first_write : On_subscription_after_first_write.t)
      (state : State.t)
      (write_ever_called : bool)
      (subscribers : _ Subscriber.t Array.t)]
;;

type ('callback, 'phantom) bus = ('callback, 'phantom) t [@@deriving sexp_of]

let read_only t = (t :> (_, read) t)

let invariant invariant_a _ t =
  Invariant.invariant [%here] t [%sexp_of: (_, _) t] (fun () ->
    let check f = Invariant.check_field t f in
    Fields.iter
      ~bus_id:ignore
      ~name:ignore
      ~callbacks:
        (check (fun callbacks ->
           assert (Option_array.length callbacks = Option_array.length t.subscribers);
           for i = 0 to Option_array.length callbacks - 1 do
             if i < t.num_subscribers
             then invariant_a (Option_array.get_some_exn callbacks i)
             else assert (Option_array.is_none callbacks i)
           done))
      ~callback_arity:ignore
      ~created_from:ignore
      ~num_subscribers:(check (fun num_subscribers -> assert (num_subscribers >= 0)))
      ~on_subscription_after_first_write:ignore
      ~on_callback_raise:ignore
      ~last_value:ignore
      ~state:ignore
      ~write_ever_called:ignore
      ~subscribers:
        (check (fun subscribers ->
           for i = 0 to Option_array.length subscribers - 1 do
             if i < t.num_subscribers
             then (
               let subscriber = Option_array.get_some_exn subscribers i in
               Subscriber.invariant invariant_a subscriber;
               assert (i = subscriber.subscribers_index))
             else assert (Option_array.is_none subscribers i)
           done))
      ~unsubscribes_during_write:ignore)
;;

let is_closed t = State.is_closed t.state

module Read_write = struct
  type 'callback t = ('callback, read_write) bus [@@deriving sexp_of]

  let invariant invariant_a t = invariant invariant_a ignore t
end

module Read_only = struct
  type 'callback t = ('callback, read) bus [@@deriving sexp_of]

  let invariant invariant_a t = invariant invariant_a ignore t
end

let[@cold] start_write_failing t =
  match t.state with
  | Closed ->
    failwiths ~here:[%here] "[Bus.write] called on closed bus" t [%sexp_of: (_, _) t]
  | Write_in_progress ->
    failwiths
      ~here:[%here]
      "[Bus.write] called from callback on the same bus"
      t
      [%sexp_of: (_, _) t]
  | Ok_to_write -> assert false
;;

let capacity t = Option_array.length t.subscribers

let maybe_shrink_capacity t =
  if t.num_subscribers * 4 <= capacity t
  then (
    let desired_capacity = t.num_subscribers in
    let copy_and_shrink array =
      let new_array = Option_array.create ~len:desired_capacity in
      Option_array.blit
        ~src:array
        ~src_pos:0
        ~dst:new_array
        ~dst_pos:0
        ~len:t.num_subscribers;
      new_array
    in
    t.subscribers <- copy_and_shrink t.subscribers;
    t.callbacks <- copy_and_shrink t.callbacks)
;;

let add_subscriber t (subscriber : _ Subscriber.t) ~at_subscribers_index =
  subscriber.subscribers_index <- at_subscribers_index;
  Option_array.set_some t.subscribers at_subscribers_index subscriber;
  Option_array.set_some t.callbacks at_subscribers_index subscriber.callback
;;

let remove_subscriber t (subscriber : _ Subscriber.t) =
  let subscribers_index = subscriber.subscribers_index in
  subscriber.subscribers_index <- -1;
  Option_array.set_none t.subscribers subscribers_index;
  Option_array.set_none t.callbacks subscribers_index
;;

let unsubscribe_assuming_valid_subscriber t (subscriber : _ Subscriber.t) =
  let subscriber_index = subscriber.subscribers_index in
  let last_subscriber_index = t.num_subscribers - 1 in
  remove_subscriber t subscriber;
  if subscriber_index < last_subscriber_index
  then (
    let last_subscriber =
      Option_array.get_some_exn t.subscribers last_subscriber_index
    in
    remove_subscriber t last_subscriber;
    add_subscriber t last_subscriber ~at_subscribers_index:subscriber_index);
  t.num_subscribers <- t.num_subscribers - 1;
  maybe_shrink_capacity t
;;

let unsubscribe t subscriber =
  if Subscriber.is_subscribed subscriber ~to_:t.bus_id
  then (
    match t.state with
    | Write_in_progress ->
      t.unsubscribes_during_write <- subscriber :: t.unsubscribes_during_write
    | Closed ->
      (* This can happen if during [write], [unsubscribe] is called after [close].  We
         don't do anything here because all subscribers will be unsubscribed after the
         [write] finishes. *)
      ()
    | Ok_to_write -> unsubscribe_assuming_valid_subscriber t subscriber)
;;

let[@cold] unsubscribe_after_finish_write t =
  List.iter t.unsubscribes_during_write ~f:(unsubscribe_assuming_valid_subscriber t);
  t.unsubscribes_during_write <- []
;;

let[@cold] unsubscribe_all t =
  assert (is_closed t);
  for i = 0 to t.num_subscribers - 1 do
    let subscriber = Option_array.get_some_exn t.subscribers i in
    Option.iter subscriber.on_close ~f:(fun on_close -> on_close ());
    remove_subscriber t subscriber
  done;
  t.num_subscribers <- 0;
  maybe_shrink_capacity t
;;

let[@inline always] finish_write t =
  if not (List.is_empty t.unsubscribes_during_write)
  then unsubscribe_after_finish_write t;
  match t.state with
  | Closed -> unsubscribe_all t
  | Ok_to_write -> assert false
  | Write_in_progress -> t.state <- Ok_to_write
;;

let[@cold] close t =
  match t.state with
  | Closed -> ()
  | Write_in_progress -> t.state <- Closed
  | Ok_to_write ->
    t.state <- Closed;
    unsubscribe_all t
;;

let call_on_callback_raise t error =
  try t.on_callback_raise error with
  | exn ->
    close t;
    raise exn
;;

let callback_raised t i exn =
  let backtrace = Backtrace.Exn.most_recent () in
  (* [i] was incremented before the callback was called, so we have to subtract one
     here.  We do this here, rather than at the call site, because there are multiple
     call sites due to the optimizations needed to keep this zero-alloc. *)
  let subscriber = Option_array.get_some_exn t.subscribers (i - 1) in
  let error =
    [%message
      "Bus subscriber raised"
        (exn : exn)
        (backtrace : Backtrace.t)
        (subscriber : _ Subscriber.t)]
    |> [%of_sexp: Error.t]
  in
  match subscriber.on_callback_raise with
  | None -> call_on_callback_raise t error
  | Some f ->
    let error = if subscriber.extract_exn then Error.of_exn exn else error in
    (try f error with
     | exn ->
       let backtrace = Backtrace.Exn.most_recent () in
       call_on_callback_raise
         t
         (let original_error = error in
          [%message
            "Bus subscriber's [on_callback_raise] raised"
              (exn : exn)
              (backtrace : Backtrace.t)
              (original_error : Error.t)]
          |> [%of_sexp: Error.t]))
;;

let[@inline always] unsafe_get_callback a i =
  (* We considered using [Option_array.get_some_exn] and
     [Option_array.unsafe_get_some_exn] here, but both are significantly slower.  Check
     the write benchmarks in [bench_bus.ml] before changing this. *)
  Option_array.unsafe_get_some_assuming_some a i
;;

let write_non_optimized t callbacks a1 =
  let len = t.num_subscribers in
  let i = ref 0 in
  while !i < len do
    try
      let callback = unsafe_get_callback callbacks !i in
      incr i;
      callback a1
    with
    | exn -> callback_raised t !i exn
  done;
  finish_write t
;;

let write2_non_optimized t callbacks a1 a2 =
  let len = t.num_subscribers in
  let i = ref 0 in
  while !i < len do
    try
      let callback = unsafe_get_callback callbacks !i in
      incr i;
      callback a1 a2
    with
    | exn -> callback_raised t !i exn
  done;
  finish_write t
;;

let write3_non_optimized t callbacks a1 a2 a3 =
  let len = t.num_subscribers in
  let i = ref 0 in
  while !i < len do
    try
      let callback = unsafe_get_callback callbacks !i in
      incr i;
      callback a1 a2 a3
    with
    | exn -> callback_raised t !i exn
  done;
  finish_write t
;;

let write4_non_optimized t callbacks a1 a2 a3 a4 =
  let len = t.num_subscribers in
  let i = ref 0 in
  while !i < len do
    try
      let callback = unsafe_get_callback callbacks !i in
      incr i;
      callback a1 a2 a3 a4
    with
    | exn -> callback_raised t !i exn
  done;
  finish_write t
;;

let write5_non_optimized t callbacks a1 a2 a3 a4 a5 =
  let len = t.num_subscribers in
  let i = ref 0 in
  while !i < len do
    try
      let callback = unsafe_get_callback callbacks !i in
      incr i;
      callback a1 a2 a3 a4 a5
    with
    | exn -> callback_raised t !i exn
  done;
  finish_write t
;;

(* The [write_N] functions are written to minimise registers live across function calls
   (these have to be spilled).  They are also annotated for partial inlining (the
   one-callback case becomes inlined whereas the >1-callback-case requires a further
   direct call). *)

let[@inline always] write t a1 =
  let callbacks = t.callbacks in
  t.write_ever_called <- true;
  match t.state with
  | Closed | Write_in_progress -> start_write_failing t
  | Ok_to_write ->
    t.state <- Write_in_progress;
    if t.num_subscribers = 1
    then (
      (try (unsafe_get_callback callbacks 0) a1 with
       | exn -> callback_raised t 1 exn);
      finish_write t)
    else (write_non_optimized [@inlined never]) t callbacks a1;
    (match t.last_value with
     | None -> ()
     | Some last_value -> Last_value.set1 last_value a1)
;;

let[@inline always] write2 t a1 a2 =
  let callbacks = t.callbacks in
  t.write_ever_called <- true;
  match t.state with
  | Closed | Write_in_progress -> start_write_failing t
  | Ok_to_write ->
    t.state <- Write_in_progress;
    if t.num_subscribers = 1
    then (
      (try (unsafe_get_callback callbacks 0) a1 a2 with
       | exn -> callback_raised t 1 exn);
      finish_write t)
    else (write2_non_optimized [@inlined never]) t callbacks a1 a2;
    (match t.last_value with
     | None -> ()
     | Some last_value -> Last_value.set2 last_value a1 a2)
;;

let[@inline always] write3 t a1 a2 a3 =
  let callbacks = t.callbacks in
  t.write_ever_called <- true;
  match t.state with
  | Closed | Write_in_progress -> start_write_failing t
  | Ok_to_write ->
    t.state <- Write_in_progress;
    if t.num_subscribers = 1
    then (
      (try (unsafe_get_callback callbacks 0) a1 a2 a3 with
       | exn -> callback_raised t 1 exn);
      finish_write t)
    else (write3_non_optimized [@inlined never]) t callbacks a1 a2 a3;
    (match t.last_value with
     | None -> ()
     | Some last_value -> Last_value.set3 last_value a1 a2 a3)
;;

let[@inline always] write4 t a1 a2 a3 a4 =
  let callbacks = t.callbacks in
  t.write_ever_called <- true;
  match t.state with
  | Closed | Write_in_progress -> start_write_failing t
  | Ok_to_write ->
    t.state <- Write_in_progress;
    if t.num_subscribers = 1
    then (
      (try (unsafe_get_callback callbacks 0) a1 a2 a3 a4 with
       | exn -> callback_raised t 1 exn);
      finish_write t)
    else (write4_non_optimized [@inlined never]) t callbacks a1 a2 a3 a4;
    (match t.last_value with
     | None -> ()
     | Some last_value -> Last_value.set4 last_value a1 a2 a3 a4)
;;

let[@inline always] write5 t a1 a2 a3 a4 a5 =
  let callbacks = t.callbacks in
  t.write_ever_called <- true;
  match t.state with
  | Closed | Write_in_progress -> start_write_failing t
  | Ok_to_write ->
    t.state <- Write_in_progress;
    if t.num_subscribers = 1
    then (
      (try (unsafe_get_callback callbacks 0) a1 a2 a3 a4 a5 with
       | exn -> callback_raised t 1 exn);
      finish_write t)
    else (write5_non_optimized [@inlined never]) t callbacks a1 a2 a3 a4 a5;
    (match t.last_value with
     | None -> ()
     | Some last_value -> Last_value.set5 last_value a1 a2 a3 a4 a5)
;;

let allow_subscription_after_first_write t =
  On_subscription_after_first_write.allow_subscription_after_first_write
    t.on_subscription_after_first_write
;;

let create
      ?name
      created_from
      callback_arity
      ~(on_subscription_after_first_write : On_subscription_after_first_write.t)
      ~on_callback_raise
  =
  let last_value =
    match on_subscription_after_first_write with
    | Allow_and_send_last_value -> Some (Last_value.create callback_arity)
    | Allow -> None
    | Raise -> None
  in
  { bus_id = Bus_id.create ()
  ; name
  ; callback_arity
  ; created_from
  ; num_subscribers = 0
  ; on_subscription_after_first_write
  ; on_callback_raise
  ; last_value
  ; subscribers = Option_array.create ~len:0
  ; callbacks = Option_array.create ~len:0
  ; state = Ok_to_write
  ; write_ever_called = false
  ; unsubscribes_during_write = []
  }
;;

let can_subscribe t = allow_subscription_after_first_write t || not t.write_ever_called

let enlarge_capacity t =
  let capacity = capacity t in
  let new_capacity = Int.max 1 (capacity * 2) in
  let copy_and_double array =
    let new_array = Option_array.create ~len:new_capacity in
    Option_array.blit ~src:array ~src_pos:0 ~dst:new_array ~dst_pos:0 ~len:capacity;
    new_array
  in
  t.subscribers <- copy_and_double t.subscribers;
  t.callbacks <- copy_and_double t.callbacks
;;

let subscribe_exn
      ?(extract_exn = false)
      ?on_callback_raise
      ?on_close
      t
      subscribed_from
      ~f:callback
  =
  if not (can_subscribe t)
  then
    failwiths
      ~here:[%here]
      "Bus.subscribe_exn called after first write"
      [%sexp ~~(subscribed_from : Source_code_position.t), { bus = (t : (_, _) t) }]
      [%sexp_of: Sexp.t];
  match t.state with
  | Closed ->
    (* Anything that satisfies the return type will do.  Since the subscriber is never
       stored in the arrays, the [on_close] callback will never be called. *)
    Subscriber.create
      subscribed_from
      ~bus_id:t.bus_id
      ~callback
      ~extract_exn
      ~subscribers_index:(-1)
      ~on_callback_raise
      ~on_close
  | Ok_to_write | Write_in_progress ->
    (* The code below side effects [t], which potentially could interfere with a write in
       progress.  However, the side effects don't change the prefix of [t.callbacks] that
       write uses; they only change [t.callbacks] beyond that prefix.  And all writes
       extract [t.num_subscribers] at the start, so that they will not see any subsequent
       changes to it. *)
    let subscriber =
      Subscriber.create
        subscribed_from
        ~bus_id:t.bus_id
        ~callback
        ~extract_exn
        ~subscribers_index:t.num_subscribers
        ~on_callback_raise
        ~on_close
    in
    if capacity t = t.num_subscribers then enlarge_capacity t;
    add_subscriber t subscriber ~at_subscribers_index:t.num_subscribers;
    t.num_subscribers <- t.num_subscribers + 1;
    (match t.last_value with
     | None -> ()
     | Some last_value -> Last_value.send last_value callback);
    subscriber
;;

let iter_exn t subscribed_from ~f =
  if not (can_subscribe t)
  then
    failwiths
      ~here:[%here]
      "Bus.iter_exn called after first write"
      t
      [%sexp_of: (_, _) t];
  ignore (subscribe_exn t subscribed_from ~f : _ Subscriber.t)
;;

module Fold_arity = struct
  type (_, _, _) t =
    | Arity1 : ('a -> unit, 's -> 'a -> 's, 's) t
    | Arity2 : ('a -> 'b -> unit, 's -> 'a -> 'b -> 's, 's) t
    | Arity3 : ('a -> 'b -> 'c -> unit, 's -> 'a -> 'b -> 'c -> 's, 's) t
    | Arity4 : ('a -> 'b -> 'c -> 'd -> unit, 's -> 'a -> 'b -> 'c -> 'd -> 's, 's) t
    | Arity5
      : ( 'a -> 'b -> 'c -> 'd -> 'e -> unit
        , 's -> 'a -> 'b -> 'c -> 'd -> 'e -> 's
        , 's )
          t
  [@@deriving sexp_of]
end

let fold_exn
      (type c f s)
      (t : (c, _) t)
      subscribed_from
      (fold_arity : (c, f, s) Fold_arity.t)
      ~(init : s)
      ~(f : f)
  =
  let state = ref init in
  if not (can_subscribe t)
  then
    failwiths
      ~here:[%here]
      "Bus.fold_exn called after first write"
      t
      [%sexp_of: (_, _) t];
  let module A = Fold_arity in
  iter_exn
    t
    subscribed_from
    ~f:
      (match fold_arity with
       | Arity1 -> fun a1 -> state := f !state a1
       | Arity2 -> fun a1 a2 -> state := f !state a1 a2
       | Arity3 -> fun a1 a2 a3 -> state := f !state a1 a2 a3
       | Arity4 -> fun a1 a2 a3 a4 -> state := f !state a1 a2 a3 a4
       | Arity5 -> fun a1 a2 a3 a4 a5 -> state := f !state a1 a2 a3 a4 a5)
;;

let%test_module _ =
  (module struct
    let assert_no_allocation bus callback write =
      let bus_r = read_only bus in
      ignore (subscribe_exn bus_r [%here] ~f:callback : _ Subscriber.t);
      let starting_minor_words = Gc.minor_words () in
      let starting_major_words = Gc.major_words () in
      write ();
      let ending_minor_words = Gc.minor_words () in
      let ending_major_words = Gc.major_words () in
      [%test_result: int] (ending_minor_words - starting_minor_words) ~expect:0;
      [%test_result: int] (ending_major_words - starting_major_words) ~expect:0
    ;;

    (* This test only works when [write] is properly inlined.  It does not guarantee that
       [write] never allocates in any situation.  For example, if this test is moved to
       another library and run with X_LIBRARY_INLINING=false, it fails. *)
    let%test_unit "write doesn't allocate when inlined" =
      let create created_from arity =
        create
          created_from
          arity
          ~on_subscription_after_first_write:Raise
          ~on_callback_raise:Error.raise
      in
      let bus1 = create [%here] Arity1 in
      let bus2 = create [%here] Arity2 in
      let bus3 = create [%here] Arity3 in
      let bus4 = create [%here] Arity4 in
      let bus5 = create [%here] Arity5 in
      assert_no_allocation bus1 (fun () -> ()) (fun () -> write bus1 ());
      assert_no_allocation bus2 (fun () () -> ()) (fun () -> write2 bus2 () ());
      assert_no_allocation bus3 (fun () () () -> ()) (fun () -> write3 bus3 () () ());
      assert_no_allocation
        bus4
        (fun () () () () -> ())
        (fun () -> write4 bus4 () () () ());
      assert_no_allocation
        bus5
        (fun () () () () () -> ())
        (fun () -> write5 bus5 () () () () ())
    ;;
  end)
;;
