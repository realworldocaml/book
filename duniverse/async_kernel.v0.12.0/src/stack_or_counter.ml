open! Base

type _ t =
  | Stack : 'a Stack.t -> 'a t
  | Counter : { mutable length : int } -> unit t
[@@deriving sexp_of]

let of_list list = Stack (Stack.of_list list)

let create_counter ~length =
  if length < 0
  then
    raise_s
      [%message "[Stack_or_counter.create_counter] got negative length" (length : int)];
  Counter { length }
;;

let length (type a) (t : a t) =
  match t with
  | Stack s -> Stack.length s
  | Counter r -> r.length
;;

let clear (type a) (t : a t) =
  match t with
  | Stack s -> Stack.clear s
  | Counter r -> r.length <- 0
;;

let push (type a) (t : a t) a =
  match t with
  | Stack s -> Stack.push s a
  | Counter r -> r.length <- r.length + 1
;;

let pop_exn (type a) (t : a t) =
  match t with
  | Stack s -> Stack.pop_exn s
  | Counter r ->
    if r.length = 0 then raise_s [%message "[Stack_or_counter.pop_exn] of empty stack"];
    r.length <- r.length - 1
;;

let iter (type a) (t : a t) ~(f : a -> unit) =
  match t with
  | Stack s -> Stack.iter s ~f
  | Counter r ->
    for _ = 1 to r.length do
      f ()
    done
;;
