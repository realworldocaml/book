open! Import
include Stack_intf

let raise_s = Error.raise_s

(* This implementation is similar to [Deque] in that it uses an array of ['a] and
   a mutable [int] to indicate what in the array is used.  We choose to implement [Stack]
   directly rather than on top of [Deque] for performance reasons.  E.g. a simple
   microbenchmark shows that push/pop is about 20% faster. *)
type 'a t =
  { mutable length : int
  ; mutable elts : 'a Option_array.t
  }
[@@deriving_inline sexp_of]

let sexp_of_t : 'a. ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t =
  fun _of_a -> function
    | { length = v_length; elts = v_elts } ->
      let bnds = [] in
      let bnds =
        let arg = Option_array.sexp_of_t _of_a v_elts in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "elts"; arg ] :: bnds
      in
      let bnds =
        let arg = sexp_of_int v_length in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "length"; arg ] :: bnds
      in
      Ppx_sexp_conv_lib.Sexp.List bnds
;;

[@@@end]

let sexp_of_t_internal = sexp_of_t
let sexp_of_t = `Rebound_later
let _ = sexp_of_t
let capacity t = Option_array.length t.elts

let invariant invariant_a ({ length; elts } as t) : unit =
  try
    assert (0 <= length && length <= Option_array.length elts);
    for i = 0 to length - 1 do
      invariant_a (Option_array.get_some_exn elts i)
    done;
    (* We maintain the invariant that unused elements are unset to avoid a space
       leak. *)
    for i = length to Option_array.length elts - 1 do
      assert (not (Option_array.is_some elts i))
    done
  with
  | exn ->
    raise_s
      (Sexp.message
         "Stack.invariant failed"
         [ "exn", exn |> Exn.sexp_of_t; "stack", t |> sexp_of_t_internal sexp_of_opaque ])
;;

let create (type a) () : a t = { length = 0; elts = Option_array.empty }
let length t = t.length
let is_empty t = length t = 0

(* The order in which elements are visited has been chosen so as to be backwards
   compatible with both [Linked_stack] and [Caml.Stack] *)
let fold t ~init ~f =
  let r = ref init in
  for i = t.length - 1 downto 0 do
    r := f !r (Option_array.get_some_exn t.elts i)
  done;
  !r
;;

let iter t ~f =
  for i = t.length - 1 downto 0 do
    f (Option_array.get_some_exn t.elts i)
  done
;;

module C = Container.Make (struct
    type nonrec 'a t = 'a t

    let fold = fold
    let iter = `Custom iter
    let length = `Custom length
  end)

let mem = C.mem
let exists = C.exists
let for_all = C.for_all
let count = C.count
let sum = C.sum
let find = C.find
let find_map = C.find_map
let to_list = C.to_list
let to_array = C.to_array
let min_elt = C.min_elt
let max_elt = C.max_elt
let fold_result = C.fold_result
let fold_until = C.fold_until

let of_list (type a) (l : a list) =
  if List.is_empty l
  then create ()
  else (
    let length = List.length l in
    let elts = Option_array.create ~len:(2 * length) in
    let r = ref l in
    for i = length - 1 downto 0 do
      match !r with
      | [] -> assert false
      | a :: l ->
        Option_array.set_some elts i a;
        r := l
    done;
    { length; elts })
;;

let sexp_of_t sexp_of_a t = List.sexp_of_t sexp_of_a (to_list t)
let t_of_sexp a_of_sexp sexp = of_list (List.t_of_sexp a_of_sexp sexp)

let resize t size =
  let arr = Option_array.create ~len:size in
  Option_array.blit ~src:t.elts ~dst:arr ~src_pos:0 ~dst_pos:0 ~len:t.length;
  t.elts <- arr
;;

let set_capacity t new_capacity =
  let new_capacity = max new_capacity (length t) in
  if new_capacity <> capacity t then resize t new_capacity
;;

let push t a =
  if t.length = Option_array.length t.elts then resize t (2 * (t.length + 1));
  Option_array.set_some t.elts t.length a;
  t.length <- t.length + 1
;;

let pop_nonempty t =
  let i = t.length - 1 in
  let result = Option_array.get_some_exn t.elts i in
  Option_array.set_none t.elts i;
  t.length <- i;
  result
;;

let pop_error = Error.of_string "Stack.pop of empty stack"
let pop t = if is_empty t then None else Some (pop_nonempty t)
let pop_exn t = if is_empty t then Error.raise pop_error else pop_nonempty t
let top_nonempty t = Option_array.get_some_exn t.elts (t.length - 1)
let top_error = Error.of_string "Stack.top of empty stack"
let top t = if is_empty t then None else Some (top_nonempty t)
let top_exn t = if is_empty t then Error.raise top_error else top_nonempty t
let copy { length; elts } = { length; elts = Option_array.copy elts }

let clear t =
  if t.length > 0
  then (
    for i = 0 to t.length - 1 do
      Option_array.set_none t.elts i
    done;
    t.length <- 0)
;;

let until_empty t f =
  let rec loop () =
    if t.length > 0
    then (
      f (pop_nonempty t);
      loop ())
  in
  loop ()
;;

let singleton x =
  let t = create () in
  push t x;
  t
;;
