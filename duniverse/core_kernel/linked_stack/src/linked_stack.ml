open! Core_kernel

type 'a t =
  { mutable elts : 'a list
  ; mutable length : int
  }
[@@deriving bin_io, fields, sexp_of]

let sexp_of_t_internal = sexp_of_t
let sexp_of_t = `Rebound_later
let _ = sexp_of_t

let invariant a_invariant t : unit =
  try
    let check f field = f (Field.get field t) in
    Fields.iter
      ~elts:(check (fun elts -> List.iter elts ~f:a_invariant))
      ~length:(check (fun length -> assert (length = List.length t.elts)))
  with
  | exn ->
    failwiths
      ~here:[%here]
      "Linked_stack.invariant failed"
      (exn, t)
      [%sexp_of: exn * _ t_internal]
;;

let create () = { elts = []; length = 0 }
let singleton a = { elts = [ a ]; length = 1 }

(* We always want to set elts and length at the same time.  Having a function to do so
   helps us to remember. *)
let set t elts length =
  t.elts <- elts;
  t.length <- length
;;

let push t x = set t (x :: t.elts) (t.length + 1)
let pop_error = Error.of_string "Stack.pop of empty stack"

let pop_exn t =
  match t.elts with
  | [] -> Error.raise pop_error
  | x :: l ->
    set t l (t.length - 1);
    x
;;

let pop t =
  match t.elts with
  | [] -> None
  | x :: l ->
    set t l (t.length - 1);
    Some x
;;

let top_error = Error.of_string "Stack.top of empty stack"

let top_exn t =
  match t.elts with
  | [] -> Error.raise top_error
  | x :: _ -> x
;;

let top t =
  match t.elts with
  | [] -> None
  | x :: _ -> Some x
;;

let clear t = set t [] 0
let copy t = { elts = t.elts; length = t.length }
let length t = t.length
let is_empty t = t.length = 0
let iter t ~f = List.iter t.elts ~f
let fold t ~init ~f = List.fold t.elts ~init ~f
let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t
let fold_until t ~init ~f = Container.fold_until ~fold ~init ~f t
let count t ~f = List.count t.elts ~f
let sum m t ~f = List.sum m t.elts ~f
let min_elt t ~compare = List.min_elt t.elts ~compare
let max_elt t ~compare = List.max_elt t.elts ~compare
let exists t ~f = List.exists t.elts ~f
let mem t a ~equal = List.mem t.elts a ~equal
let for_all t ~f = List.for_all t.elts ~f
let find t ~f = List.find t.elts ~f
let find_map t ~f = List.find_map t.elts ~f
let to_list t = t.elts
let of_list l = { elts = l; length = List.length l }
let to_array t = Array.of_list t.elts
let sexp_of_t sexp_of_a t = [%sexp_of: a list] (to_list t)

let t_of_sexp a_of_sexp sexp =
  let elts = [%of_sexp: a list] sexp in
  { elts; length = List.length elts }
;;

let until_empty t f =
  let rec loop () =
    if t.length > 0
    then (
      f (pop_exn t);
      loop ())
  in
  loop ()
;;
