open! Import
module Array = Array0
module List = List0
include Container_intf

let with_return = With_return.with_return

type ('t, 'a, 'accum) fold = 't -> init:'accum -> f:('accum -> 'a -> 'accum) -> 'accum
type ('t, 'a) iter = 't -> f:('a -> unit) -> unit
type 't length = 't -> int

let iter ~fold t ~f = fold t ~init:() ~f:(fun () a -> f a)
let count ~fold t ~f = fold t ~init:0 ~f:(fun n a -> if f a then n + 1 else n)

let sum (type a) ~fold (module M : Summable with type t = a) t ~f =
  fold t ~init:M.zero ~f:(fun n a -> M.( + ) n (f a))
;;

let fold_result ~fold ~init ~f t =
  with_return (fun { return } ->
    Result.Ok
      (fold t ~init ~f:(fun acc item ->
         match f acc item with
         | Result.Ok x -> x
         | Error _ as e -> return e)))
;;

let fold_until ~fold ~init ~f ~finish t =
  with_return (fun { return } ->
    finish
      (fold t ~init ~f:(fun acc item ->
         match f acc item with
         | Continue_or_stop.Continue x -> x
         | Stop x -> return x)))
;;

let min_elt ~fold t ~compare =
  fold t ~init:None ~f:(fun acc elt ->
    match acc with
    | None -> Some elt
    | Some min -> if compare min elt > 0 then Some elt else acc)
;;

let max_elt ~fold t ~compare =
  fold t ~init:None ~f:(fun acc elt ->
    match acc with
    | None -> Some elt
    | Some max -> if compare max elt < 0 then Some elt else acc)
;;

let length ~fold c = fold c ~init:0 ~f:(fun acc _ -> acc + 1)

let is_empty ~iter c =
  with_return (fun r ->
    iter c ~f:(fun _ -> r.return false);
    true)
;;

let exists ~iter c ~f =
  with_return (fun r ->
    iter c ~f:(fun x -> if f x then r.return true);
    false)
;;

let for_all ~iter c ~f =
  with_return (fun r ->
    iter c ~f:(fun x -> if not (f x) then r.return false);
    true)
;;

let find_map ~iter t ~f =
  with_return (fun r ->
    iter t ~f:(fun x ->
      match f x with
      | None -> ()
      | Some _ as res -> r.return res);
    None)
;;

let find ~iter c ~f =
  with_return (fun r ->
    iter c ~f:(fun x -> if f x then r.return (Some x));
    None)
;;

let to_list ~fold c = List.rev (fold c ~init:[] ~f:(fun acc x -> x :: acc))

let to_array ~length ~iter c =
  let array = ref [||] in
  let i = ref 0 in
  iter c ~f:(fun x ->
    if !i = 0 then array := Array.create ~len:(length c) x;
    !array.(!i) <- x;
    incr i);
  !array
;;

module Make_gen (T : Make_gen_arg) : sig
  include Generic with type 'a t := 'a T.t with type 'a elt := 'a T.elt
end = struct
  let fold = T.fold

  let iter =
    match T.iter with
    | `Custom iter -> iter
    | `Define_using_fold -> fun t ~f -> iter ~fold t ~f
  ;;

  let length =
    match T.length with
    | `Custom length -> length
    | `Define_using_fold -> fun t -> length ~fold t
  ;;

  let is_empty t = is_empty ~iter t
  let sum m t = sum ~fold m t
  let count t ~f = count ~fold t ~f
  let exists t ~f = exists ~iter t ~f
  let for_all t ~f = for_all ~iter t ~f
  let find_map t ~f = find_map ~iter t ~f
  let find t ~f = find ~iter t ~f
  let to_list t = to_list ~fold t
  let to_array t = to_array ~length ~iter t
  let min_elt t ~compare = min_elt ~fold t ~compare
  let max_elt t ~compare = max_elt ~fold t ~compare
  let fold_result t ~init ~f = fold_result t ~fold ~init ~f
  let fold_until t ~init ~f ~finish = fold_until t ~fold ~init ~f ~finish
end

module Make (T : Make_arg) = struct
  include Make_gen (struct
      include T

      type 'a elt = 'a
    end)

  let mem t a ~equal = exists t ~f:(equal a)
end

module Make0 (T : Make0_arg) = struct
  include Make_gen (struct
      include (T : Make0_arg with type t := T.t with module Elt := T.Elt)

      type 'a t = T.t
      type 'a elt = T.Elt.t
    end)

  let mem t elt = exists t ~f:(T.Elt.equal elt)
end

open T


(* The following functors exist as a consistency check among all the various [S?]
   interfaces.  They ensure that each particular [S?] is an instance of a more generic
   signature. *)
module Check
    (T : T1)
    (Elt : T1)
    (M : Generic with type 'a t := 'a T.t with type 'a elt := 'a Elt.t) =
struct end

module Check_S0 (M : S0) =
  Check
    (struct
      type 'a t = M.t
    end)
    (struct
      type 'a t = M.elt
    end)
    (M)

module Check_S0_phantom (M : S0_phantom) =
  Check
    (struct
      type 'a t = 'a M.t
    end)
    (struct
      type 'a t = M.elt
    end)
    (M)

module Check_S1 (M : S1) =
  Check
    (struct
      type 'a t = 'a M.t
    end)
    (struct
      type 'a t = 'a
    end)
    (M)

type phantom

module Check_S1_phantom (M : S1_phantom) =
  Check
    (struct
      type 'a t = ('a, phantom) M.t
    end)
    (struct
      type 'a t = 'a
    end)
    (M)

module Check_S1_phantom_invariant (M : S1_phantom_invariant) =
  Check
    (struct
      type 'a t = ('a, phantom) M.t
    end)
    (struct
      type 'a t = 'a
    end)
    (M)
