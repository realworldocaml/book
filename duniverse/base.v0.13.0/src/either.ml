open! Import
include Either_intf
module Array = Array0

type ('f, 's) t =
  | First of 'f
  | Second of 's
[@@deriving_inline compare, hash, sexp]
let compare :
  'f 's .
  ('f -> 'f -> int) -> ('s -> 's -> int) -> ('f, 's) t -> ('f, 's) t -> int
  =
  fun _cmp__f ->
  fun _cmp__s ->
  fun a__001_ ->
  fun b__002_ ->
  if Ppx_compare_lib.phys_equal a__001_ b__002_
  then 0
  else
    (match (a__001_, b__002_) with
     | (First _a__003_, First _b__004_) -> _cmp__f _a__003_ _b__004_
     | (First _, _) -> (-1)
     | (_, First _) -> 1
     | (Second _a__005_, Second _b__006_) ->
       _cmp__s _a__005_ _b__006_)
let hash_fold_t : type f s.
  (Ppx_hash_lib.Std.Hash.state -> f -> Ppx_hash_lib.Std.Hash.state) ->
  (Ppx_hash_lib.Std.Hash.state -> s -> Ppx_hash_lib.Std.Hash.state) ->
  Ppx_hash_lib.Std.Hash.state -> (f, s) t -> Ppx_hash_lib.Std.Hash.state
  =
  fun _hash_fold_f ->
  fun _hash_fold_s ->
  fun hsv ->
  fun arg ->
  match arg with
  | First _a0 ->
    let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 0 in
    let hsv = hsv in _hash_fold_f hsv _a0
  | Second _a0 ->
    let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 1 in
    let hsv = hsv in _hash_fold_s hsv _a0
let t_of_sexp : type f s.
  (Ppx_sexp_conv_lib.Sexp.t -> f) ->
  (Ppx_sexp_conv_lib.Sexp.t -> s) -> Ppx_sexp_conv_lib.Sexp.t -> (f, s) t
  =
  let _tp_loc = "src/either.ml.t" in
  fun _of_f ->
  fun _of_s ->
    function
    | Ppx_sexp_conv_lib.Sexp.List ((Ppx_sexp_conv_lib.Sexp.Atom
                                      ("first"|"First" as _tag))::sexp_args) as _sexp ->
      (match sexp_args with
       | v0::[] -> let v0 = _of_f v0 in First v0
       | _ ->
         Ppx_sexp_conv_lib.Conv_error.stag_incorrect_n_args _tp_loc
           _tag _sexp)
    | Ppx_sexp_conv_lib.Sexp.List ((Ppx_sexp_conv_lib.Sexp.Atom
                                      ("second"|"Second" as _tag))::sexp_args) as _sexp ->
      (match sexp_args with
       | v0::[] -> let v0 = _of_s v0 in Second v0
       | _ ->
         Ppx_sexp_conv_lib.Conv_error.stag_incorrect_n_args _tp_loc
           _tag _sexp)
    | Ppx_sexp_conv_lib.Sexp.Atom ("first"|"First") as sexp ->
      Ppx_sexp_conv_lib.Conv_error.stag_takes_args _tp_loc sexp
    | Ppx_sexp_conv_lib.Sexp.Atom ("second"|"Second") as sexp ->
      Ppx_sexp_conv_lib.Conv_error.stag_takes_args _tp_loc sexp
    | Ppx_sexp_conv_lib.Sexp.List ((Ppx_sexp_conv_lib.Sexp.List _)::_) as
      sexp ->
      Ppx_sexp_conv_lib.Conv_error.nested_list_invalid_sum _tp_loc sexp
    | Ppx_sexp_conv_lib.Sexp.List [] as sexp ->
      Ppx_sexp_conv_lib.Conv_error.empty_list_invalid_sum _tp_loc sexp
    | sexp -> Ppx_sexp_conv_lib.Conv_error.unexpected_stag _tp_loc sexp
let sexp_of_t : type f s.
  (f -> Ppx_sexp_conv_lib.Sexp.t) ->
  (s -> Ppx_sexp_conv_lib.Sexp.t) -> (f, s) t -> Ppx_sexp_conv_lib.Sexp.t
  =
  fun _of_f ->
  fun _of_s ->
  function
  | First v0 ->
    let v0 = _of_f v0 in
    Ppx_sexp_conv_lib.Sexp.List
      [Ppx_sexp_conv_lib.Sexp.Atom "First"; v0]
  | Second v0 ->
    let v0 = _of_s v0 in
    Ppx_sexp_conv_lib.Sexp.List
      [Ppx_sexp_conv_lib.Sexp.Atom "Second"; v0]
[@@@end]

let swap = function
  | First x -> Second x
  | Second x -> First x
;;

let is_first = function
  | First _ -> true
  | Second _ -> false
;;

let is_second = function
  | First _ -> false
  | Second _ -> true
;;

let value (First x | Second x) = x

let value_map t ~first ~second =
  match t with
  | First x -> first x
  | Second x -> second x
;;

let iter = value_map

let map t ~first ~second =
  match t with
  | First x -> First (first x)
  | Second x -> Second (second x)
;;

let first x = First x
let second x = Second x

let equal eq1 eq2 t1 t2 =
  match t1, t2 with
  | First x, First y -> eq1 x y
  | Second x, Second y -> eq2 x y
  | First _, Second _ | Second _, First _ -> false
;;

let invariant f s = function
  | First x -> f x
  | Second y -> s y
;;

module Make_focused (M : sig
    type (+'a, +'b) t

    val return : 'a -> ('a, _) t
    val other : 'b -> (_, 'b) t
    val either : ('a, 'b) t -> return:('a -> 'c) -> other:('b -> 'c) -> 'c

    val combine
      :  ('a, 'd) t
      -> ('b, 'd) t
      -> f:('a -> 'b -> 'c)
      -> other:('d -> 'd -> 'd)
      -> ('c, 'd) t

    val bind : ('a, 'b) t -> f:('a -> ('c, 'b) t) -> ('c, 'b) t
  end) =
struct
  include M
  open With_return

  let map t ~f = bind t ~f:(fun x -> return (f x))

  include Monad.Make2 (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let return = return
      let bind = bind
      let map = `Custom map
    end)

  module App = Applicative.Make2 (struct
      type nonrec ('a, 'b) t = ('a, 'b) t

      let return = return
      let apply t1 t2 = bind t1 ~f:(fun f -> bind t2 ~f:(fun x -> return (f x)))
      let map = `Custom map
    end)

  include App

  let combine_all =
    let rec other_loop f acc = function
      | [] -> other acc
      | t :: ts ->
        either
          t
          ~return:(fun _ -> other_loop f acc ts)
          ~other:(fun o -> other_loop f (f acc o) ts)
    in
    let rec return_loop f acc = function
      | [] -> return (List.rev acc)
      | t :: ts ->
        either
          t
          ~return:(fun x -> return_loop f (x :: acc) ts)
          ~other:(fun o -> other_loop f o ts)
    in
    fun ts ~f -> return_loop f [] ts
  ;;

  let combine_all_unit =
    let rec other_loop f acc = function
      | [] -> other acc
      | t :: ts ->
        either
          t
          ~return:(fun () -> other_loop f acc ts)
          ~other:(fun o -> other_loop f (f acc o) ts)
    in
    let rec return_loop f = function
      | [] -> return ()
      | t :: ts ->
        either t ~return:(fun () -> return_loop f ts) ~other:(fun o -> other_loop f o ts)
    in
    fun ts ~f -> return_loop f ts
  ;;

  let to_option t = either t ~return:Option.some ~other:(fun _ -> None)
  let value t ~default = either t ~return:Fn.id ~other:(fun _ -> default)

  let with_return f =
    with_return (fun ret -> other (f (With_return.prepend ret ~f:return)))
  ;;
end

module First = Make_focused (struct
    type nonrec ('a, 'b) t = ('a, 'b) t

    let return = first
    let other = second

    let either t ~return ~other =
      match t with
      | First x -> return x
      | Second y -> other y
    ;;

    let combine t1 t2 ~f ~other =
      match t1, t2 with
      | First x, First y -> First (f x y)
      | Second x, Second y -> Second (other x y)
      | Second x, _ | _, Second x -> Second x
    ;;

    let bind t ~f =
      match t with
      | First x -> f x
      (* Reuse the value in order to avoid allocation. *)
      | Second _ as y -> y
    ;;
  end)

module Second = Make_focused (struct
    type nonrec ('a, 'b) t = ('b, 'a) t

    let return = second
    let other = first

    let either t ~return ~other =
      match t with
      | Second y -> return y
      | First x -> other x
    ;;

    let combine t1 t2 ~f ~other =
      match t1, t2 with
      | Second x, Second y -> Second (f x y)
      | First x, First y -> First (other x y)
      | First x, _ | _, First x -> First x
    ;;

    let bind t ~f =
      match t with
      | Second x -> f x
      (* Reuse the value in order to avoid allocation, like [First.bind] above. *)
      | First _ as y -> y
    ;;
  end)

module Export = struct
  type ('f, 's) _either = ('f, 's) t =
    | First of 'f
    | Second of 's
end
