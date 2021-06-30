open! Import
include Either_intf
module Array = Array0
module List = List0
include Either0

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
