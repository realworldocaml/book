module type Basic = Monad_intf.Basic

module type S = Monad_intf.S

module type List = Monad_intf.List

module type Option = Monad_intf.Option

module type Result = Monad_intf.Result

module Make (M : Basic) = struct
  include M

  let map t ~f = bind t ~f:(fun x -> return (f x))

  module O = struct
    let ( >>= ) t f = bind t ~f

    let ( >>| ) t f = map t ~f

    let ( >>> ) a b = bind a ~f:(fun () -> b)

    let ( let+ ) t f = map t ~f

    let ( and+ ) x y =
      let open M in
      x >>= fun x ->
      y >>= fun y -> return (x, y)

    let ( let* ) t f = bind t ~f

    let ( and* ) = ( and+ )
  end
end
[@@inline always]

module Id = Make (struct
  type 'a t = 'a

  let return x = x

  let bind x ~f = f x
end)

module List (M : S) = struct
  open M
  open M.O

  let rec find_map xs ~f =
    match xs with
    | [] -> return None
    | x :: xs -> (
      let* x = f x in
      match x with
      | None -> find_map xs ~f
      | Some s -> return (Some s))

  let rec fold_left xs ~f ~init =
    match xs with
    | [] -> return init
    | x :: xs ->
      let* init = f init x in
      fold_left xs ~f ~init

  let filter_map xs ~f =
    let rec loop acc = function
      | [] -> return (List.rev acc)
      | x :: xs -> (
        let* y = f x in
        match y with
        | None -> loop acc xs
        | Some y -> loop (y :: acc) xs)
    in
    loop [] xs

  let filter xs ~f =
    filter_map xs ~f:(fun x ->
        let+ pred = f x in
        Option.some_if pred x)

  let map xs ~f =
    filter_map xs ~f:(fun x ->
        let+ x = f x in
        Some x)

  let concat_map xs ~f = map xs ~f >>| List.concat

  let rec iter xs ~f =
    match xs with
    | [] -> return ()
    | x :: xs ->
      let* () = f x in
      iter xs ~f

  let rec for_all xs ~f =
    match xs with
    | [] -> return true
    | x :: xs ->
      let* pred = f x in
      if pred then for_all xs ~f else return false

  let rec exists xs ~f =
    match xs with
    | [] -> return false
    | x :: xs ->
      let* pred = f x in
      if pred then return true else exists xs ~f
end

module Option (M : S) = struct
  let iter option ~f =
    match option with
    | None -> M.return ()
    | Some a -> f a

  let map option ~f =
    match option with
    | None -> M.return None
    | Some a -> M.map (f a) ~f:Option.some

  let bind option ~f =
    match option with
    | None -> M.return None
    | Some a -> f a
end

module Result (M : S) = struct
  let iter result ~f =
    match result with
    | Error _ -> M.return ()
    | Ok a -> f a
end
