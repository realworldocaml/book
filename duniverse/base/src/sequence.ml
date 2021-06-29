open! Import
open Container_intf.Export
module Array = Array0
module List = List1

module Step = struct
  (* 'a is an item in the sequence, 's is the state that will produce the remainder of
     the sequence *)
  type ('a, 's) t =
    | Done
    | Skip of 's
    | Yield of 'a * 's
  [@@deriving_inline sexp_of]

  let sexp_of_t
    : type a s.
      (a -> Ppx_sexp_conv_lib.Sexp.t)
      -> (s -> Ppx_sexp_conv_lib.Sexp.t)
      -> (a, s) t
      -> Ppx_sexp_conv_lib.Sexp.t
    =
    fun _of_a _of_s -> function
      | Done -> Ppx_sexp_conv_lib.Sexp.Atom "Done"
      | Skip v0 ->
        let v0 = _of_s v0 in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Skip"; v0 ]
      | Yield (v0, v1) ->
        let v0 = _of_a v0
        and v1 = _of_s v1 in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Yield"; v0; v1 ]
  ;;

  [@@@end]
end

open Step

(* 'a is an item in the sequence, 's is the state that will produce the remainder of the
   sequence *)
type +_ t = Sequence : 's * ('s -> ('a, 's) Step.t) -> 'a t
type 'a sequence = 'a t

module Expert = struct
  let next_step (Sequence (s, f)) =
    match f s with
    | Done -> Done
    | Skip s -> Skip (Sequence (s, f))
    | Yield (a, s) -> Yield (a, Sequence (s, f))
  ;;

  let delayed_fold_step s ~init ~f ~finish =
    let rec loop s next finish f acc =
      match next s with
      | Done -> finish acc
      | Skip s -> f acc None ~k:(loop s next finish f)
      | Yield (a, s) -> f acc (Some a) ~k:(loop s next finish f)
    in
    match s with
    | Sequence (s, next) -> loop s next finish f init
  ;;
end

let unfold_step ~init ~f = Sequence (init, f)

let unfold ~init ~f =
  unfold_step ~init ~f:(fun s ->
    match f s with
    | None -> Step.Done
    | Some (a, s) -> Step.Yield (a, s))
;;

let unfold_with s ~init ~f =
  match s with
  | Sequence (s, next) ->
    Sequence
      ( (init, s)
      , fun (seed, s) ->
        match next s with
        | Done -> Done
        | Skip s -> Skip (seed, s)
        | Yield (a, s) ->
          (match f seed a with
           | Done -> Done
           | Skip seed -> Skip (seed, s)
           | Yield (a, seed) -> Yield (a, (seed, s))) )
;;

let unfold_with_and_finish s ~init ~running_step ~inner_finished ~finishing_step =
  match s with
  | Sequence (s, next) ->
    Sequence
      ( `Inner_running (init, s)
      , fun state ->
        match state with
        | `Inner_running (state, inner_state) ->
          (match next inner_state with
           | Done -> Skip (`Inner_finished (inner_finished state))
           | Skip inner_state -> Skip (`Inner_running (state, inner_state))
           | Yield (x, inner_state) ->
             (match running_step state x with
              | Done -> Done
              | Skip state -> Skip (`Inner_running (state, inner_state))
              | Yield (y, state) -> Yield (y, `Inner_running (state, inner_state))))
        | `Inner_finished state ->
          (match finishing_step state with
           | Done -> Done
           | Skip state -> Skip (`Inner_finished state)
           | Yield (y, state) -> Yield (y, `Inner_finished state)) )
;;

let of_list l =
  unfold_step ~init:l ~f:(function
    | [] -> Done
    | x :: l -> Yield (x, l))
;;


let fold t ~init ~f =
  let rec loop seed v next f =
    match next seed with
    | Done -> v
    | Skip s -> loop s v next f
    | Yield (a, s) -> loop s (f v a) next f
  in
  match t with
  | Sequence (seed, next) -> loop seed init next f
;;

let to_list_rev t = fold t ~init:[] ~f:(fun l x -> x :: l)


let to_list (Sequence (s, next)) =
  let safe_to_list t = List.rev (to_list_rev t) in
  let rec to_list s next i =
    if i = 0
    then safe_to_list (Sequence (s, next))
    else (
      match next s with
      | Done -> []
      | Skip s -> to_list s next i
      | Yield (a, s) -> a :: to_list s next (i - 1))
  in
  to_list s next 500
;;

let sexp_of_t sexp_of_a t = sexp_of_list sexp_of_a (to_list t)

let range ?(stride = 1) ?(start = `inclusive) ?(stop = `exclusive) start_v stop_v =
  let step =
    match stop with
    | `inclusive when stride >= 0 ->
      fun i -> if i > stop_v then Done else Yield (i, i + stride)
    | `inclusive -> fun i -> if i < stop_v then Done else Yield (i, i + stride)
    | `exclusive when stride >= 0 ->
      fun i -> if i >= stop_v then Done else Yield (i, i + stride)
    | `exclusive -> fun i -> if i <= stop_v then Done else Yield (i, i + stride)
  in
  let init =
    match start with
    | `inclusive -> start_v
    | `exclusive -> start_v + stride
  in
  unfold_step ~init ~f:step
;;

let of_lazy t_lazy =
  unfold_step ~init:t_lazy ~f:(fun t_lazy ->
    let (Sequence (s, next)) = Lazy.force t_lazy in
    match next s with
    | Done -> Done
    | Skip s ->
      Skip
        (let v = Sequence (s, next) in
         lazy v)
    | Yield (x, s) ->
      Yield
        ( x
        , let v = Sequence (s, next) in
          lazy v ))
;;

let map t ~f =
  match t with
  | Sequence (seed, next) ->
    Sequence
      ( seed
      , fun seed ->
        match next seed with
        | Done -> Done
        | Skip s -> Skip s
        | Yield (a, s) -> Yield (f a, s) )
;;

let mapi t ~f =
  match t with
  | Sequence (s, next) ->
    Sequence
      ( (0, s)
      , fun (i, s) ->
        match next s with
        | Done -> Done
        | Skip s -> Skip (i, s)
        | Yield (a, s) -> Yield (f i a, (i + 1, s)) )
;;

let folding_map t ~init ~f =
  unfold_with t ~init ~f:(fun acc x ->
    let acc, x = f acc x in
    Yield (x, acc))
;;

let folding_mapi t ~init ~f =
  unfold_with t ~init:(0, init) ~f:(fun (i, acc) x ->
    let acc, x = f i acc x in
    Yield (x, (i + 1, acc)))
;;

let filter t ~f =
  match t with
  | Sequence (seed, next) ->
    Sequence
      ( seed
      , fun seed ->
        match next seed with
        | Done -> Done
        | Skip s -> Skip s
        | Yield (a, s) when f a -> Yield (a, s)
        | Yield (_, s) -> Skip s )
;;

let filteri t ~f =
  map ~f:snd (filter (mapi t ~f:(fun i s -> i, s)) ~f:(fun (i, s) -> f i s))
;;

let length t =
  let rec loop i s next =
    match next s with
    | Done -> i
    | Skip s -> loop i s next
    | Yield (_, s) -> loop (i + 1) s next
  in
  match t with
  | Sequence (seed, next) -> loop 0 seed next
;;

let to_list_rev_with_length t = fold t ~init:([], 0) ~f:(fun (l, i) x -> x :: l, i + 1)

let to_array t =
  let l, len = to_list_rev_with_length t in
  match l with
  | [] -> [||]
  | x :: l ->
    let a = Array.create ~len x in
    let rec loop i l =
      match l with
      | [] -> assert (i = -1)
      | x :: l ->
        a.(i) <- x;
        loop (i - 1) l
    in
    loop (len - 2) l;
    a
;;

let find t ~f =
  let rec loop s next f =
    match next s with
    | Done -> None
    | Yield (a, _) when f a -> Some a
    | Yield (_, s) | Skip s -> loop s next f
  in
  match t with
  | Sequence (seed, next) -> loop seed next f
;;

let find_map t ~f =
  let rec loop s next f =
    match next s with
    | Done -> None
    | Yield (a, s) ->
      (match f a with
       | None -> loop s next f
       | some_b -> some_b)
    | Skip s -> loop s next f
  in
  match t with
  | Sequence (seed, next) -> loop seed next f
;;


let find_mapi t ~f =
  let rec loop s next f i =
    match next s with
    | Done -> None
    | Yield (a, s) ->
      (match f i a with
       | None -> loop s next f (i + 1)
       | some_b -> some_b)
    | Skip s -> loop s next f i
  in
  match t with
  | Sequence (seed, next) -> loop seed next f 0
;;

let for_all t ~f =
  let rec loop s next f =
    match next s with
    | Done -> true
    | Yield (a, _) when not (f a) -> false
    | Yield (_, s) | Skip s -> loop s next f
  in
  match t with
  | Sequence (seed, next) -> loop seed next f
;;

let for_alli t ~f =
  let rec loop s next f i =
    match next s with
    | Done -> true
    | Yield (a, _) when not (f i a) -> false
    | Yield (_, s) -> loop s next f (i + 1)
    | Skip s -> loop s next f i
  in
  match t with
  | Sequence (seed, next) -> loop seed next f 0
;;

let exists t ~f =
  let rec loop s next f =
    match next s with
    | Done -> false
    | Yield (a, _) when f a -> true
    | Yield (_, s) | Skip s -> loop s next f
  in
  match t with
  | Sequence (seed, next) -> loop seed next f
;;

let existsi t ~f =
  let rec loop s next f i =
    match next s with
    | Done -> false
    | Yield (a, _) when f i a -> true
    | Yield (_, s) -> loop s next f (i + 1)
    | Skip s -> loop s next f i
  in
  match t with
  | Sequence (seed, next) -> loop seed next f 0
;;

let iter t ~f =
  let rec loop seed next f =
    match next seed with
    | Done -> ()
    | Skip s -> loop s next f
    | Yield (a, s) ->
      f a;
      loop s next f
  in
  match t with
  | Sequence (seed, next) -> loop seed next f
;;

let is_empty t =
  let rec loop s next =
    match next s with
    | Done -> true
    | Skip s -> loop s next
    | Yield _ -> false
  in
  match t with
  | Sequence (seed, next) -> loop seed next
;;

let mem t a ~equal =
  let rec loop s next a =
    match next s with
    | Done -> false
    | Yield (b, _) when equal a b -> true
    | Yield (_, s) | Skip s -> loop s next a
  in
  match t with
  | Sequence (seed, next) -> loop seed next a
;;

let empty = Sequence ((), fun () -> Done)

let bind t ~f =
  unfold_step
    ~f:(function
      | Sequence (seed, next), rest ->
        (match next seed with
         | Done ->
           (match rest with
            | Sequence (seed, next) ->
              (match next seed with
               | Done -> Done
               | Skip s -> Skip (empty, Sequence (s, next))
               | Yield (a, s) -> Skip (f a, Sequence (s, next))))
         | Skip s -> Skip (Sequence (s, next), rest)
         | Yield (a, s) -> Yield (a, (Sequence (s, next), rest))))
    ~init:(empty, t)
;;

let return x =
  unfold_step ~init:(Some x) ~f:(function
    | None -> Done
    | Some x -> Yield (x, None))
;;

include Monad.Make (struct
    type nonrec 'a t = 'a t

    let map = `Custom map
    let bind = bind
    let return = return
  end)

let nth s n =
  if n < 0
  then None
  else (
    let rec loop i s next =
      match next s with
      | Done -> None
      | Skip s -> loop i s next
      | Yield (a, s) -> if phys_equal i 0 then Some a else loop (i - 1) s next
    in
    match s with
    | Sequence (s, next) -> loop n s next)
;;

let nth_exn s n =
  if n < 0
  then invalid_arg "Sequence.nth"
  else (
    match nth s n with
    | None -> failwith "Sequence.nth"
    | Some x -> x)
;;

module Merge_with_duplicates_element = struct
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b
    | Both of 'a * 'b
  [@@deriving_inline compare, hash, sexp]

  let compare :
    'a 'b. ('a -> 'a -> int) -> ('b -> 'b -> int) -> ('a, 'b) t -> ('a, 'b) t -> int
    =
    fun _cmp__a _cmp__b a__001_ b__002_ ->
    if Ppx_compare_lib.phys_equal a__001_ b__002_
    then 0
    else (
      match a__001_, b__002_ with
      | Left _a__003_, Left _b__004_ -> _cmp__a _a__003_ _b__004_
      | Left _, _ -> -1
      | _, Left _ -> 1
      | Right _a__005_, Right _b__006_ -> _cmp__b _a__005_ _b__006_
      | Right _, _ -> -1
      | _, Right _ -> 1
      | Both (_a__007_, _a__009_), Both (_b__008_, _b__010_) ->
        (match _cmp__a _a__007_ _b__008_ with
         | 0 -> _cmp__b _a__009_ _b__010_
         | n -> n))
  ;;

  let hash_fold_t
    : type a b.
      (Ppx_hash_lib.Std.Hash.state -> a -> Ppx_hash_lib.Std.Hash.state)
      -> (Ppx_hash_lib.Std.Hash.state -> b -> Ppx_hash_lib.Std.Hash.state)
      -> Ppx_hash_lib.Std.Hash.state
      -> (a, b) t
      -> Ppx_hash_lib.Std.Hash.state
    =
    fun _hash_fold_a _hash_fold_b hsv arg ->
      match arg with
      | Left _a0 ->
        let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 0 in
        let hsv = hsv in
        _hash_fold_a hsv _a0
      | Right _a0 ->
        let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 1 in
        let hsv = hsv in
        _hash_fold_b hsv _a0
      | Both (_a0, _a1) ->
        let hsv = Ppx_hash_lib.Std.Hash.fold_int hsv 2 in
        let hsv =
          let hsv = hsv in
          _hash_fold_a hsv _a0
        in
        _hash_fold_b hsv _a1
  ;;

  let t_of_sexp
    : type a b.
      (Ppx_sexp_conv_lib.Sexp.t -> a)
      -> (Ppx_sexp_conv_lib.Sexp.t -> b)
      -> Ppx_sexp_conv_lib.Sexp.t
      -> (a, b) t
    =
    let _tp_loc = "sequence.ml.Merge_with_duplicates_element.t" in
    fun _of_a _of_b -> function
      | Ppx_sexp_conv_lib.Sexp.List
          (Ppx_sexp_conv_lib.Sexp.Atom (("left" | "Left") as _tag) :: sexp_args) as _sexp
        ->
        (match sexp_args with
         | [ v0 ] ->
           let v0 = _of_a v0 in
           Left v0
         | _ -> Ppx_sexp_conv_lib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
      | Ppx_sexp_conv_lib.Sexp.List
          (Ppx_sexp_conv_lib.Sexp.Atom (("right" | "Right") as _tag) :: sexp_args) as
        _sexp ->
        (match sexp_args with
         | [ v0 ] ->
           let v0 = _of_b v0 in
           Right v0
         | _ -> Ppx_sexp_conv_lib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
      | Ppx_sexp_conv_lib.Sexp.List
          (Ppx_sexp_conv_lib.Sexp.Atom (("both" | "Both") as _tag) :: sexp_args) as _sexp
        ->
        (match sexp_args with
         | [ v0; v1 ] ->
           let v0 = _of_a v0
           and v1 = _of_b v1 in
           Both (v0, v1)
         | _ -> Ppx_sexp_conv_lib.Conv_error.stag_incorrect_n_args _tp_loc _tag _sexp)
      | Ppx_sexp_conv_lib.Sexp.Atom ("left" | "Left") as sexp ->
        Ppx_sexp_conv_lib.Conv_error.stag_takes_args _tp_loc sexp
      | Ppx_sexp_conv_lib.Sexp.Atom ("right" | "Right") as sexp ->
        Ppx_sexp_conv_lib.Conv_error.stag_takes_args _tp_loc sexp
      | Ppx_sexp_conv_lib.Sexp.Atom ("both" | "Both") as sexp ->
        Ppx_sexp_conv_lib.Conv_error.stag_takes_args _tp_loc sexp
      | Ppx_sexp_conv_lib.Sexp.List (Ppx_sexp_conv_lib.Sexp.List _ :: _) as sexp ->
        Ppx_sexp_conv_lib.Conv_error.nested_list_invalid_sum _tp_loc sexp
      | Ppx_sexp_conv_lib.Sexp.List [] as sexp ->
        Ppx_sexp_conv_lib.Conv_error.empty_list_invalid_sum _tp_loc sexp
      | sexp -> Ppx_sexp_conv_lib.Conv_error.unexpected_stag _tp_loc sexp
  ;;

  let sexp_of_t
    : type a b.
      (a -> Ppx_sexp_conv_lib.Sexp.t)
      -> (b -> Ppx_sexp_conv_lib.Sexp.t)
      -> (a, b) t
      -> Ppx_sexp_conv_lib.Sexp.t
    =
    fun _of_a _of_b -> function
      | Left v0 ->
        let v0 = _of_a v0 in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Left"; v0 ]
      | Right v0 ->
        let v0 = _of_b v0 in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Right"; v0 ]
      | Both (v0, v1) ->
        let v0 = _of_a v0
        and v1 = _of_b v1 in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Both"; v0; v1 ]
  ;;

  [@@@end]
end

let merge_with_duplicates (Sequence (s1, next1)) (Sequence (s2, next2)) ~compare =
  let unshadowed_compare = compare in
  let open Merge_with_duplicates_element in
  let next = function
    | Skip s1, s2 -> Skip (next1 s1, s2)
    | s1, Skip s2 -> Skip (s1, next2 s2)
    | (Yield (a, s1') as s1), (Yield (b, s2') as s2) ->
      let comparison = unshadowed_compare a b in
      if comparison < 0
      then Yield (Left a, (Skip s1', s2))
      else if comparison = 0
      then Yield (Both (a, b), (Skip s1', Skip s2'))
      else Yield (Right b, (s1, Skip s2'))
    | Done, Done -> Done
    | Yield (a, s1), Done -> Yield (Left a, (Skip s1, Done))
    | Done, Yield (b, s2) -> Yield (Right b, (Done, Skip s2))
  in
  Sequence ((Skip s1, Skip s2), next)
;;

let merge s1 s2 ~compare =
  map (merge_with_duplicates s1 s2 ~compare) ~f:(function
    | Left x | Right x | Both (x, _) -> x)
;;

let hd s =
  let rec loop s next =
    match next s with
    | Done -> None
    | Skip s -> loop s next
    | Yield (a, _) -> Some a
  in
  match s with
  | Sequence (s, next) -> loop s next
;;

let hd_exn s =
  match hd s with
  | None -> failwith "hd_exn"
  | Some a -> a
;;

let tl s =
  let rec loop s next =
    match next s with
    | Done -> None
    | Skip s -> loop s next
    | Yield (_, a) -> Some a
  in
  match s with
  | Sequence (s, next) ->
    (match loop s next with
     | None -> None
     | Some s -> Some (Sequence (s, next)))
;;

let tl_eagerly_exn s =
  match tl s with
  | None -> failwith "Sequence.tl_exn"
  | Some s -> s
;;

let lift_identity next s =
  match next s with
  | Done -> Done
  | Skip s -> Skip (`Identity s)
  | Yield (a, s) -> Yield (a, `Identity s)
;;

let next s =
  let rec loop s next =
    match next s with
    | Done -> None
    | Skip s -> loop s next
    | Yield (a, s) -> Some (a, Sequence (s, next))
  in
  match s with
  | Sequence (s, next) -> loop s next
;;

let filter_opt s =
  match s with
  | Sequence (s, next) ->
    Sequence
      ( s
      , fun s ->
        match next s with
        | Done -> Done
        | Skip s -> Skip s
        | Yield (None, s) -> Skip s
        | Yield (Some a, s) -> Yield (a, s) )
;;

let filter_map s ~f = filter_opt (map s ~f)
let filter_mapi s ~f = filter_map (mapi s ~f:(fun i s -> i, s)) ~f:(fun (i, s) -> f i s)

let split_n s n =
  let rec loop s i accum next =
    if i <= 0
    then List.rev accum, Sequence (s, next)
    else (
      match next s with
      | Done -> List.rev accum, empty
      | Skip s -> loop s i accum next
      | Yield (a, s) -> loop s (i - 1) (a :: accum) next)
  in
  match s with
  | Sequence (s, next) -> loop s n [] next
;;

let chunks_exn t n =
  if n <= 0
  then invalid_arg "Sequence.chunks_exn"
  else
    unfold_step ~init:t ~f:(fun t ->
      match split_n t n with
      | [], _empty -> Done
      | (_ :: _ as xs), t -> Yield (xs, t))
;;

let findi s ~f = find (mapi s ~f:(fun i s -> i, s)) ~f:(fun (i, s) -> f i s)

let find_exn s ~f =
  match find s ~f with
  | None -> failwith "Sequence.find_exn"
  | Some x -> x
;;

let append s1 s2 =
  match s1, s2 with
  | Sequence (s1, next1), Sequence (s2, next2) ->
    Sequence
      ( `First_list s1
      , function
        | `First_list s1 ->
          (match next1 s1 with
           | Done -> Skip (`Second_list s2)
           | Skip s1 -> Skip (`First_list s1)
           | Yield (a, s1) -> Yield (a, `First_list s1))
        | `Second_list s2 ->
          (match next2 s2 with
           | Done -> Done
           | Skip s2 -> Skip (`Second_list s2)
           | Yield (a, s2) -> Yield (a, `Second_list s2)) )
;;

let concat_map s ~f = bind s ~f
let concat s = concat_map s ~f:Fn.id
let concat_mapi s ~f = concat_map (mapi s ~f:(fun i s -> i, s)) ~f:(fun (i, s) -> f i s)

let zip (Sequence (s1, next1)) (Sequence (s2, next2)) =
  let next = function
    | Yield (a, s1), Yield (b, s2) -> Yield ((a, b), (Skip s1, Skip s2))
    | Done, _ | _, Done -> Done
    | Skip s1, s2 -> Skip (next1 s1, s2)
    | s1, Skip s2 -> Skip (s1, next2 s2)
  in
  Sequence ((Skip s1, Skip s2), next)
;;

let zip_full (Sequence (s1, next1)) (Sequence (s2, next2)) =
  let next = function
    | Yield (a, s1), Yield (b, s2) -> Yield (`Both (a, b), (Skip s1, Skip s2))
    | Done, Done -> Done
    | Skip s1, s2 -> Skip (next1 s1, s2)
    | s1, Skip s2 -> Skip (s1, next2 s2)
    | Done, Yield (b, s2) -> Yield (`Right b, (Done, next2 s2))
    | Yield (a, s1), Done -> Yield (`Left a, (next1 s1, Done))
  in
  Sequence ((Skip s1, Skip s2), next)
;;

let bounded_length (Sequence (seed, next)) ~at_most =
  let rec loop i seed next =
    if i > at_most
    then `Greater
    else (
      match next seed with
      | Done -> `Is i
      | Skip seed -> loop i seed next
      | Yield (_, seed) -> loop (i + 1) seed next)
  in
  loop 0 seed next
;;

let length_is_bounded_by ?(min = -1) ?max t =
  let length_is_at_least (Sequence (s, next)) =
    let rec loop s acc =
      if acc >= min
      then true
      else (
        match next s with
        | Done -> false
        | Skip s -> loop s acc
        | Yield (_, s) -> loop s (acc + 1))
    in
    loop s 0
  in
  match max with
  | None -> length_is_at_least t
  | Some max ->
    (match bounded_length t ~at_most:max with
     | `Is len when len >= min -> true
     | _ -> false)
;;

let iteri s ~f = iter (mapi s ~f:(fun i s -> i, s)) ~f:(fun (i, s) -> f i s)

let foldi s ~init ~f =
  fold ~init (mapi s ~f:(fun i s -> i, s)) ~f:(fun acc (i, s) -> f i acc s)
;;

let reduce s ~f =
  match next s with
  | None -> None
  | Some (a, s) -> Some (fold s ~init:a ~f)
;;

let reduce_exn s ~f =
  match reduce s ~f with
  | None -> failwith "Sequence.reduce_exn"
  | Some res -> res
;;

let group (Sequence (s, next)) ~break =
  unfold_step
    ~init:(Some ([], s))
    ~f:(function
      | None -> Done
      | Some (acc, s) ->
        (match acc, next s with
         | _, Skip s -> Skip (Some (acc, s))
         | [], Done -> Done
         | acc, Done -> Yield (List.rev acc, None)
         | [], Yield (cur, s) -> Skip (Some ([ cur ], s))
         | (prev :: _ as acc), Yield (cur, s) ->
           if break prev cur
           then Yield (List.rev acc, Some ([ cur ], s))
           else Skip (Some (cur :: acc, s))))
;;

let find_consecutive_duplicate (Sequence (s, next)) ~equal =
  let rec loop last_elt s =
    match next s with
    | Done -> None
    | Skip s -> loop last_elt s
    | Yield (a, s) ->
      (match last_elt with
       | Some b when equal a b -> Some (b, a)
       | None | Some _ -> loop (Some a) s)
  in
  loop None s
;;

let remove_consecutive_duplicates s ~equal =
  unfold_with s ~init:None ~f:(fun prev a ->
    match prev with
    | Some b when equal a b -> Skip (Some a)
    | None | Some _ -> Yield (a, Some a))
;;

let count s ~f = length (filter s ~f)
let counti t ~f = length (filteri t ~f)
let sum m t ~f = Container.sum ~fold m t ~f
let min_elt t ~compare = Container.min_elt ~fold t ~compare
let max_elt t ~compare = Container.max_elt ~fold t ~compare

let init n ~f =
  unfold_step ~init:0 ~f:(fun i -> if i >= n then Done else Yield (f i, i + 1))
;;

let sub s ~pos ~len =
  if pos < 0 || len < 0 then failwith "Sequence.sub";
  match s with
  | Sequence (s, next) ->
    Sequence
      ( (0, s)
      , fun (i, s) ->
        if i - pos >= len
        then Done
        else (
          match next s with
          | Done -> Done
          | Skip s -> Skip (i, s)
          | Yield (a, s) when i >= pos -> Yield (a, (i + 1, s))
          | Yield (_, s) -> Skip (i + 1, s)) )
;;

let take s len =
  if len < 0 then failwith "Sequence.take";
  match s with
  | Sequence (s, next) ->
    Sequence
      ( (0, s)
      , fun (i, s) ->
        if i >= len
        then Done
        else (
          match next s with
          | Done -> Done
          | Skip s -> Skip (i, s)
          | Yield (a, s) -> Yield (a, (i + 1, s))) )
;;

let drop s len =
  if len < 0 then failwith "Sequence.drop";
  match s with
  | Sequence (s, next) ->
    Sequence
      ( (0, s)
      , fun (i, s) ->
        match next s with
        | Done -> Done
        | Skip s -> Skip (i, s)
        | Yield (a, s) when i >= len -> Yield (a, (i + 1, s))
        | Yield (_, s) -> Skip (i + 1, s) )
;;

let take_while s ~f =
  match s with
  | Sequence (s, next) ->
    Sequence
      ( s
      , fun s ->
        match next s with
        | Done -> Done
        | Skip s -> Skip s
        | Yield (a, s) when f a -> Yield (a, s)
        | Yield (_, _) -> Done )
;;

let drop_while s ~f =
  match s with
  | Sequence (s, next) ->
    Sequence
      ( `Dropping s
      , function
        | `Dropping s ->
          (match next s with
           | Done -> Done
           | Skip s -> Skip (`Dropping s)
           | Yield (a, s) when f a -> Skip (`Dropping s)
           | Yield (a, s) -> Yield (a, `Identity s))
        | `Identity s -> lift_identity next s )
;;

let shift_right s x =
  match s with
  | Sequence (seed, next) ->
    Sequence
      ( `Consing (seed, x)
      , function
        | `Consing (seed, x) -> Yield (x, `Identity seed)
        | `Identity s -> lift_identity next s )
;;

let shift_right_with_list s l = append (of_list l) s
let shift_left = drop

module Infix = struct
  let ( @ ) = append
end

let intersperse s ~sep =
  match s with
  | Sequence (s, next) ->
    Sequence
      ( `Init s
      , function
        | `Init s ->
          (match next s with
           | Done -> Done
           | Skip s -> Skip (`Init s)
           | Yield (a, s) -> Yield (a, `Running s))
        | `Running s ->
          (match next s with
           | Done -> Done
           | Skip s -> Skip (`Running s)
           | Yield (a, s) -> Yield (sep, `Putting (a, s)))
        | `Putting (a, s) -> Yield (a, `Running s) )
;;

let repeat x = unfold_step ~init:x ~f:(fun x -> Yield (x, x))

let cycle_list_exn xs =
  if List.is_empty xs then invalid_arg "Sequence.cycle_list_exn";
  let s = of_list xs in
  concat_map ~f:(fun () -> s) (repeat ())
;;

let cartesian_product sa sb = concat_map sa ~f:(fun a -> zip (repeat a) sb)
let singleton x = return x

let delayed_fold s ~init ~f ~finish =
  Expert.delayed_fold_step s ~init ~finish ~f:(fun acc option ~k ->
    match option with
    | None -> k acc
    | Some a -> f acc a ~k)
;;

let fold_m ~bind ~return t ~init ~f =
  Expert.delayed_fold_step
    t
    ~init
    ~f:(fun acc option ~k ->
      match option with
      | None -> bind (return acc) ~f:k
      | Some a -> bind (f acc a) ~f:k)
    ~finish:return
;;

let iter_m ~bind ~return t ~f =
  Expert.delayed_fold_step
    t
    ~init:()
    ~f:(fun () option ~k ->
      match option with
      | None -> bind (return ()) ~f:k
      | Some a -> bind (f a) ~f:k)
    ~finish:return
;;

let fold_until s ~init ~f ~finish =
  let rec loop s next f acc =
    match next s with
    | Done -> finish acc
    | Skip s -> loop s next f acc
    | Yield (a, s) ->
      (match (f acc a : ('a, 'b) Continue_or_stop.t) with
       | Stop x -> x
       | Continue acc -> loop s next f acc)
  in
  match s with
  | Sequence (s, next) -> loop s next f init
;;

let fold_result s ~init ~f =
  let rec loop s next f acc =
    match next s with
    | Done -> Result.return acc
    | Skip s -> loop s next f acc
    | Yield (a, s) ->
      (match (f acc a : (_, _) Result.t) with
       | Error _ as e -> e
       | Ok acc -> loop s next f acc)
  in
  match s with
  | Sequence (s, next) -> loop s next f init
;;

let force_eagerly t = of_list (to_list t)

let memoize (type a) (Sequence (s, next)) =
  let module M = struct
    type t = T of (a, t) Step.t Lazy.t
  end
  in
  let rec memoize s = M.T (lazy (find_step s))
  and find_step s =
    match next s with
    | Done -> Done
    | Skip s -> find_step s
    | Yield (a, s) -> Yield (a, memoize s)
  in
  Sequence (memoize s, fun (M.T l) -> Lazy.force l)
;;

let drop_eagerly s len =
  let rec loop i ~len s next =
    if i >= len
    then Sequence (s, next)
    else (
      match next s with
      | Done -> empty
      | Skip s -> loop i ~len s next
      | Yield (_, s) -> loop (i + 1) ~len s next)
  in
  match s with
  | Sequence (s, next) -> loop 0 ~len s next
;;

let drop_while_option (Sequence (s, next)) ~f =
  let rec loop s =
    match next s with
    | Done -> None
    | Skip s -> loop s
    | Yield (x, s) -> if f x then loop s else Some (x, Sequence (s, next))
  in
  loop s
;;

let compare compare_a t1 t2 =
  With_return.with_return (fun r ->
    iter (zip_full t1 t2) ~f:(function
      | `Left _ -> r.return 1
      | `Right _ -> r.return (-1)
      | `Both (v1, v2) ->
        let c = compare_a v1 v2 in
        if c <> 0 then r.return c);
    0)
;;

let equal equal_a t1 t2 =
  for_all (zip_full t1 t2) ~f:(function
    | `Both (a1, a2) -> equal_a a1 a2
    | `Left _ | `Right _ -> false)
;;

let round_robin list =
  let next (todo_stack, done_stack) =
    match todo_stack with
    | Sequence (s, f) :: todo_stack ->
      (match f s with
       | Yield (x, s) -> Yield (x, (todo_stack, Sequence (s, f) :: done_stack))
       | Skip s -> Skip (Sequence (s, f) :: todo_stack, done_stack)
       | Done -> Skip (todo_stack, done_stack))
    | [] -> if List.is_empty done_stack then Done else Skip (List.rev done_stack, [])
  in
  let state = list, [] in
  Sequence (state, next)
;;

let interleave (Sequence (s1, f1)) =
  let next (todo_stack, done_stack, s1) =
    match todo_stack with
    | Sequence (s2, f2) :: todo_stack ->
      (match f2 s2 with
       | Yield (x, s2) -> Yield (x, (todo_stack, Sequence (s2, f2) :: done_stack, s1))
       | Skip s2 -> Skip (todo_stack, Sequence (s2, f2) :: done_stack, s1)
       | Done -> Skip (todo_stack, done_stack, s1))
    | [] ->
      (match f1 s1, done_stack with
       | Yield (t, s1), _ -> Skip (List.rev (t :: done_stack), [], s1)
       | Skip s1, _ -> Skip (List.rev done_stack, [], s1)
       | Done, _ :: _ -> Skip (List.rev done_stack, [], s1)
       | Done, [] -> Done)
  in
  let state = [], [], s1 in
  Sequence (state, next)
;;

let interleaved_cartesian_product s1 s2 =
  map s1 ~f:(fun x1 -> map s2 ~f:(fun x2 -> x1, x2)) |> interleave
;;

let of_seq (seq : _ Caml.Seq.t) =
  unfold_step ~init:seq ~f:(fun seq ->
    match seq () with
    | Nil -> Done
    | Cons (hd, tl) -> Yield (hd, tl))
;;

let to_seq (Sequence (state, next)) =
  let rec loop state =
    match next state with
    | Done -> Caml.Seq.Nil
    | Skip state -> loop state
    | Yield (hd, state) -> Caml.Seq.Cons (hd, fun () -> loop state)
  in
  fun () -> loop state
;;

module Generator = struct
  type 'elt steps = Wrap of ('elt, unit -> 'elt steps) Step.t

  let unwrap (Wrap step) = step

  module T = struct
    type ('a, 'elt) t = ('a -> 'elt steps) -> 'elt steps

    let return x k = k x

    let bind m ~f k =
      m (fun a ->
        let m' = f a in
        m' k)
    ;;

    let map m ~f k = m (fun a -> k (f a))
    let map = `Custom map
  end

  include T
  include Monad.Make2 (T)

  let yield e k = Wrap (Yield (e, k))
  let to_steps t = t (fun () -> Wrap Done)

  let of_sequence sequence =
    delayed_fold
      sequence
      ~init:()
      ~f:(fun () x ~k f -> Wrap (Yield (x, fun () -> k () f)))
      ~finish:return
  ;;

  let run t =
    let init () = to_steps t in
    let f thunk = unwrap (thunk ()) in
    unfold_step ~init ~f
  ;;
end
