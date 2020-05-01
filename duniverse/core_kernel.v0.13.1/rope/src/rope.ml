open! Core_kernel

(* Invariants:

   - [Append (x, y)] must have both [x] and [y] non-empty (complexity analysis
     of [to_string] relies on it).
   - Overall length is less than [String.max_length] (so [to_string] can work, at least in
     principle). *)
module Tree = struct
  type t =
    | Base of string
    | Append of t * t

  let rec unroll t aux =
    match t with
    | Base x -> x, aux
    | Append (x, y) -> unroll x (y :: aux)
  ;;

  let to_char_sequence t =
    let f (((x, xs) as xxs), xpos) : _ Sequence.Step.t =
      if xpos < String.length x
      then Yield (x.[xpos], (xxs, xpos + 1))
      else (
        match xs with
        | [] -> Done
        | y :: ys -> Skip (unroll y ys, 0))
    in
    Sequence.unfold_step ~init:(unroll t [], 0) ~f
  ;;

  let either_is_prefix_of_other t1 t2 =
    Sequence.for_all
      (Sequence.zip (to_char_sequence t1) (to_char_sequence t2))
      ~f:(fun (x, y) -> Char.equal x y)
  ;;
end

type t =
  { len : int
  ; tree : Tree.t
  }

let of_string s = { len = String.length s; tree = Base s }
let empty = of_string ""
let length t = t.len
let is_empty t = length t = 0

module To_string = struct
  (* [todo_right] avoids stack overflow (some usage patterns result in highly
     unbalanced trees, so the naive recursive approach doesn't work). However we can
     avoid that allocation by using the process stack when the depth appears small, as
     inspired by [Base.List.map].  This is sufficient to make the common case do
     zero minor-heap allocations.

     Using unsafe blitting substantially improves performance, but depends on the
     correctness of the [len] field to avoid memory corruption. To be precise, if [len] is
     too small the code may (but also may not, if you're lucky) write past the bounds of
     the buffer. If [len] is too large, we always write in-bounds, but will leave some of
     the buffer uninitialized. In either case if we don't corrupt memory (if we do all
     bets are off) then the assert at the end should fail, so we won't actually return the
     bad data to the caller. *)

  let rec unsafe_blit_allocate_tailcall ~dst ~todo_right ~left tree : int =
    match (tree : Tree.t) with
    | Append (t1, t2) ->
      unsafe_blit_allocate_tailcall ~dst ~todo_right:(t2 :: todo_right) ~left t1
    | Base s ->
      let left =
        let len_s = String.length s in
        Bytes.From_string.unsafe_blit ~src:s ~src_pos:0 ~dst ~dst_pos:left ~len:len_s;
        left + len_s
      in
      (match todo_right with
       | [] -> left
       | tree :: todo_right -> unsafe_blit_allocate_tailcall ~dst ~todo_right ~left tree)
  ;;

  (* We call this function when we're recursing into a left subtree but we don't know the
     size of the right subtree, so we don't know how much of the buffer we'll write into.
  *)
  let rec unsafe_blit_fast_partial ~dst ~left ~depth tree : int =
    match (tree : Tree.t) with
    | Append (t1, t2) ->
      let left =
        (* Only check the [depth] when we plan to increase it. *)
        if depth > 1000
        then unsafe_blit_allocate_tailcall ~dst ~todo_right:[] ~left t1
        else unsafe_blit_fast_partial ~dst ~left ~depth:(depth + 1) t1
      in
      unsafe_blit_fast_partial ~dst ~left ~depth t2
    | Base s ->
      let len_s = String.length s in
      Bytes.From_string.unsafe_blit ~src:s ~src_pos:0 ~dst ~dst_pos:left ~len:len_s;
      left + len_s
  ;;

  (* We call this function when we know both bounds of the data we'll write. *)
  let rec unsafe_blit_fast_entire_range ~dst ~left ~right tree =
    match (tree : Tree.t) with
    | Append (t1, Base s2) ->
      (* Optimization: preserve tailcall by blitting from the right. We can only do this
         when we know [right], so we can't do it once we recurse into the left of
         [Append (Append _, Append _)] *)
      let len_s = String.length s2 in
      let right = right - len_s in
      Bytes.From_string.unsafe_blit ~src:s2 ~src_pos:0 ~dst ~dst_pos:right ~len:len_s;
      unsafe_blit_fast_entire_range ~dst ~left ~right t1
    | Append (t1, t2) ->
      let left = unsafe_blit_fast_partial ~dst ~left ~depth:1 t1 in
      unsafe_blit_fast_entire_range ~dst ~left ~right t2
    | Base s ->
      let len_s = String.length s in
      (* This assert is not expensive since it can occur only once per [to_string] *)
      assert (left + len_s = right);
      Bytes.From_string.unsafe_blit ~src:s ~src_pos:0 ~dst ~dst_pos:left ~len:len_s
  ;;

  let to_string { len; tree } =
    match tree with
    | Base s -> s
    | Append _ ->
      let buf = Bytes.create len in
      unsafe_blit_fast_entire_range ~dst:buf ~left:0 ~right:len tree;
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
  ;;
end

let to_string = To_string.to_string
let to_char_sequence t = Tree.to_char_sequence t.tree

include Sexpable.Of_stringable (struct
    type nonrec t = t

    let to_string = to_string
    let of_string = of_string
  end)

(* We could loosen the [String.max_length] length restriction, since people can still read
   an arbitrary-length sequence out of [to_char_sequence]. I choose not to do this because
   I think [to_string] will be the more popular choice, and I'd prefer for it not to be
   able to raise. If someone else chooses differently, we'll likely still want to check
   against [Int.max_value]. *)
let ( ^ ) a b =
  if is_empty a
  then b
  else if is_empty b
  then a
  else if String.max_length - a.len < b.len
  then
    Error.raise_s
      [%message
        "Rope.(a ^ b) would be longer than String.max_length"
          (length a : int)
          (length b : int)
          (String.max_length : int)]
  else { len = a.len + b.len; tree = Append (a.tree, b.tree) }
;;

let concat ?(sep = empty) ts =
  List.reduce ts ~f:(fun x y -> x ^ sep ^ y) |> Option.value ~default:empty
;;

let concat_array ?(sep = empty) ts =
  Array.reduce ts ~f:(fun x y -> x ^ sep ^ y) |> Option.value ~default:empty
;;

let rec add_to_buffer_internal buffer todo : Tree.t -> _ = function
  | Append (s1, s2) -> add_to_buffer_internal buffer (s2 :: todo) s1
  | Base s ->
    Buffer.add_string buffer s;
    (match todo with
     | [] -> ()
     | x :: xs -> add_to_buffer_internal buffer xs x)
;;

let add_to_buffer { len = _; tree } buffer = add_to_buffer_internal buffer [] tree

let is_prefix t ~prefix =
  prefix.len <= t.len && Tree.either_is_prefix_of_other t.tree prefix.tree
;;

let equal a b = a.len = b.len && Tree.either_is_prefix_of_other a.tree b.tree

let quickcheck_generator =
  Quickcheck.Generator.weighted_union
    [ 1., Quickcheck.Generator.singleton empty
    ; ( 100.
      , Quickcheck.Generator.recursive_union
          [ Quickcheck.Generator.map String.gen_nonempty ~f:of_string ]
          ~f:(fun t ->
            [ Quickcheck.Generator.map2 t t ~f:( ^ )
            ; Quickcheck.Generator.map2 t String.gen_nonempty ~f:(fun left right ->
                left ^ of_string right)
            ; Quickcheck.Generator.map2 String.gen_nonempty t ~f:(fun left right ->
                of_string left ^ right)
            ]) )
    ]
;;

module T_deriving_hash = struct
  type nonrec t = t

  let hash_fold_t acc t = String.hash_fold_t acc (to_string t)
  let hash t = String.hash (to_string t)
end

let quickcheck_observer = Quickcheck.Observer.of_hash (module T_deriving_hash)

let quickcheck_shrinker =
  let of_tree tree =
    { len =
        (let rec go todo total t =
           match (t : Tree.t) with
           | Append (t1, t2) -> go (t1 :: todo) total t2
           | Base s ->
             let total = String.length s + total in
             (match todo with
              | [] -> total
              | t :: todo -> go todo total t)
         in
         go [] 0 tree)
    ; tree
    }
  in
  Quickcheck.Shrinker.create (fun t ->
    match t.tree with
    | Base string ->
      Sequence.map
        ~f:of_string
        (Quickcheck.Shrinker.shrink String.quickcheck_shrinker string)
    | Append (left, right) -> Sequence.of_list [ of_tree left; of_tree right ])
;;

module For_testing = struct
  module Tree = struct
    type t = Tree.t =
      | Base of string
      | Append of t * t
    [@@deriving sexp_of]
  end

  type nonrec t = t =
    { len : int
    ; tree : Tree.t
    }
  [@@deriving sexp_of]

  let num_bases { len = _; tree } =
    let rec go todo acc = function
      | Tree.Append (t1, t2) -> go (t2 :: todo) acc t1
      | Base _ ->
        let acc = acc + 1 in
        (match todo with
         | [] -> acc
         | t :: ts -> go ts acc t)
    in
    go [] 0 tree
  ;;

  let to_string_tailcall ({ len; tree } as t) =
    match tree with
    | Base s -> s
    | Append _ ->
      let buf = Bytes.create len in
      let left =
        To_string.unsafe_blit_allocate_tailcall ~dst:buf ~todo_right:[] ~left:0 t.tree
      in
      assert (left = len);
      Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
  ;;
end
