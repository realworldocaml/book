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

let to_string { len; tree } =
  match tree with
  | Base s -> s
  | Append (s1, s2) ->
    let buf = Bytes.create len in
    (* [todo] avoids stack overflow (some usage patterns result in highly unbalanced
       trees, so the naive recursive approach doesn't work) *)
    let rec go todo start : Tree.t -> _ = function
      | Base s ->
        Bytes.From_string.blit
          ~src:s
          ~src_pos:0
          ~dst:buf
          ~dst_pos:start
          ~len:(String.length s);
        let start = start + String.length s in
        (match todo with
         | [] -> assert (start = len)
         | x :: xs -> go xs start x)
      | Append (s1, s2) -> go (s2 :: todo) start s1
    in
    go [ s2 ] 0 s1;
    Bytes.unsafe_to_string ~no_mutation_while_string_reachable:buf
;;

let to_char_sequence t = Tree.to_char_sequence t.tree

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
