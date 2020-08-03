open! Import
module Array = Array0
module Either = Either0


include List1

(* This itself includes [List0]. *)

let invalid_argf = Printf.invalid_argf

module T = struct
  type 'a t = 'a list [@@deriving_inline sexp, sexp_grammar]

  let t_of_sexp :
    'a. (Ppx_sexp_conv_lib.Sexp.t -> 'a) -> Ppx_sexp_conv_lib.Sexp.t -> 'a t
    =
    list_of_sexp
  ;;

  let sexp_of_t :
    'a. ('a -> Ppx_sexp_conv_lib.Sexp.t) -> 'a t -> Ppx_sexp_conv_lib.Sexp.t
    =
    sexp_of_list
  ;;

  let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
    let (_the_generic_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.generic_group) =
      { implicit_vars = [ "list" ]
      ; ggid = "j\132);\135qH\158\135\222H\001\007\004\158\218"
      ; types =
          [ "t", Explicit_bind ([ "a" ], Apply (Implicit_var 0, [ Explicit_var 0 ])) ]
      }
    in
    let (_the_group : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.group) =
      { gid = Ppx_sexp_conv_lib.Lazy_group_id.create ()
      ; apply_implicit = [ list_sexp_grammar ]
      ; generic_group = _the_generic_group
      ; origin = "list.ml.T"
      }
    in
    let (t_sexp_grammar : Ppx_sexp_conv_lib.Sexp.Private.Raw_grammar.t) =
      Ref ("t", _the_group)
    in
    t_sexp_grammar
  ;;

  [@@@end]
end

module Or_unequal_lengths = struct
  type 'a t =
    | Ok of 'a
    | Unequal_lengths
  [@@deriving_inline compare, sexp_of]

  let compare : 'a. ('a -> 'a -> int) -> 'a t -> 'a t -> int =
    fun _cmp__a a__001_ b__002_ ->
    if Ppx_compare_lib.phys_equal a__001_ b__002_
    then 0
    else (
      match a__001_, b__002_ with
      | Ok _a__003_, Ok _b__004_ -> _cmp__a _a__003_ _b__004_
      | Ok _, _ -> -1
      | _, Ok _ -> 1
      | Unequal_lengths, Unequal_lengths -> 0)
  ;;

  let sexp_of_t
    : type a. (a -> Ppx_sexp_conv_lib.Sexp.t) -> a t -> Ppx_sexp_conv_lib.Sexp.t
    =
    fun _of_a -> function
      | Ok v0 ->
        let v0 = _of_a v0 in
        Ppx_sexp_conv_lib.Sexp.List [ Ppx_sexp_conv_lib.Sexp.Atom "Ok"; v0 ]
      | Unequal_lengths -> Ppx_sexp_conv_lib.Sexp.Atom "Unequal_lengths"
  ;;

  [@@@end]
end

include T

let invariant f t = iter t ~f
let of_list t = t

let range' ~compare ~stride ?(start = `inclusive) ?(stop = `exclusive) start_i stop_i =
  let next_i = stride start_i in
  let order x y = Ordering.of_int (compare x y) in
  let raise_stride_cannot_return_same_value () =
    invalid_arg "List.range': stride function cannot return the same value"
  in
  let initial_stride_order =
    match order start_i next_i with
    | Equal -> raise_stride_cannot_return_same_value ()
    | Less -> `Less
    | Greater -> `Greater
  in
  let rec loop i accum =
    let i_to_stop_order = order i stop_i in
    match i_to_stop_order, initial_stride_order with
    | Less, `Less | Greater, `Greater ->
      (* haven't yet reached [stop_i]. Continue. *)
      let next_i = stride i in
      (match order i next_i, initial_stride_order with
       | Equal, _ -> raise_stride_cannot_return_same_value ()
       | Less, `Greater | Greater, `Less ->
         invalid_arg "List.range': stride function cannot change direction"
       | Less, `Less | Greater, `Greater -> loop next_i (i :: accum))
    | Less, `Greater | Greater, `Less ->
      (* stepped past [stop_i].  Finished. *)
      accum
    | Equal, _ ->
      (* reached [stop_i].  Finished. *)
      (match stop with
       | `inclusive -> i :: accum
       | `exclusive -> accum)
  in
  let start_i =
    match start with
    | `inclusive -> start_i
    | `exclusive -> next_i
  in
  rev (loop start_i [])
;;

let range ?(stride = 1) ?(start = `inclusive) ?(stop = `exclusive) start_i stop_i =
  if stride = 0 then invalid_arg "List.range: stride must be non-zero";
  range' ~compare ~stride:(fun x -> x + stride) ~start ~stop start_i stop_i
;;

let hd t =
  match t with
  | [] -> None
  | x :: _ -> Some x
;;

let tl t =
  match t with
  | [] -> None
  | _ :: t' -> Some t'
;;

let nth t n =
  if n < 0
  then None
  else (
    let rec nth_aux t n =
      match t with
      | [] -> None
      | a :: t -> if n = 0 then Some a else nth_aux t (n - 1)
    in
    nth_aux t n)
;;

let nth_exn t n =
  match nth t n with
  | None -> invalid_argf "List.nth_exn %d called on list of length %d" n (length t) ()
  | Some a -> a
;;

let unordered_append l1 l2 =
  match l1, l2 with
  | [], l | l, [] -> l
  | _ -> rev_append l1 l2
;;

let check_length2_exn name l1 l2 =
  let n1 = length l1 in
  let n2 = length l2 in
  if n1 <> n2 then invalid_argf "length mismatch in %s: %d <> %d" name n1 n2 ()
;;

let check_length3_exn name l1 l2 l3 =
  let n1 = length l1 in
  let n2 = length l2 in
  let n3 = length l3 in
  if n1 <> n2 || n2 <> n3
  then invalid_argf "length mismatch in %s: %d <> %d || %d <> %d" name n1 n2 n2 n3 ()
;;

let check_length2 l1 l2 ~f =
  if length l1 <> length l2 then Or_unequal_lengths.Unequal_lengths else Ok (f l1 l2)
;;

let check_length3 l1 l2 l3 ~f =
  let n1 = length l1 in
  let n2 = length l2 in
  let n3 = length l3 in
  if n1 <> n2 || n2 <> n3 then Or_unequal_lengths.Unequal_lengths else Ok (f l1 l2 l3)
;;

let iter2 l1 l2 ~f = check_length2 l1 l2 ~f:(iter2_ok ~f)

let iter2_exn l1 l2 ~f =
  check_length2_exn "iter2_exn" l1 l2;
  iter2_ok l1 l2 ~f
;;

let rev_map2 l1 l2 ~f = check_length2 l1 l2 ~f:(rev_map2_ok ~f)

let rev_map2_exn l1 l2 ~f =
  check_length2_exn "rev_map2_exn" l1 l2;
  rev_map2_ok l1 l2 ~f
;;

let fold2 l1 l2 ~init ~f = check_length2 l1 l2 ~f:(fold2_ok ~init ~f)

let fold2_exn l1 l2 ~init ~f =
  check_length2_exn "fold2_exn" l1 l2;
  fold2_ok l1 l2 ~init ~f
;;

let for_all2 l1 l2 ~f = check_length2 l1 l2 ~f:(for_all2_ok ~f)

let for_all2_exn l1 l2 ~f =
  check_length2_exn "for_all2_exn" l1 l2;
  for_all2_ok l1 l2 ~f
;;

let exists2 l1 l2 ~f = check_length2 l1 l2 ~f:(exists2_ok ~f)

let exists2_exn l1 l2 ~f =
  check_length2_exn "exists2_exn" l1 l2;
  exists2_ok l1 l2 ~f
;;

let mem t a ~equal =
  let rec loop equal a = function
    | [] -> false
    | b :: bs -> equal a b || loop equal a bs
  in
  loop equal a t
;;

(* This is a copy of the code from the standard library, with an extra eta-expansion to
   avoid creating partial closures (showed up for [filter]) in profiling). *)
let rev_filter t ~f =
  let rec find ~f accu = function
    | [] -> accu
    | x :: l -> if f x then find ~f (x :: accu) l else find ~f accu l
  in
  find ~f [] t
;;

let filter t ~f = rev (rev_filter t ~f)

let find_map t ~f =
  let rec loop = function
    | [] -> None
    | x :: l ->
      (match f x with
       | None -> loop l
       | Some _ as r -> r)
  in
  loop t
;;

let find_map_exn =
  let not_found = Not_found_s (Atom "List.find_map_exn: not found") in
  let find_map_exn t ~f =
    match find_map t ~f with
    | None -> raise not_found
    | Some x -> x
  in
  (* named to preserve symbol in compiled binary *)
  find_map_exn
;;

let find t ~f =
  let rec loop = function
    | [] -> None
    | x :: l -> if f x then Some x else loop l
  in
  loop t
;;

let find_exn =
  let not_found = Not_found_s (Atom "List.find_exn: not found") in
  let rec find_exn t ~f =
    match t with
    | [] -> raise not_found
    | x :: t -> if f x then x else find_exn t ~f
  in
  (* named to preserve symbol in compiled binary *)
  find_exn
;;

let findi t ~f =
  let rec loop i t =
    match t with
    | [] -> None
    | x :: l -> if f i x then Some (i, x) else loop (i + 1) l
  in
  loop 0 t
;;

let find_mapi t ~f =
  let rec loop i t =
    match t with
    | [] -> None
    | x :: l ->
      (match f i x with
       | Some _ as result -> result
       | None -> loop (i + 1) l)
  in
  loop 0 t
;;

let find_mapi_exn =
  let not_found = Not_found_s (Atom "List.find_mapi_exn: not found") in
  let find_mapi_exn t ~f =
    match find_mapi t ~f with
    | None -> raise not_found
    | Some x -> x
  in
  (* named to preserve symbol in compiled binary *)
  find_mapi_exn
;;

let for_alli t ~f =
  let rec loop i t =
    match t with
    | [] -> true
    | hd :: tl -> f i hd && loop (i + 1) tl
  in
  loop 0 t
;;

let existsi t ~f =
  let rec loop i t =
    match t with
    | [] -> false
    | hd :: tl -> f i hd || loop (i + 1) tl
  in
  loop 0 t
;;

(** For the container interface. *)
let fold_left = fold

let to_array = Array.of_list
let to_list t = t

(** Tail recursive versions of standard [List] module *)

let slow_append l1 l2 = rev_append (rev l1) l2

(* There are a few optimized list operations here, including append and map.  There are
   basically two optimizations in play: loop unrolling, and dynamic switching between
   stack and heap allocation.

   The loop-unrolling is straightforward, we just unroll 5 levels of the loop.  This makes
   each iteration faster, and also reduces the number of stack frames consumed per list
   element.

   The dynamic switching is done by counting the number of stack frames, and then
   switching to the "slow" implementation when we exceed a given limit.  This means that
   short lists use the fast stack-allocation method, and long lists use a slower one that
   doesn't require stack space. *)
let rec count_append l1 l2 count =
  match l2 with
  | [] -> l1
  | _ ->
    (match l1 with
     | [] -> l2
     | [ x1 ] -> x1 :: l2
     | [ x1; x2 ] -> x1 :: x2 :: l2
     | [ x1; x2; x3 ] -> x1 :: x2 :: x3 :: l2
     | [ x1; x2; x3; x4 ] -> x1 :: x2 :: x3 :: x4 :: l2
     | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
       x1
       :: x2
       :: x3
       :: x4
       :: x5
       :: (if count > 1000 then slow_append tl l2 else count_append tl l2 (count + 1)))
;;

let append l1 l2 = count_append l1 l2 0
let slow_map l ~f = rev (rev_map l ~f)

let rec count_map ~f l ctr =
  match l with
  | [] -> []
  | [ x1 ] ->
    let f1 = f x1 in
    [ f1 ]
  | [ x1; x2 ] ->
    let f1 = f x1 in
    let f2 = f x2 in
    [ f1; f2 ]
  | [ x1; x2; x3 ] ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    [ f1; f2; f3 ]
  | [ x1; x2; x3; x4 ] ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    let f4 = f x4 in
    [ f1; f2; f3; f4 ]
  | x1 :: x2 :: x3 :: x4 :: x5 :: tl ->
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    let f4 = f x4 in
    let f5 = f x5 in
    f1
    :: f2
    :: f3
    :: f4
    :: f5
    :: (if ctr > 1000 then slow_map ~f tl else count_map ~f tl (ctr + 1))
;;

let map l ~f = count_map ~f l 0

let folding_map t ~init ~f =
  let acc = ref init in
  map t ~f:(fun x ->
    let new_acc, y = f !acc x in
    acc := new_acc;
    y)
;;

let fold_map t ~init ~f =
  let acc = ref init in
  let result =
    map t ~f:(fun x ->
      let new_acc, y = f !acc x in
      acc := new_acc;
      y)
  in
  !acc, result
;;

let ( >>| ) l f = map l ~f
let map2_ok l1 l2 ~f = rev (rev_map2_ok l1 l2 ~f)
let map2 l1 l2 ~f = check_length2 l1 l2 ~f:(map2_ok ~f)

let map2_exn l1 l2 ~f =
  check_length2_exn "map2_exn" l1 l2;
  map2_ok l1 l2 ~f
;;

let rev_map3_ok l1 l2 l3 ~f =
  let rec loop l1 l2 l3 ac =
    match l1, l2, l3 with
    | [], [], [] -> ac
    | x1 :: l1, x2 :: l2, x3 :: l3 -> loop l1 l2 l3 (f x1 x2 x3 :: ac)
    | _ -> assert false
  in
  loop l1 l2 l3 []
;;

let rev_map3 l1 l2 l3 ~f = check_length3 l1 l2 l3 ~f:(rev_map3_ok ~f)

let rev_map3_exn l1 l2 l3 ~f =
  check_length3_exn "rev_map3_exn" l1 l2 l3;
  rev_map3_ok l1 l2 l3 ~f
;;

let map3_ok l1 l2 l3 ~f = rev (rev_map3_ok l1 l2 l3 ~f)
let map3 l1 l2 l3 ~f = check_length3 l1 l2 l3 ~f:(map3_ok ~f)

let map3_exn l1 l2 l3 ~f =
  check_length3_exn "map3_exn" l1 l2 l3;
  map3_ok l1 l2 l3 ~f
;;

let rec rev_map_append l1 l2 ~f =
  match l1 with
  | [] -> l2
  | h :: t -> rev_map_append ~f t (f h :: l2)
;;

let fold_right l ~f ~init =
  match l with
  | [] -> init (* avoid the allocation of [~f] below *)
  | _ -> fold ~f:(fun a b -> f b a) ~init (rev l)
;;

let unzip list =
  let rec loop list l1 l2 =
    match list with
    | [] -> rev l1, rev l2
    | (x, y) :: tl -> loop tl (x :: l1) (y :: l2)
  in
  loop list [] []
;;

let unzip3 list =
  let rec loop list l1 l2 l3 =
    match list with
    | [] -> rev l1, rev l2, rev l3
    | (x, y, z) :: tl -> loop tl (x :: l1) (y :: l2) (z :: l3)
  in
  loop list [] [] []
;;

let zip_exn l1 l2 =
  check_length2_exn "zip_exn" l1 l2;
  map2_ok ~f:(fun a b -> a, b) l1 l2
;;

let zip l1 l2 = map2 ~f:(fun a b -> a, b) l1 l2

(** Additional list operations *)

let rev_mapi l ~f =
  let rec loop i acc = function
    | [] -> acc
    | h :: t -> loop (i + 1) (f i h :: acc) t
  in
  loop 0 [] l
;;

let mapi l ~f = rev (rev_mapi l ~f)

let folding_mapi t ~init ~f =
  let acc = ref init in
  mapi t ~f:(fun i x ->
    let new_acc, y = f i !acc x in
    acc := new_acc;
    y)
;;

let fold_mapi t ~init ~f =
  let acc = ref init in
  let result =
    mapi t ~f:(fun i x ->
      let new_acc, y = f i !acc x in
      acc := new_acc;
      y)
  in
  !acc, result
;;

let iteri l ~f =
  ignore
    (fold l ~init:0 ~f:(fun i x ->
       f i x;
       i + 1)
     : int)
;;

let foldi t ~init ~f =
  snd (fold t ~init:(0, init) ~f:(fun (i, acc) v -> i + 1, f i acc v))
;;

let filteri l ~f =
  rev (foldi l ~f:(fun pos acc x -> if f pos x then x :: acc else acc) ~init:[])
;;

let reduce l ~f =
  match l with
  | [] -> None
  | hd :: tl -> Some (fold ~init:hd ~f tl)
;;

let reduce_exn l ~f =
  match reduce l ~f with
  | None -> invalid_arg "List.reduce_exn"
  | Some v -> v
;;

let reduce_balanced l ~f =
  (* Call the "size" of a value the number of list elements that have been combined into
     it via calls to [f].  We proceed by using [f] to combine elements in the accumulator
     of the same size until we can't combine any more, then getting a new element from the
     input list and repeating.

     With this strategy, in the accumulator:
     - we only ever have elements of sizes a power of two
     - we never have more than one element of each size
     - the sum of all the element sizes is equal to the number of elements consumed

     These conditions enforce that list of elements of each size is precisely the binary
     expansion of the number of elements consumed: if you've consumed 13 = 0b1101
     elements, you have one element of size 8, one of size 4, and one of size 1.  Hence
     when a new element comes along, the number of combinings you need to do is the number
     of trailing 1s in the binary expansion of [num], the number of elements that have
     already gone into the accumulator.  The accumulator is in ascending order of size, so
     the next element to combine with is always the head of the list. *)
  let rec step_accum num acc x =
    if num land 1 = 0
    then x :: acc
    else (
      match acc with
      | [] -> assert false
      (* New elements from later in the input list go on the front of the accumulator, so
         the accumulator is in reverse order wrt the original list order, hence [f y x]
         instead of [f x y]. *)
      | y :: ys -> step_accum (num asr 1) ys (f y x))
  in
  (* Experimentally, inlining [foldi] and unrolling this loop a few times can reduce
     runtime down to a third and allocation to 1/16th or so in the microbenchmarks below.
     However, in most use cases [f] is likely to be expensive (otherwise why do you care
     about the order of reduction?) so the overhead of this function itself doesn't really
     matter. If you come up with a use-case where it does, then that's something you might
     want to try: see hg log -pr 49ef065f429d. *)
  match foldi l ~init:[] ~f:step_accum with
  | [] -> None
  | x :: xs -> Some (fold xs ~init:x ~f:(fun x y -> f y x))
;;

let reduce_balanced_exn l ~f =
  match reduce_balanced l ~f with
  | None -> invalid_arg "List.reduce_balanced_exn"
  | Some v -> v
;;

let groupi l ~break =
  let groups =
    foldi l ~init:[] ~f:(fun i acc x ->
      match acc with
      | [] -> [ [ x ] ]
      | current_group :: tl ->
        if break i (hd_exn current_group) x
        then [ x ] :: current_group :: tl (* start new group *)
        else (x :: current_group) :: tl)
    (* extend current group *)
  in
  match groups with
  | [] -> []
  | l -> rev_map l ~f:rev
;;

let group l ~break = groupi l ~break:(fun _ x y -> break x y)

let concat_map l ~f =
  let rec aux acc = function
    | [] -> rev acc
    | hd :: tl -> aux (rev_append (f hd) acc) tl
  in
  aux [] l
;;

let concat_mapi l ~f =
  let rec aux cont acc = function
    | [] -> rev acc
    | hd :: tl -> aux (cont + 1) (rev_append (f cont hd) acc) tl
  in
  aux 0 [] l
;;

let merge l1 l2 ~compare =
  let rec loop acc l1 l2 =
    match l1, l2 with
    | [], l2 -> rev_append acc l2
    | l1, [] -> rev_append acc l1
    | h1 :: t1, h2 :: t2 ->
      if compare h1 h2 <= 0 then loop (h1 :: acc) t1 l2 else loop (h2 :: acc) l1 t2
  in
  loop [] l1 l2
;;

include struct
  (* We are explicit about what we import from the general Monad functor so that we don't
     accidentally rebind more efficient list-specific functions. *)
  module Monad = Monad.Make (struct
      type 'a t = 'a list

      let bind x ~f = concat_map x ~f
      let map = `Custom map
      let return x = [ x ]
    end)

  open Monad
  module Monad_infix = Monad_infix
  module Let_syntax = Let_syntax

  let ignore_m = ignore_m
  let join = join
  let bind = bind
  let ( >>= ) t f = bind t ~f
  let return = return
  let all = all
  let all_unit = all_unit
end

(** returns final element of list *)
let rec last_exn list =
  match list with
  | [ x ] -> x
  | _ :: tl -> last_exn tl
  | [] -> invalid_arg "List.last"
;;

(** optionally returns final element of list *)
let rec last list =
  match list with
  | [ x ] -> Some x
  | _ :: tl -> last tl
  | [] -> None
;;

let rec is_prefix list ~prefix ~equal =
  match prefix with
  | [] -> true
  | hd :: tl ->
    (match list with
     | [] -> false
     | hd' :: tl' -> equal hd hd' && is_prefix tl' ~prefix:tl ~equal)
;;

let find_consecutive_duplicate t ~equal =
  match t with
  | [] -> None
  | a1 :: t ->
    let rec loop a1 t =
      match t with
      | [] -> None
      | a2 :: t -> if equal a1 a2 then Some (a1, a2) else loop a2 t
    in
    loop a1 t
;;

(* returns list without adjacent duplicates *)
let remove_consecutive_duplicates ?(which_to_keep = `Last) list ~equal =
  let rec loop to_keep accum = function
    | [] -> to_keep :: accum
    | hd :: tl ->
      if equal hd to_keep
      then (
        let to_keep =
          match which_to_keep with
          | `First -> to_keep
          | `Last -> hd
        in
        loop to_keep accum tl)
      else loop hd (to_keep :: accum) tl
  in
  match list with
  | [] -> []
  | hd :: tl -> rev (loop hd [] tl)
;;

(** returns sorted version of list with duplicates removed *)
let dedup_and_sort ~compare list =
  match list with
  | [] | [ _ ] -> list (* performance hack *)
  | _ ->
    let equal x x' = compare x x' = 0 in
    let sorted = sort ~compare list in
    remove_consecutive_duplicates ~equal sorted
;;

let find_a_dup ~compare l =
  let sorted = sort ~compare l in
  let rec loop l =
    match l with
    | [] | [ _ ] -> None
    | hd1 :: (hd2 :: _ as tl) -> if compare hd1 hd2 = 0 then Some hd1 else loop tl
  in
  loop sorted
;;

let contains_dup ~compare lst =
  match find_a_dup ~compare lst with
  | Some _ -> true
  | None -> false
;;

let find_all_dups ~compare l =
  (* We add this reversal, so we can skip a [rev] at the end. We could skip
     [rev] anyway since we don not give any ordering guarantees, but it is
     nice to get results in natural order. *)
  let compare a b = -1 * compare a b in
  let sorted = sort ~compare l in
  (* Walk the list and record the first of each consecutive run of identical elements *)
  let rec loop sorted prev ~already_recorded acc =
    match sorted with
    | [] -> acc
    | hd :: tl ->
      if compare prev hd <> 0
      then loop tl hd ~already_recorded:false acc
      else if already_recorded
      then loop tl hd ~already_recorded:true acc
      else loop tl hd ~already_recorded:true (hd :: acc)
  in
  match sorted with
  | [] -> []
  | hd :: tl -> loop tl hd ~already_recorded:false []
;;

let count t ~f = Container.count ~fold t ~f
let sum m t ~f = Container.sum ~fold m t ~f
let min_elt t ~compare = Container.min_elt ~fold t ~compare
let max_elt t ~compare = Container.max_elt ~fold t ~compare

let counti t ~f =
  foldi t ~init:0 ~f:(fun idx count a -> if f idx a then count + 1 else count)
;;

let init n ~f =
  if n < 0 then invalid_argf "List.init %d" n ();
  let rec loop i accum =
    assert (i >= 0);
    if i = 0 then accum else loop (i - 1) (f (i - 1) :: accum)
  in
  loop n []
;;

let rev_filter_map l ~f =
  let rec loop l accum =
    match l with
    | [] -> accum
    | hd :: tl ->
      (match f hd with
       | Some x -> loop tl (x :: accum)
       | None -> loop tl accum)
  in
  loop l []
;;

let filter_map l ~f = rev (rev_filter_map l ~f)

let rev_filter_mapi l ~f =
  let rec loop i l accum =
    match l with
    | [] -> accum
    | hd :: tl ->
      (match f i hd with
       | Some x -> loop (i + 1) tl (x :: accum)
       | None -> loop (i + 1) tl accum)
  in
  loop 0 l []
;;

let filter_mapi l ~f = rev (rev_filter_mapi l ~f)
let filter_opt l = filter_map l ~f:Fn.id

let partition3_map t ~f =
  let rec loop t fst snd trd =
    match t with
    | [] -> rev fst, rev snd, rev trd
    | x :: t ->
      (match f x with
       | `Fst y -> loop t (y :: fst) snd trd
       | `Snd y -> loop t fst (y :: snd) trd
       | `Trd y -> loop t fst snd (y :: trd))
  in
  loop t [] [] []
;;

let partition_tf t ~f =
  let f x : _ Either.t = if f x then First x else Second x in
  partition_map t ~f
;;

let partition_result t = partition_map t ~f:Result.to_either

module Assoc = struct
  type ('a, 'b) t = ('a * 'b) list [@@deriving_inline sexp]

  let t_of_sexp :
    'a 'b. (Ppx_sexp_conv_lib.Sexp.t -> 'a) -> (Ppx_sexp_conv_lib.Sexp.t -> 'b)
    -> Ppx_sexp_conv_lib.Sexp.t -> ('a, 'b) t
    =
    let _tp_loc = "list.ml.Assoc.t" in
    fun _of_a _of_b t ->
      list_of_sexp
        (function
          | Ppx_sexp_conv_lib.Sexp.List [ v0; v1 ] ->
            let v0 = _of_a v0
            and v1 = _of_b v1 in
            v0, v1
          | sexp -> Ppx_sexp_conv_lib.Conv_error.tuple_of_size_n_expected _tp_loc 2 sexp)
        t
  ;;

  let sexp_of_t :
    'a 'b. ('a -> Ppx_sexp_conv_lib.Sexp.t) -> ('b -> Ppx_sexp_conv_lib.Sexp.t)
    -> ('a, 'b) t -> Ppx_sexp_conv_lib.Sexp.t
    =
    fun _of_a _of_b v ->
      sexp_of_list
        (function
          | v0, v1 ->
            let v0 = _of_a v0
            and v1 = _of_b v1 in
            Ppx_sexp_conv_lib.Sexp.List [ v0; v1 ])
        v
  ;;

  [@@@end]

  let find t ~equal key =
    match find t ~f:(fun (key', _) -> equal key key') with
    | None -> None
    | Some x -> Some (snd x)
  ;;

  let find_exn =
    let not_found = Not_found_s (Atom "List.Assoc.find_exn: not found") in
    let find_exn t ~equal key =
      match find t key ~equal with
      | None -> raise not_found
      | Some value -> value
    in
    (* named to preserve symbol in compiled binary *)
    find_exn
  ;;

  let mem t ~equal key =
    match find t ~equal key with
    | None -> false
    | Some _ -> true
  ;;

  let remove t ~equal key = filter t ~f:(fun (key', _) -> not (equal key key'))

  let add t ~equal key value =
    (* the remove doesn't change the map semantics, but keeps the list small *)
    (key, value) :: remove t ~equal key
  ;;

  let inverse t = map t ~f:(fun (x, y) -> y, x)
  let map t ~f = map t ~f:(fun (key, value) -> key, f value)
end

let sub l ~pos ~len =
  (* We use [pos > length l - len] rather than [pos + len > length l] to avoid the
     possibility of overflow. *)
  if pos < 0 || len < 0 || pos > length l - len then invalid_arg "List.sub";
  rev
    (foldi l ~init:[] ~f:(fun i acc el ->
       if i >= pos && i < pos + len then el :: acc else acc))
;;

let split_n t_orig n =
  if n <= 0
  then [], t_orig
  else (
    let rec loop n t accum =
      if n = 0
      then rev accum, t
      else (
        match t with
        | [] -> t_orig, [] (* in this case, t_orig = rev accum *)
        | hd :: tl -> loop (n - 1) tl (hd :: accum))
    in
    loop n t_orig [])
;;

(* copied from [split_n] to avoid allocating a tuple *)
let take t_orig n =
  if n <= 0
  then []
  else (
    let rec loop n t accum =
      if n = 0
      then rev accum
      else (
        match t with
        | [] -> t_orig
        | hd :: tl -> loop (n - 1) tl (hd :: accum))
    in
    loop n t_orig [])
;;

let rec drop t n =
  match t with
  | _ :: tl when n > 0 -> drop tl (n - 1)
  | t -> t
;;

let chunks_of l ~length =
  if length <= 0
  then invalid_argf "List.chunks_of: Expected length > 0, got %d" length ();
  let rec aux of_length acc l =
    match l with
    | [] -> rev acc
    | _ :: _ ->
      let sublist, l = split_n l length in
      aux of_length (sublist :: acc) l
  in
  aux length [] l
;;

let split_while xs ~f =
  let rec loop acc = function
    | hd :: tl when f hd -> loop (hd :: acc) tl
    | t -> rev acc, t
  in
  loop [] xs
;;

(* copied from [split_while] to avoid allocating a tuple *)
let take_while xs ~f =
  let rec loop acc = function
    | hd :: tl when f hd -> loop (hd :: acc) tl
    | _ -> rev acc
  in
  loop [] xs
;;

let rec drop_while t ~f =
  match t with
  | hd :: tl when f hd -> drop_while tl ~f
  | t -> t
;;

let drop_last t =
  match rev t with
  | [] -> None
  | _ :: lst -> Some (rev lst)
;;

let drop_last_exn t =
  match drop_last t with
  | None -> failwith "List.drop_last_exn: empty list"
  | Some lst -> lst
;;

let cartesian_product list1 list2 =
  if is_empty list2
  then []
  else (
    let rec loop l1 l2 accum =
      match l1 with
      | [] -> accum
      | hd :: tl -> loop tl l2 (rev_append (map ~f:(fun x -> hd, x) l2) accum)
    in
    rev (loop list1 list2 []))
;;

let concat l = fold_right l ~init:[] ~f:append
let concat_no_order l = fold l ~init:[] ~f:(fun acc l -> rev_append l acc)
let cons x l = x :: l

let is_sorted l ~compare =
  let rec loop l =
    match l with
    | [] | [ _ ] -> true
    | x1 :: (x2 :: _ as rest) -> compare x1 x2 <= 0 && loop rest
  in
  loop l
;;

let is_sorted_strictly l ~compare =
  let rec loop l =
    match l with
    | [] | [ _ ] -> true
    | x1 :: (x2 :: _ as rest) -> compare x1 x2 < 0 && loop rest
  in
  loop l
;;

module Infix = struct
  let ( @ ) = append
end

let permute ?(random_state = Random.State.default) list =
  match list with
  (* special cases to speed things up in trivial cases *)
  | [] | [ _ ] -> list
  | [ x; y ] -> if Random.State.bool random_state then [ y; x ] else list
  | _ ->
    let arr = Array.of_list list in
    Array_permute.permute arr ~random_state;
    Array.to_list arr
;;

let random_element_exn ?(random_state = Random.State.default) list =
  if is_empty list
  then failwith "List.random_element_exn: empty list"
  else nth_exn list (Random.State.int random_state (length list))
;;

let random_element ?(random_state = Random.State.default) list =
  try Some (random_element_exn ~random_state list) with
  | _ -> None
;;

let rec compare cmp a b =
  match a, b with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | x :: xs, y :: ys ->
    let n = cmp x y in
    if n = 0 then compare cmp xs ys else n
;;

let hash_fold_t = hash_fold_list

let equal equal t1 t2 =
  let rec loop ~equal t1 t2 =
    match t1, t2 with
    | [], [] -> true
    | x1 :: t1, x2 :: t2 -> equal x1 x2 && loop ~equal t1 t2
    | _ -> false
  in
  loop ~equal t1 t2
;;

let transpose =
  let rec transpose_aux t rev_columns =
    match
      partition_map t ~f:(function
        | [] -> Second ()
        | x :: xs -> First (x, xs))
    with
    | _ :: _, _ :: _ -> None
    | [], _ -> Some (rev_append rev_columns [])
    | heads_and_tails, [] ->
      let column, trimmed_rows = unzip heads_and_tails in
      transpose_aux trimmed_rows (column :: rev_columns)
  in
  fun t -> transpose_aux t []
;;

exception Transpose_got_lists_of_different_lengths of int list [@@deriving_inline sexp]

let () =
  Ppx_sexp_conv_lib.Conv.Exn_converter.add
    [%extension_constructor Transpose_got_lists_of_different_lengths]
    (function
      | Transpose_got_lists_of_different_lengths v0 ->
        let v0 = sexp_of_list sexp_of_int v0 in
        Ppx_sexp_conv_lib.Sexp.List
          [ Ppx_sexp_conv_lib.Sexp.Atom
              "list.ml.Transpose_got_lists_of_different_lengths"
          ; v0
          ]
      | _ -> assert false)
;;

[@@@end]

let transpose_exn l =
  match transpose l with
  | Some l -> l
  | None -> raise (Transpose_got_lists_of_different_lengths (map l ~f:length))
;;

let intersperse t ~sep =
  match t with
  | [] -> []
  | x :: xs -> x :: fold_right xs ~init:[] ~f:(fun y acc -> sep :: y :: acc)
;;

let fold_result t ~init ~f = Container.fold_result ~fold ~init ~f t
let fold_until t ~init ~f = Container.fold_until ~fold ~init ~f t

let is_suffix list ~suffix ~equal:equal_elt =
  let list_len = length list in
  let suffix_len = length suffix in
  list_len >= suffix_len && equal equal_elt (drop list (list_len - suffix_len)) suffix
;;
