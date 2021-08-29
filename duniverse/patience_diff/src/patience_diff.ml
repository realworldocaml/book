open Core_kernel
open Core_kernel.Int.Replace_polymorphic_compare

let ( <|> ) ar (i, j) = if j <= i then [||] else Array.slice ar i j

module Ordered_sequence : sig
  type elt = int * int [@@deriving compare]

  (* A [t] has its second coordinates in increasing order *)

  type t = private elt array [@@deriving sexp_of]

  val create : (int * int) list -> t
  val is_empty : t -> bool
end = struct
  type elt = int * int [@@deriving sexp_of]

  let compare_elt =
    Comparable.lexicographic
      [ (fun (_, y0) (_, y1) -> Int.compare y0 y1)
      ; (fun (x0, _) (x1, _) -> Int.compare x0 x1)
      ]
  ;;

  type t = elt array [@@deriving sexp_of]

  let create l =
    let t = Array.of_list l in
    Array.sort t ~compare:compare_elt;
    t
  ;;

  let is_empty = Array.is_empty
end

(* This is an implementation of the patience sorting algorithm as explained at
   http://en.wikipedia.org/wiki/Patience_sorting *)
module Patience : sig
  val longest_increasing_subsequence : Ordered_sequence.t -> (int * int) list
end = struct
  module Pile = struct
    type 'a t = 'a Stack.t

    let create x =
      let t = Stack.create () in
      Stack.push t x;
      t
    ;;

    let top t = Stack.top t |> Option.value_exn
    let put_on_top t x = Stack.push t x
  end

  module Piles = struct
    type 'a t = 'a Pile.t Deque.t

    let empty () : 'a t = Deque.create ~never_shrink:true ()

    let get_ith_pile t i dir =
      let get index offset =
        Option.bind (index t) ~f:(fun index -> Deque.get_opt t (index + offset))
      in
      match dir with
      | `From_left -> get Deque.front_index i
      | `From_right -> get Deque.back_index (-i)
    ;;

    let new_rightmost_pile t pile = Deque.enqueue_back t pile
  end

  module Backpointers = struct
    (* in the terminology of the Wikipedia article, this corresponds to a card together
       with its backpointers *)
    type 'a tag = 'a t

    and 'a t =
      { value : 'a
      ; tag : 'a tag option
      }

    let to_list t =
      let rec to_list acc t =
        match t.tag with
        | None -> t.value :: acc
        | Some t' -> to_list (t.value :: acc) t'
      in
      to_list [] t
    ;;
  end

  module Play_patience : sig
    val play_patience
      :  Ordered_sequence.t
      -> get_tag:(pile_opt:int option
                  -> piles:Ordered_sequence.elt Backpointers.t Piles.t
                  -> Ordered_sequence.elt Backpointers.tag option)
      -> Ordered_sequence.elt Backpointers.t Piles.t
  end = struct
    let optimized_findi_from_left piles x =
      (* first see if any work *)
      let last_pile = Piles.get_ith_pile piles 0 `From_right in
      (* [x_pile] is a dummy pile just used for comparisons *)
      let x_pile = Pile.create { Backpointers.value = x, 0; tag = None } in
      let compare_top_values pile1 pile2 =
        let top pile = fst (Pile.top pile).Backpointers.value in
        Int.compare (top pile1) (top pile2)
      in
      let%bind.Option last_pile = last_pile in
      if compare_top_values last_pile x_pile < 0
      then None
      else
        (* do binary search *)
        Deque.binary_search
          piles
          `First_strictly_greater_than
          x_pile
          ~compare:compare_top_values
    ;;

    (* [play_patience ar ~get_tag] plays patience with the greedy algorithm as described
       in the Wikipedia article, taking [ar] to be the deck of cards.  It returns the
       resulting [Piles.t].  Before putting an element of [ar] in a pile, it tags it using
       [get_tag].  [get_tag] takes as its arguments the full [Piles.t] in its current
       state, and also the specific [Pile.t] that the element of [ar] is being added to.
    *)
    let play_patience ar ~get_tag =
      let ar = (ar : Ordered_sequence.t :> Ordered_sequence.elt array) in
      if Array.length ar = 0 then raise (Invalid_argument "Patience_diff.play_patience");
      let piles = Piles.empty () in
      Array.iter ar ~f:(fun x ->
        let pile_opt = optimized_findi_from_left piles (fst x) in
        let tagged_x = { Backpointers.value = x; tag = get_tag ~pile_opt ~piles } in
        match pile_opt with
        | None -> Piles.new_rightmost_pile piles (Pile.create tagged_x)
        | Some i ->
          let pile = Deque.get piles i in
          Pile.put_on_top pile tagged_x);
      piles
    ;;
  end

  let longest_increasing_subsequence ar =
    if Ordered_sequence.is_empty ar
    then []
    else
      let module P = Play_patience in
      let get_tag ~pile_opt ~piles =
        match pile_opt with
        | None -> Piles.get_ith_pile piles 0 `From_right |> Option.map ~f:Pile.top
        | Some i ->
          if i = 0
          then None
          else
            Piles.get_ith_pile piles (i - 1) `From_left
            |> Option.value_exn
            |> Pile.top
            |> Option.some
      in
      let piles = P.play_patience ar ~get_tag in
      Piles.get_ith_pile piles 0 `From_right
      |> Option.value_exn
      |> Pile.top
      |> Backpointers.to_list
  ;;
end

let compare_int_pair = Tuple.T2.compare ~cmp1:Int.compare ~cmp2:Int.compare

let _longest_increasing_subsequence ar =
  let ar = (ar : Ordered_sequence.t :> (int * int) array) in
  let len = Array.length ar in
  if len <= 1
  then Array.to_list ar
  else (
    let maxlen = ref 0 in
    let m = Array.create ~len:(len + 1) (-1) in
    let pred = Array.create ~len:(len + 1) (-1) in
    for i = 0 to len - 1 do
      let p =
        Array.binary_search
          ~compare:Ordered_sequence.compare_elt
          ar
          `First_greater_than_or_equal_to
          ar.(i)
          ~len:(max (!maxlen - 1) 0)
          ~pos:1
        |> Option.value ~default:0
      in
      pred.(i) <- m.(p);
      if p = !maxlen || compare_int_pair ar.(i) ar.(p + 1) < 0
      then (
        m.(p + 1) <- i;
        if p + 1 > !maxlen then maxlen := p + 1)
    done;
    let rec loop ac p = if p = -1 then ac else loop (ar.(p) :: ac) pred.(p) in
    loop [] m.(!maxlen))
;;

module Matching_block = struct
  module Stable = struct
    module V1 = struct
      type t =
        { prev_start : int
        ; next_start : int
        ; length : int
        }
      [@@deriving sexp, bin_io]
    end
  end

  include Stable.V1
end

module Range = struct
  module Stable = struct
    module V1 = struct
      type 'a t =
        | Same of ('a * 'a) array
        | Prev of 'a array
        | Next of 'a array
        | Replace of 'a array * 'a array
        | Unified of 'a array
      [@@deriving sexp, bin_io]
    end
  end

  include Stable.V1

  let all_same ranges =
    List.for_all ranges ~f:(fun range ->
      match range with
      | Same _ -> true
      | _ -> false)
  ;;

  let prev_only ranges =
    let f = function
      | Replace (l_range, _) -> [ Prev l_range ]
      | Next _ -> []
      | range -> [ range ]
    in
    List.concat_map ranges ~f
  ;;

  let next_only ranges =
    let f = function
      | Replace (_, r_range) -> [ Next r_range ]
      | Prev _ -> []
      | range -> [ range ]
    in
    List.concat_map ranges ~f
  ;;

  let prev_size = function
    | Unified lines | Replace (lines, _) | Prev lines -> Array.length lines
    | Same lines -> Array.length lines
    | Next _ -> 0
  ;;

  let next_size = function
    | Unified lines | Replace (_, lines) | Next lines -> Array.length lines
    | Same lines -> Array.length lines
    | Prev _ -> 0
  ;;
end

module Hunk = struct
  module Stable = struct
    module V1 = struct
      type 'a t =
        { prev_start : int
        ; prev_size : int
        ; next_start : int
        ; next_size : int
        ; ranges : 'a Range.Stable.V1.t list
        }
      [@@deriving fields, sexp, bin_io]
    end
  end

  include Stable.V1

  let _invariant t =
    Invariant.invariant [%here] t [%sexp_of: _ t] (fun () ->
      [%test_result: int]
        (List.sum (module Int) t.ranges ~f:Range.prev_size)
        ~expect:t.prev_size
        ~message:"prev_size";
      [%test_result: int]
        (List.sum (module Int) t.ranges ~f:Range.next_size)
        ~expect:t.next_size
        ~message:"next_size")
  ;;

  (* Does the nitty gritty of turning indexes into
     line numbers and reversing the ranges, returning a nice new hunk *)
  let create prev_start prev_stop next_start next_stop ranges =
    { prev_start = prev_start + 1
    ; prev_size = prev_stop - prev_start
    ; next_start = next_start + 1
    ; next_size = next_stop - next_start
    ; ranges = List.rev ranges
    }
  ;;

  let all_same hunk = Range.all_same hunk.ranges
  let concat_map t ~f = { t with ranges = List.concat_map t.ranges ~f }
end

module Hunks = struct
  module Stable = struct
    module V1 = struct
      type 'a t = 'a Hunk.Stable.V1.t list [@@deriving sexp, bin_io]
    end
  end

  include Stable.V1

  let concat_map_ranges hunks ~f = List.map hunks ~f:(Hunk.concat_map ~f)

  let unified hunks =
    let f : 'a Range.t -> 'a Range.t list = function
      | Replace (l_range, r_range) -> [ Prev l_range; Next r_range ]
      | range -> [ range ]
    in
    concat_map_ranges hunks ~f
  ;;

  let ranges hunks = List.concat_map hunks ~f:(fun hunk -> hunk.Hunk.ranges)
end

module type S = sig
  type elt

  val get_matching_blocks
    :  transform:('a -> elt)
    -> ?big_enough:int
    -> prev:'a array
    -> next:'a array
    -> Matching_block.t list

  val matches : elt array -> elt array -> (int * int) list
  val match_ratio : elt array -> elt array -> float

  val get_hunks
    :  transform:('a -> elt)
    -> context:int
    -> ?big_enough:int
    -> prev:'a array
    -> next:'a array
    -> 'a Hunk.t list

  type 'a segment =
    | Same of 'a array
    | Different of 'a array array

  type 'a merged_array = 'a segment list

  val merge : elt array array -> elt merged_array
end

(* Configurable parameters for [semantic_cleanup] and [unique_lcs], all chosen based
   on empirical observation. *)
(* This function is called on the edge case of semantic cleanup, when there's a change
   that's exactly the same length as the size of the match.  If the insert on the next
   side is a LOT larger than the match, it should be semantically cleaned up, but
   most of the time it should be left alone. *)
let should_discard_if_other_side_equal ~big_enough = 100 / big_enough

(* These are the numerator and denominator of the cutoff for aborting the patience diff
   algorithm in [unique_lcs].  (This will result in us using [Plain_diff] instead.)
   Lowering [switch_to_plain_diff_numerator] / [switch_to_plain_diff_denominator]
   makes us switch to plain diff less often.  The range of this cutoff is from 0 to 1,
   where 0 means we always switch and 1 means we never switch. *)
let switch_to_plain_diff_numerator = 1
let switch_to_plain_diff_denominator = 10

module Make (Elt : Hashtbl.Key) = struct
  module Table = Hashtbl.Make (Elt)

  type elt = Elt.t

  (* This is an implementation of the patience diff algorithm by Bram Cohen as seen in
     Bazaar version 1.14.1 *)

  module Line_metadata = struct
    type t =
      | Unique_in_a of { index_in_a : int }
      | Unique_in_a_b of
          { index_in_a : int
          ; index_in_b : int
          }
      | Not_unique of { occurrences_in_a : int }
  end

  let unique_lcs (alpha, alo, ahi) (bravo, blo, bhi) =
    (* Create a hashtable which takes elements of a to their index in a iff they're
       unique. If an element is not unique, it takes it to its frequency in a. *)
    let unique : (elt, Line_metadata.t) Table.hashtbl =
      Table.create ~size:(Int.min (ahi - alo) (bhi - blo)) ()
    in
    for x's_pos_in_a = alo to ahi - 1 do
      let x = alpha.(x's_pos_in_a) in
      match Hashtbl.find unique x with
      | None ->
        Hashtbl.set unique ~key:x ~data:(Unique_in_a { index_in_a = x's_pos_in_a })
      | Some (Unique_in_a _) ->
        Hashtbl.set unique ~key:x ~data:(Not_unique { occurrences_in_a = 2 })
      | Some (Not_unique { occurrences_in_a = n }) ->
        Hashtbl.set unique ~key:x ~data:(Not_unique { occurrences_in_a = n + 1 })
      (* This case doesn't occur until the second pass through [unique] *)
      | Some (Unique_in_a_b _) -> assert false
    done;
    (* [num_pairs] is the size of the list we use for Longest Increasing Subsequence.
       [intersection_size] is the number of tokens in the intersection of the two
       sequences, with multiplicity, and is an upper bound on the size of the LCS. *)
    let num_pairs = ref 0 in
    let intersection_size = ref 0 in
    for x's_pos_in_b = blo to bhi - 1 do
      let x = bravo.(x's_pos_in_b) in
      Hashtbl.find unique x
      |> Option.iter ~f:(fun pos ->
        match pos with
        | Not_unique { occurrences_in_a = n } ->
          if n > 0
          then (
            Hashtbl.set
              unique
              ~key:x
              ~data:(Not_unique { occurrences_in_a = n - 1 });
            incr intersection_size)
        | Unique_in_a { index_in_a = x's_pos_in_a } ->
          incr num_pairs;
          incr intersection_size;
          Hashtbl.set
            unique
            ~key:x
            ~data:
              (Unique_in_a_b
                 { index_in_a = x's_pos_in_a; index_in_b = x's_pos_in_b })
        | Unique_in_a_b _ ->
          decr num_pairs;
          Hashtbl.set unique ~key:x ~data:(Not_unique { occurrences_in_a = 0 }))
    done;
    (* If we're ignoring almost all of the text when we perform the patience
       diff algorithm, it will often give bad results. *)
    if !num_pairs * switch_to_plain_diff_denominator
       < !intersection_size * switch_to_plain_diff_numerator
    then `Not_enough_unique_tokens
    else (
      let a_b =
        let unique =
          Hashtbl.filter_map unique ~f:(function
            | Not_unique _ | Unique_in_a _ -> None
            | Unique_in_a_b { index_in_a = i_a; index_in_b = i_b } -> Some (i_a, i_b))
        in
        Ordered_sequence.create (Hashtbl.data unique)
      in
      `Computed_lcs (Patience.longest_increasing_subsequence a_b))
  ;;

  (* [matches a b] returns a list of pairs (i,j) such that a.(i) = b.(j) and such that
     the list is strictly increasing in both its first and second coordinates.

     This is done by first applying unique_lcs to find matches from a to b among those
     elements which are unique in both a and b, and then recursively applying [matches] to
     each subinterval determined by those matches.  The uniqueness requirement is waived
     for blocks of matching lines at the beginning or end.

     I couldn't figure out how to do this efficiently in a functional way, so
     this is pretty much a straight translation of the original Python code. *)
  let matches alpha bravo =
    let matches_ref_length = ref 0 in
    let matches_ref = ref [] in
    let add_match m =
      incr matches_ref_length;
      matches_ref := m :: !matches_ref
    in
    let rec recurse_matches alo blo ahi bhi =
      (*    printf "alo %d blo %d ahi %d bhi %d\n%!" alo blo ahi bhi; *)
      let old_length = !matches_ref_length in
      if not (alo >= ahi || blo >= bhi)
      then
        if Elt.compare alpha.(alo) bravo.(blo) = 0
        then (
          let alo = ref alo in
          let blo = ref blo in
          while !alo < ahi && !blo < bhi && Elt.compare alpha.(!alo) bravo.(!blo) = 0 do
            add_match (!alo, !blo);
            incr alo;
            incr blo
          done;
          recurse_matches !alo !blo ahi bhi)
        else if Elt.compare alpha.(ahi - 1) bravo.(bhi - 1) = 0
        then (
          let nahi = ref (ahi - 1) in
          let nbhi = ref (bhi - 1) in
          while
            !nahi > alo
            && !nbhi > blo
            && Elt.compare alpha.(!nahi - 1) bravo.(!nbhi - 1) = 0
          do
            decr nahi;
            decr nbhi
          done;
          recurse_matches alo blo !nahi !nbhi;
          for i = 0 to ahi - !nahi - 1 do
            add_match (!nahi + i, !nbhi + i)
          done)
        else (
          let last_a_pos = ref (alo - 1) in
          let last_b_pos = ref (blo - 1) in
          let plain_diff () =
            Plain_diff.iter_matches
              ~hashable:(module Elt)
              (Array.sub alpha ~pos:alo ~len:(ahi - alo))
              (Array.sub bravo ~pos:blo ~len:(bhi - blo))
              ~f:(fun (i1, i2) -> add_match (alo + i1, blo + i2))
          in
          match unique_lcs (alpha, alo, ahi) (bravo, blo, bhi) with
          | `Not_enough_unique_tokens -> plain_diff ()
          | `Computed_lcs lcs ->
            lcs
            |> List.iter ~f:(fun (apos, bpos) ->
              if !last_a_pos + 1 <> apos || !last_b_pos + 1 <> bpos
              then recurse_matches (!last_a_pos + 1) (!last_b_pos + 1) apos bpos;
              last_a_pos := apos;
              last_b_pos := bpos;
              add_match (apos, bpos));
            if !matches_ref_length > old_length
            (* Did unique_lcs find anything at all? *)
            then recurse_matches (!last_a_pos + 1) (!last_b_pos + 1) ahi bhi
            else plain_diff ())
    in
    recurse_matches 0 0 (Array.length alpha) (Array.length bravo);
    List.rev !matches_ref
  ;;

  let collapse_sequences matches =
    let collapsed = ref [] in
    let start_a = ref None in
    let start_b = ref None in
    let length = ref 0 in
    List.iter matches ~f:(fun (i_a, i_b) ->
      match !start_a, !start_b with
      | Some start_a_val, Some start_b_val
        when i_a = start_a_val + !length && i_b = start_b_val + !length -> incr length
      | _ ->
        (match !start_a, !start_b with
         | Some start_a_val, Some start_b_val ->
           let matching_block =
             { Matching_block.prev_start = start_a_val
             ; next_start = start_b_val
             ; length = !length
             }
           in
           collapsed := matching_block :: !collapsed
         | _ -> ());
        start_a := Some i_a;
        start_b := Some i_b;
        length := 1);
    (match !start_a, !start_b with
     | Some start_a_val, Some start_b_val when !length <> 0 ->
       let matching_block =
         { Matching_block.prev_start = start_a_val
         ; next_start = start_b_val
         ; length = !length
         }
       in
       collapsed := matching_block :: !collapsed
     | _ -> ());
    List.rev !collapsed
  ;;

  (* Given that there's an insert/delete of size [left_change] to the left, and
     an insert/delete of size [right_change] to the right, should we keep
     this block of length [block_len] in our list of matches, or discard it? *)
  let should_discard_match ~big_enough ~left_change ~right_change ~block_len =
    (* Throw away if its effective length is too small,
       relative to its surrounding inserts / deletes. *)
    block_len < big_enough
    && ((left_change > block_len && right_change > block_len)
        || (left_change >= block_len + should_discard_if_other_side_equal ~big_enough
            && right_change = block_len)
        || (right_change >= block_len + should_discard_if_other_side_equal ~big_enough
            && left_change = block_len))
  ;;

  let change_between
        (left_matching_block : Matching_block.t)
        (right_matching_block : Matching_block.t)
    =
    max
      (right_matching_block.prev_start - left_matching_block.prev_start)
      (right_matching_block.next_start - left_matching_block.next_start)
    - left_matching_block.length
  ;;

  (* See the "Semantic Chaff" section of https://neil.fraser.name/writing/diff/ *)
  let basic_semantic_cleanup ~big_enough matching_blocks =
    if big_enough <= 1
    then matching_blocks
    else (
      match matching_blocks with
      | [] -> []
      | first_block :: other_blocks ->
        let final_ans, final_pending =
          List.fold
            other_blocks
            ~init:([], first_block)
            ~f:(fun (ans, pending) current_block ->
              let rec loop ans pending =
                match ans with
                | [] -> ans, pending
                | hd :: tl ->
                  if should_discard_match
                       ~big_enough
                       ~left_change:(change_between hd pending)
                       ~right_change:(change_between pending current_block)
                       ~block_len:pending.length
                  then loop tl hd
                  else ans, pending
              in
              let updated_ans, updated_pending = loop ans pending in
              updated_pending :: updated_ans, current_block)
        in
        List.rev (final_pending :: final_ans))
  ;;

  (* Attempts to eliminate the "tunnel vision" problem described in the
     "Semantic Chaff" section of https://neil.fraser.name/writing/diff/.
     To do this, we go through each pair of consecutive matches
     and pretend to combine them into one match.  If that match would
     be deleted by [basic_semantic_cleanup], we delete both. *)
  let advanced_semantic_cleanup ~big_enough matching_blocks =
    if big_enough <= 1
    then matching_blocks
    else (
      match matching_blocks with
      | [] -> []
      | [ first_block ] -> [ first_block ]
      | first_block :: second_block :: other_blocks ->
        let final_ans, final_pendingA, final_pendingB =
          List.fold
            other_blocks
            ~init:([], first_block, second_block)
            ~f:(fun (ans, pendingA, pendingB) current_block ->
              let rec loop ans pendingA pendingB =
                match ans with
                | [] -> ans, pendingA, pendingB
                | hd :: tl ->
                  if should_discard_match
                       ~big_enough
                       ~left_change:(change_between hd pendingA)
                       ~right_change:(change_between pendingB current_block)
                       ~block_len:
                         (pendingB.length
                          + min
                              (pendingB.prev_start - pendingA.prev_start)
                              (pendingB.next_start - pendingA.next_start))
                  then loop tl hd pendingA
                  else ans, pendingA, pendingB
              in
              let updated_ans, updated_pendingA, updated_pendingB =
                loop ans pendingA pendingB
              in
              updated_pendingA :: updated_ans, updated_pendingB, current_block)
        in
        List.rev (final_pendingB :: final_pendingA :: final_ans)
        (* The loop above only deleted the second element of each pair we're supposed to
           delete.  This call to [basic_semantic_cleanup] is guaranteed to finish the job
           by deleting the remaining element of those pairs. *)
        |> basic_semantic_cleanup ~big_enough)
  ;;

  (* Goal: eliminate small, semantically meaningless matches. *)
  let semantic_cleanup ~big_enough matching_blocks =
    basic_semantic_cleanup ~big_enough matching_blocks
    |> advanced_semantic_cleanup ~big_enough
  ;;

  (* When we have a choice, we'd prefer one block of equality to two.
     For example, instead of A <insert>B A</insert> C D E F, we prefer
     <insert>A B</insert> A C D E F.  There are two reasons:

     (1) A is usually something like "let", and so the second version is more
     semantically accurate
     (2) Semantic cleanup may delete the lone A match, but it will not delete
     the A C D E F match). So by moving the A match, we've also saved it. *)
  let combine_equalities ~prev ~next ~matches =
    match matches with
    | [] -> []
    | first_block :: tl ->
      List.fold tl ~init:([], first_block) ~f:(fun (ans, pending) block ->
        let rec loop ans ~(pending : Matching_block.t) ~(new_block : Matching_block.t) =
          if pending.length = 0
          then ans, pending, new_block
          else (
            let advance_in_prev =
              Elt.compare
                prev.(pending.prev_start + pending.length - 1)
                prev.(new_block.prev_start - 1)
              = 0
            in
            let advance_in_next =
              Elt.compare
                next.(pending.next_start + pending.length - 1)
                next.(new_block.next_start - 1)
              = 0
            in
            if advance_in_prev && advance_in_next
            then
              loop
                ans
                ~pending:
                  { prev_start = pending.prev_start
                  ; next_start = pending.next_start
                  ; length = pending.length - 1
                  }
                ~new_block:
                  { prev_start = new_block.prev_start - 1
                  ; next_start = new_block.next_start - 1
                  ; length = new_block.length + 1
                  }
            else ans, pending, new_block)
        in
        let updated_ans, updated_pending, updated_new_block =
          loop ans ~pending ~new_block:block
        in
        (* In the original Google heuristic, we would either move all or none
           of pending.  But because it might start with an unmatched `Newline(0, None),
           we are fine with moving all but one token of it. *)
        if updated_pending.length = 0 || updated_pending.length = 1
        then (
          let new_ans =
            if updated_pending.length = 0
            then updated_ans
            else updated_pending :: updated_ans
          in
          new_ans, updated_new_block)
        else (* Do nothing *)
          pending :: ans, block)
      |> fun (ans, pending) -> List.rev (pending :: ans)
  ;;

  let get_matching_blocks ~transform ?(big_enough = 1) ~prev ~next =
    let prev = Array.map prev ~f:transform in
    let next = Array.map next ~f:transform in
    let matches = matches prev next |> collapse_sequences in
    let matches = combine_equalities ~prev ~next ~matches in
    let last_match =
      { Matching_block.prev_start = Array.length prev
      ; next_start = Array.length next
      ; length = 0
      }
    in
    List.append matches [ last_match ] |> semantic_cleanup ~big_enough
  ;;

  let get_ranges_rev ~transform ~big_enough ~prev ~next =
    let rec aux (matching_blocks : Matching_block.t list) i j l : _ Range.t list =
      match matching_blocks with
      | current_block :: remaining_blocks ->
        let prev_index, next_index, size =
          current_block.prev_start, current_block.next_start, current_block.length
        in
        (* Throw away crossover matches *)
        if prev_index < i || next_index < j
        then aux remaining_blocks i j l
        else (
          let range_opt : _ Range.t option =
            if i < prev_index && j < next_index
            then (
              let prev_range = prev <|> (i, prev_index) in
              let next_range = next <|> (j, next_index) in
              Some (Replace (prev_range, next_range)))
            else if i < prev_index
            then (
              let prev_range = prev <|> (i, prev_index) in
              Some (Prev prev_range))
            else if j < next_index
            then (
              let next_range = next <|> (j, next_index) in
              Some (Next next_range))
            else None
          in
          let l =
            match range_opt with
            | Some range -> range :: l
            | None -> l
          in
          let prev_stop, next_stop = prev_index + size, next_index + size in
          let l =
            if size = 0
            then l
            else (
              let prev_range = prev <|> (prev_index, prev_stop) in
              let next_range = next <|> (next_index, next_stop) in
              let range = Array.map2_exn prev_range next_range ~f:(fun x y -> x, y) in
              Same range :: l)
          in
          aux remaining_blocks prev_stop next_stop l)
      | [] -> List.rev l
    in
    let matching_blocks = get_matching_blocks ~transform ~big_enough ~prev ~next in
    aux matching_blocks 0 0 []
  ;;

  let get_hunks ~transform ~context ?(big_enough = 1) ~prev ~next =
    let ranges = get_ranges_rev ~transform ~big_enough ~prev ~next in
    let a = prev in
    let b = next in
    if context < 0
    then (
      let singleton_hunk =
        Hunk.create 0 (Array.length a) 0 (Array.length b) (List.rev ranges)
      in
      [ singleton_hunk ])
    else (
      let rec aux ranges_remaining curr_ranges alo ahi blo bhi acc_hunks =
        match (ranges_remaining : _ Range.t list) with
        | [] ->
          (* Finish the last hunk *)
          let new_hunk = Hunk.create alo ahi blo bhi curr_ranges in
          (* Add it to the accumulator *)
          let acc_hunks = new_hunk :: acc_hunks in
          (* Finished! Return the accumulator *)
          List.rev acc_hunks
        | [ Same range ] ->
          (* If the last range is a Same, we might need to crop to context. *)
          let stop = min (Array.length range) context in
          let new_range = Range.Same (range <|> (0, stop)) in
          let curr_ranges = new_range :: curr_ranges in
          (* Finish the current hunk *)
          let ahi = ahi + stop in
          let bhi = bhi + stop in
          let new_hunk = Hunk.create alo ahi blo bhi curr_ranges in
          (* Add it to the accumulator *)
          let acc_hunks = new_hunk :: acc_hunks in
          (* Finished! Return the accumulator *)
          List.rev acc_hunks
        | Same range :: rest ->
          let size = Array.length range in
          if size > context * 2
          then (
            (* If this Same range is sufficiently large, split off a new hunk *)
            let new_range = Range.Same (range <|> (0, context)) in
            let curr_ranges = new_range :: curr_ranges in
            (* Advance both hi's by context *)
            let ahi = ahi + context in
            let bhi = bhi + context in
            (* Finish the current hunk *)
            let new_hunk = Hunk.create alo ahi blo bhi curr_ranges in
            (* Add it to the accumulator *)
            let acc_hunks = new_hunk :: acc_hunks in
            (* Calculate ranges for the next hunk *)
            let alo = ahi + size - (2 * context) in
            let ahi = alo in
            let blo = bhi + size - (2 * context) in
            let bhi = blo in
            (* Push the remainder of the Equal range back onto the remaining_ranges *)
            let rest = Range.Same (range <|> (size - context, size)) :: rest in
            aux rest [] alo ahi blo bhi acc_hunks)
          else (
            (* Otherwise, this range is small enough that it qualifies as context for
               the both the previous and forthcoming range, so simply add it to
               curr_ranges untouched *)
            let curr_ranges = Range.Same range :: curr_ranges in
            let ahi = ahi + size in
            let bhi = bhi + size in
            aux rest curr_ranges alo ahi blo bhi acc_hunks)
        | range :: rest ->
          (* Any range that isn't an Equal is important and not just context, so keep
             it in curr_ranges *)
          let curr_ranges = range :: curr_ranges in
          (* rest could be anything, so extract hunk_info from range *)
          let ahi, bhi =
            match range with
            | Same _ ->
              (* We eliminate the possibility of a Same above *)
              assert false
            | Unified _ ->
              (* get_ranges_rev never returns a Unified range *)
              assert false
            | Next range ->
              let stop = bhi + Array.length range in
              ahi, stop
            | Prev range ->
              let stop = ahi + Array.length range in
              stop, bhi
            | Replace (a_range, b_range) ->
              let prev_stop = ahi + Array.length a_range in
              let next_stop = bhi + Array.length b_range in
              prev_stop, next_stop
          in
          aux rest curr_ranges alo ahi blo bhi acc_hunks
      in
      let ranges, alo, ahi, blo, bhi =
        match ranges with
        (* If the first range is an Equal, shave off the front of the range, according to
           context.  Keep it on the ranges list so hunk construction can see where the range
           begins *)
        | Same range :: rest ->
          let stop = Array.length range in
          let start = max 0 (stop - context) in
          let new_range = Range.Same (range <|> (start, stop)) in
          new_range :: rest, start, start, start, start
        | rest -> rest, 0, 0, 0, 0
      in
      aux ranges [] alo ahi blo bhi [])
  ;;

  let match_ratio a b =
    (matches a b |> List.length |> ( * ) 2 |> float)
    /. (Array.length a + Array.length b |> float)
  ;;

  let collapse_multi_sequences matches =
    let collapsed = ref [] in
    let value_exn x = Option.value_exn x in
    if List.is_empty matches
    then []
    else (
      let start = Array.create ~len:(List.length (List.hd_exn matches)) None in
      let length = ref 0 in
      List.iter matches ~f:(fun il ->
        if Array.for_all start ~f:Option.is_some
        && List.mapi il ~f:(fun i x -> x = value_exn start.(i) + !length)
           |> List.for_all ~f:(fun x -> x)
        then incr length
        else (
          if Array.for_all start ~f:Option.is_some
          then
            collapsed
            := (Array.map start ~f:value_exn |> Array.to_list, !length) :: !collapsed;
          List.iteri il ~f:(fun i x -> start.(i) <- Some x);
          length := 1));
      if Array.for_all start ~f:Option.is_some && !length <> 0
      then
        collapsed
        := (Array.map start ~f:value_exn |> Array.to_list, !length) :: !collapsed;
      List.rev !collapsed)
  ;;

  type 'a segment =
    | Same of 'a array
    | Different of 'a array array

  type 'a merged_array = 'a segment list

  let array_mapi2 ar1 ar2 ~f =
    Array.zip_exn ar1 ar2 |> Array.mapi ~f:(fun i (x, y) -> f i x y)
  ;;

  let merge ar =
    if Array.length ar = 0
    then []
    else if Array.length ar = 1
    then [ Same ar.(0) ]
    else (
      let matches's = Array.map (ar <|> (1, Array.length ar)) ~f:(matches ar.(0)) in
      let len = Array.length ar in
      let hashtbl = Int.Table.create () ~size:0 in
      Array.iteri matches's ~f:(fun i matches ->
        List.iter matches ~f:(fun (a, b) ->
          match Hashtbl.find hashtbl a with
          | None -> Hashtbl.set hashtbl ~key:a ~data:[ i, b ]
          | Some l -> Hashtbl.set hashtbl ~key:a ~data:((i, b) :: l)));
      let list =
        Hashtbl.to_alist hashtbl
        |> List.filter_map ~f:(fun (a, l) ->
          if List.length l = len - 1
          then Some (a :: (List.sort l ~compare:compare_int_pair |> List.map ~f:snd))
          else None)
        |> List.sort ~compare:(List.compare Int.compare)
      in
      let matching_blocks = collapse_multi_sequences list in
      let last_pos = Array.create ~len:(Array.length ar) 0 in
      let merged_array = ref [] in
      List.iter matching_blocks ~f:(fun (l, len) ->
        let ar' = Array.of_list l in
        if Array.compare Int.compare last_pos ar' <> 0
        then
          merged_array
          := Different (array_mapi2 last_pos ar' ~f:(fun i n m -> ar.(i) <|> (n, m)))
             :: !merged_array;
        merged_array := Same (ar.(0) <|> (ar'.(0), ar'.(0) + len)) :: !merged_array;
        Array.iteri last_pos ~f:(fun i _ -> last_pos.(i) <- ar'.(i) + len));
      List.rev !merged_array)
  ;;
end

let%test_module _ =
  (module struct
    module P = Make (Int)

    let%test_unit _ =
      let check a b ~expect = [%test_result: (int * int) list] (P.matches a b) ~expect in
      check [||] [||] ~expect:[];
      check [| 0 |] [| 0 |] ~expect:[ 0, 0 ];
      check [| 0; 1; 1; 2 |] [| 3; 1; 4; 5 |] ~expect:[ 1, 1 ]
    ;;

    (* Needs the plain diff section *)

    let rec is_increasing a = function
      | [] -> true
      | hd :: tl -> Int.compare a hd <= 0 && is_increasing hd tl
    ;;

    let check_lis a =
      let b = Patience.longest_increasing_subsequence (Ordered_sequence.create a) in
      if is_increasing (-1) (List.map b ~f:fst) && is_increasing (-1) (List.map b ~f:snd)
      then ()
      else
        failwiths
          ~here:[%here]
          "invariant failure"
          (a, b)
          [%sexp_of: (int * int) list * (int * int) list]
    ;;

    let%test_unit _ = check_lis [ 2, 0; 5, 1; 6, 2; 3, 3; 0, 4; 4, 5; 1, 6 ]
    let%test_unit _ = check_lis [ 0, 0; 2, 0; 5, 1; 6, 2; 3, 3; 0, 4; 4, 5; 1, 6 ]
    let%test_unit _ = check_lis [ 5, 1; 6, 2; 3, 3; 0, 4; 4, 5; 1, 6 ]

    let%test_unit _ =
      let check a b =
        let matches = P.matches a b in
        if is_increasing (-1) (List.map matches ~f:fst)
        && is_increasing (-1) (List.map matches ~f:snd)
        then ()
        else
          failwiths
            ~here:[%here]
            "invariant failure"
            (a, b, matches)
            [%sexp_of: int array * int array * (int * int) list]
      in
      check [| 0; 1; 2; 3; 4; 5; 6 |] [| 2; 5; 6; 3; 0; 4; 1 |]
    ;;
  end)
;;

module String = Make (String)

module Stable = struct
  module Matching_block = Matching_block.Stable
  module Range = Range.Stable
  module Hunk = Hunk.Stable
  module Hunks = Hunks.Stable
end
