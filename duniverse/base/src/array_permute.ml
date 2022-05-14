(** An internal-only module factored out due to a circular dependency between core_array
    and core_list.  Contains code for permuting an array. *)

open! Import
include Array0

let permute ?(random_state = Random.State.default) ?(pos = 0) ?len t =
  (* Copied from [Ordered_collection_common0] to avoid allocating a tuple when compiling
     without flambda. *)
  let total_length = length t in
  let len =
    match len with
    | Some l -> l
    | None -> total_length - pos
  in
  Ordered_collection_common0.check_pos_len_exn ~pos ~len ~total_length;
  let num_swaps = len - 1 in
  for i = num_swaps downto 1 do
    let this_i = pos + i in
    (* [random_i] is drawn from [pos,this_i] *)
    let random_i = pos + Random.State.int random_state (i + 1) in
    swap t this_i random_i
  done
;;
