open! Core
open! Import

type t =
  | Binary_same
  | Binary_different of
      { prev_is_binary : bool
      ; next_is_binary : bool
      }
  | Hunks of Patdiff_hunks.t

val create
  :  Configuration.t
  -> prev:Patdiff_core.diff_input
  -> next:Patdiff_core.diff_input
  (** This configuration may differ from what was passed into [create], depending on
      heuristics that consider [prev] and [next]. *)
  -> compare_assuming_text:(Configuration.t
                            -> prev:Patdiff_core.diff_input
                            -> next:Patdiff_core.diff_input
                            -> Patdiff_hunks.t)
  -> t

val has_no_diff : t -> bool
