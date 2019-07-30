(** An internal-only module factored out due to a circular dependency between core_array
    and core_list.  Contains code for permuting an array. *)

open! Import

include Array0

(** randomly permute an array. *)
let permute ?(random_state = Random.State.default) t =
  for i = length t downto 2 do
    swap t (i - 1) (Random.State.int random_state i)
  done
