open! Import


(* This is lifted out of [M] because [Source_code_position0] exports [String0]
   as [String], which does not export a hash function. *)
let hash_override { Caml.Lexing. pos_fname; pos_lnum; pos_bol; pos_cnum } =
  String.hash pos_fname
  lxor Int.hash pos_lnum
  lxor Int.hash pos_bol
  lxor Int.hash pos_cnum
;;

module M = struct
  include Source_code_position0

  let hash = hash_override
end

include M
include Comparable.Make_using_comparator(M)
