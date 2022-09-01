open! Core
open! Import

module type S = sig
  (* diff strings and output to strings, supposed to be used by ocaml code *)

  val diff_strings
    :  ?print_global_header:bool
    -> Configuration.t
    -> prev:Diff_input.t
    -> next:Diff_input.t
    -> [ `Different of string | `Same ]

  module Private : sig
    val compare_lines
      :  Configuration.t
      -> prev:string array
      -> next:string array
      -> string Patience_diff.Hunks.t
  end
end

module type Compare_core = sig
  module type S = S

  module Make (Patdiff_core : Patdiff_core.S) : S
  module Without_unix : S
end
