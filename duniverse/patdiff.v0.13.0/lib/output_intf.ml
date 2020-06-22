open! Core
open! Import

module type S = sig
  (** An output can apply a style to a string and print a list of hunks *)

  module Rule : sig
    val apply : string -> rule:Patdiff_format.Rule.t -> refined:bool -> string
  end

  val print
    :  print_global_header:bool
    -> file_names:string * string
    -> rules:Patdiff_format.Rules.t
    -> print:(string -> unit)
    -> location_style:Patdiff_format.Location_style.t
    -> Patdiff_hunks.t
    -> unit
end
