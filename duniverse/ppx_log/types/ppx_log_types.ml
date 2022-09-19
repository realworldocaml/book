open Base
open Sexplib

type level =
  [ `Debug
  | `Info
  | `Error
  ]

module type S = sig
  type t
  type return_type

  (* [would_log] and [Global.would_log] take an option because of
     [Async.Log.would_log] takes an option and we want to pass in a (Some
     `Debug) statically so that it won't allocate. If we made a wrapper
     function that always just wrapped the level in `Some, it might allocate.
  *)
  val would_log : t -> level option -> bool
  val default : return_type
  val sexp : ?level:level -> ?pos:Source_code_position.t -> t -> Sexp.t -> return_type

  module Global : sig
    type return_type

    val would_log : level option -> bool
    val default : return_type
    val sexp : ?level:level -> ?pos:Source_code_position.t -> Sexp.t -> return_type
  end
end
