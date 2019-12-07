open Ppxlib

module Extension_name : sig
  type t =
    | Bind
    | Bind_open
    | Map
    | Map_open

  val to_string : t -> string
end

val expand : modul:longident loc option -> Extension_name.t -> expression -> expression
