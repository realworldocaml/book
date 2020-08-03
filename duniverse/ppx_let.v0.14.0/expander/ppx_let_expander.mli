open Ppxlib

module Extension_name : sig
  type t =
    | Sub
    | Sub_open
    | Bind
    | Bind_open
    | Bindn
    | Bindn_open
    | Map
    | Map_open
    | Mapn
    | Mapn_open

  val to_string : t -> string
end

val expand : modul:longident loc option -> Extension_name.t -> expression -> expression
