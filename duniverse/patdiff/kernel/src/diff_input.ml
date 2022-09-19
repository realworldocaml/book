open! Core
open! Import

type t =
  { name : string
  ; text : string
  }
[@@deriving fields]
