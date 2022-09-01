open! Core
open! Import

type t =
  { ascii : char
  ; utf8 : string
  }
[@@deriving fields, sexp_of]

let connect ?top ?bottom ?left ?right () =
  let top, bottom, left, right =
    is_some top, is_some bottom, is_some left, is_some right
  in
  let ascii, utf8 =
    match top, bottom, left, right with
    | false, false, true, true -> '-', "\226\148\128"
    | true, true, false, false -> '|', "\226\148\130"
    | false, true, false, true -> '|', "\226\148\140"
    | false, true, true, false -> '|', "\226\148\144"
    | true, false, false, true -> '|', "\226\148\148"
    | true, false, true, false -> '|', "\226\148\152"
    | true, true, false, true -> '|', "\226\148\156"
    | true, true, true, false -> '|', "\226\148\164"
    | false, true, true, true -> '-', "\226\148\172"
    | true, false, true, true -> '-', "\226\148\180"
    | true, true, true, true -> '+', "\226\148\188"
    | false, false, true, false -> '-', "\226\149\180"
    | true, false, false, false -> '|', "\226\149\181"
    | false, false, false, true -> '-', "\226\149\182"
    | false, true, false, false -> '|', "\226\149\183"
    | false, false, false, false -> ' ', " "
  in
  { ascii; utf8 }
;;

let to_buffer { ascii; utf8 } buf = function
  | `Ascii -> Buffer.add_char buf ascii
  | `Unicode -> Buffer.add_string buf utf8
;;
