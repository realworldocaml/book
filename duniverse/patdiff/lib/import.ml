open! Core
include Composition_infix
module Patience_diff = Patience_diff_lib.Std.Patience_diff

let error_message_start ~file ~line =
  String.concat
    [ "File \""; file; "\", line "; line |> Int.to_string; ", characters 0-1:" ]
;;
