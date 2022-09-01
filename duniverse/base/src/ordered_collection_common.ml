open! Import
include Ordered_collection_common0

let get_pos_len ?pos ?len () ~total_length =
  try Result.Ok (get_pos_len_exn () ?pos ?len ~total_length) with
  | Invalid_argument s -> Or_error.error_string s
;;
