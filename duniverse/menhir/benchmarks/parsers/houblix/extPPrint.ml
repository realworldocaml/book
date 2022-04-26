(** This module extends the PPrint library. *)

open PPrint

let ribbon = 0.7

let to_string ?(width = 120) f x =
  ignore width;
  let b = Buffer.create 13 in
  ToBuffer.pretty ribbon 120 b (f x);
  Buffer.contents b

let to_stdout ?(width = 80) doc =
  ignore width;
  PPrint.ToChannel.pretty ribbon 80 stdout doc;
  print_newline ()

let ( ++ ) x y = x ^^ break 1 ^^ y

let assoc_list ?(bind = "=") ?(sep = ",") pp_key pp_val assl =
  let b = string (" " ^ bind) in
  separate_map (string sep) (fun (k, v) -> pp_key k ^^ b ^/^ pp_val v) assl

let record = OCaml.record ""
