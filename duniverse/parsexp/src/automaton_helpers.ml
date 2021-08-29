open! Import

let rec feed_substring_unsafe str state stack i stop =
  if i < stop
  then (
    let c = String.unsafe_get str i in
    let stack = Parser_automaton.feed state c stack in
    feed_substring_unsafe str state stack (i + 1) stop)
  else stack
;;

let rec feed_subbytes_unsafe str state stack i stop =
  if i < stop
  then (
    let c = Bytes.unsafe_get str i in
    let stack = Parser_automaton.feed state c stack in
    feed_subbytes_unsafe str state stack (i + 1) stop)
  else stack
;;

let feed_substring state str ~pos ~len stack =
  let str_len = String.length str in
  if pos < 0 || len < 0 || pos > str_len - len then invalid_arg "Parsexp.feed_substring";
  feed_substring_unsafe str state stack pos (pos + len)
;;

let feed_subbytes state str ~pos ~len stack =
  let str_len = Bytes.length str in
  if pos < 0 || len < 0 || pos > str_len - len then invalid_arg "Parsexp.feed_subbytes";
  feed_subbytes_unsafe str state stack pos (pos + len)
;;

let feed_string state str stack =
  feed_substring_unsafe str state stack 0 (String.length str)
;;

let feed_bytes state str stack =
  feed_subbytes_unsafe str state stack 0 (Bytes.length str)
;;
