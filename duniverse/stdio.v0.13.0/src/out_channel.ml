open! Base
open! Import

type t = Caml.out_channel

let equal (t1 : t) t2 = phys_equal t1 t2

let seek   = Caml.LargeFile.seek_out
let pos    = Caml.LargeFile.pos_out
let length = Caml.LargeFile.out_channel_length

let stdout = Caml.stdout
let stderr = Caml.stderr

let sexp_of_t t =
  if phys_equal t stderr
  then Sexp.Atom "<stderr>"
  else if phys_equal t stdout
  then Sexp.Atom "<stdout>"
  else Sexp.Atom "<Out_channel.t>"
;;

type 'a with_create_args =
  ?binary:bool
  -> ?append:bool
  -> ?fail_if_exists:bool
  -> ?perm:int
  -> 'a

let create ?(binary = true) ?(append = false) ?(fail_if_exists = false) ?(perm = 0o666) file =
  let flags = [Open_wronly; Open_creat] in
  let flags = (if binary then Open_binary else Open_text) :: flags in
  let flags = (if append then Open_append else Open_trunc) :: flags in
  let flags = (if fail_if_exists then Open_excl :: flags else flags) in
  Caml.open_out_gen flags perm file
;;

let set_binary_mode = Caml.set_binary_mode_out

let flush = Caml.flush

let close = Caml.close_out
let close_no_err = Caml.close_out_noerr

let output t ~buf ~pos ~len = Caml.output t buf pos len
let output_substring t ~buf ~pos ~len = Caml.output_substring t buf pos len
let output_string = Caml.output_string
let output_bytes = Caml.output_bytes
let output_char = Caml.output_char
let output_byte = Caml.output_byte
let output_binary_int = Caml.output_binary_int
let output_buffer = Caml.Buffer.output_buffer
let output_value = Caml.output_value

let newline t = output_string t "\n"

let output_lines t lines =
  List.iter lines ~f:(fun line -> output_string t line; newline t)
;;

let printf   = Caml.Printf.printf
let eprintf  = Caml.Printf.eprintf
let fprintf  = Caml.Printf.fprintf
let kfprintf = Caml.Printf.kfprintf

let print_string = Caml.print_string
let print_endline = Caml.print_endline
let prerr_endline = Caml.prerr_endline

let print_s ?mach sexp =
  print_endline
    (match mach with
     | Some () -> Sexp.to_string_mach sexp
     | None    -> Sexp.to_string_hum  sexp)
;;

let eprint_s ?mach sexp =
  prerr_endline
    (match mach with
     | Some () -> Sexp.to_string_mach sexp
     | None    -> Sexp.to_string_hum  sexp)
;;


let with_file ?binary ?append ?fail_if_exists ?perm file ~f =
  Exn.protectx (create ?binary ?append ?fail_if_exists ?perm file) ~f ~finally:close
;;

let write_lines file lines = with_file file ~f:(fun t -> output_lines t lines)

let write_all file ~data = with_file file ~f:(fun t -> output_string t data)
