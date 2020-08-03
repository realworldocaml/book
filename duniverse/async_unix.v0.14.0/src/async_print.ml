open Core

let stdout () = Lazy.force Writer.stdout
let stderr () = Lazy.force Writer.stderr
let do_printf writer = ksprintf (Writer.write (writer ()))
let printf fmt = do_printf stdout fmt
let fprintf writer fmt = Printf.ksprintf (fun s -> Writer.write writer s) fmt
let eprintf fmt = do_printf stderr fmt
let print_char c = Writer.write_char (stdout ()) c
let prerr_char c = Writer.write_char (stderr ()) c
let print_string s = Writer.write (stdout ()) s
let prerr_string s = Writer.write (stderr ()) s
let print_newline () = Writer.write_char (stdout ()) '\n'
let prerr_newline () = Writer.write_char (stderr ()) '\n'

let print_endline s =
  print_string s;
  print_newline ()
;;

let prerr_endline s =
  prerr_string s;
  prerr_newline ()
;;

let print_int i = print_string (Int.to_string i)
let prerr_int i = prerr_string (Int.to_string i)
let print_float f = print_string (Float.to_string_12 f)
let prerr_float f = prerr_string (Float.to_string_12 f)

let print_s ?mach sexp =
  print_endline
    (match mach with
     | Some () -> Sexp.to_string_mach sexp
     | None -> Sexp.to_string_hum sexp)
;;

let eprint_s ?mach sexp =
  prerr_endline
    (match mach with
     | Some () -> Sexp.to_string_mach sexp
     | None -> Sexp.to_string_hum sexp)
;;
