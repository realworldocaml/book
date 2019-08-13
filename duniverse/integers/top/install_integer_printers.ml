(* Adapted from Anil Madhavapeddy's ocaml-uri package. *)

let _ = Integer_printers.format_sint

let printers = [ "Integer_printers.format_sint";
                 "Integer_printers.format_long";
                 "Integer_printers.format_llong";
                 "Integer_printers.format_uchar";
                 "Integer_printers.format_uint8";
                 "Integer_printers.format_uint16";
                 "Integer_printers.format_uint32";
                 "Integer_printers.format_uint64";
                 "Integer_printers.format_ushort";
                 "Integer_printers.format_uint";
                 "Integer_printers.format_ulong";
                 "Integer_printers.format_ullong";]

let eval_string
      ?(print_outcome = false) ?(err_formatter = Format.err_formatter) str =
  let lexbuf = Lexing.from_string str in
  let phrase = !Toploop.parse_toplevel_phrase lexbuf in
  Toploop.execute_phrase print_outcome err_formatter phrase

let rec install_printers = function
  | [] -> true
  | printer :: printers ->
      let cmd = Printf.sprintf "#install_printer %s;;" printer in
      eval_string cmd && install_printers printers

let () =
  if not (install_printers printers) then
    Format.eprintf "Problem installing integer-printers@."
