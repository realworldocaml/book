(* Adapted from Anil Madhavapeddy's ocaml-uri package. *)

let printers = [ "Ctypes_printers.format_typ";
                 "Ctypes_printers.format_fn"; 
                 "Ctypes_printers.format_sint";
                 "Ctypes_printers.format_long";
                 "Ctypes_printers.format_llong";
                 "Ctypes_printers.format_uchar";
                 "Ctypes_printers.format_uint8";
                 "Ctypes_printers.format_uint16";
                 "Ctypes_printers.format_uint32";
                 "Ctypes_printers.format_uint64";
                 "Ctypes_printers.format_size_t";
                 "Ctypes_printers.format_ushort";
                 "Ctypes_printers.format_uint";
                 "Ctypes_printers.format_ulong";
                 "Ctypes_printers.format_ullong";
                 "Ctypes_printers.format_pointer";
                 "Ctypes_printers.format_struct";
                 "Ctypes_printers.format_union";
                 "Ctypes_printers.format_array";
                 "Ctypes_printers.format_ocaml";
                 "Ctypes_printers.format_clock_t";
                 "Ctypes_printers.format_dev_t";
                 "Ctypes_printers.format_ino_t";
                 "Ctypes_printers.format_mode_t";
                 "Ctypes_printers.format_nlink_t";
                 "Ctypes_printers.format_off_t";
                 "Ctypes_printers.format_pid_t";
                 "Ctypes_printers.format_size_t";
                 "Ctypes_printers.format_ssize_t";
                 "Ctypes_printers.format_time_t";
                 "Ctypes_printers.format_useconds_t";
                 "Ctypes_printers.format_ldouble";
                 "Ctypes_printers.format_complexld";]

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
    Format.eprintf "Problem installing ctypes-printers@."
