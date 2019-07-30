(** Exception sexp converters that are Unix-specific. Handles [Unix.Unix_error].

    Write [let () = Sexplib_unix.Sexplib_unix_conv.linkme] in your program to ensure that
    the code in this module is run, i.e. the unix-specific exception converters are added.
    This is already done by [Core], so any application that uses Core need not worry
    about this module at all. *)
open Sexplib.Sexp
open Sexplib.Conv

let () =
  Exn_converter.add ~finalise:false [%extension_constructor Unix.Unix_error]
    (function
      | Unix.Unix_error (err, loc, arg) ->
        let err_str = Unix.error_message err in
        List [Atom "Unix.Unix_error"; Atom err_str; Atom loc; Atom arg]
      | _ -> assert false)

let linkme = ()
