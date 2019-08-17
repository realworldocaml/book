open Import

module L = Ocaml_common.Location

type t = location =
  { loc_start : Lexing.position
  ; loc_end   : Lexing.position
  ; loc_ghost : bool
  }

let in_file name =
  let loc =
    { pos_fname = name
    ; pos_lnum  = 1
    ; pos_bol   = 0
    ; pos_cnum  = -1
    }
  in
  { loc_start = loc
  ; loc_end   = loc
  ; loc_ghost = true
  }

let none = in_file "_none_"

let raise_errorf ?loc fmt = L.raise_errorf ?loc fmt
let report_exception = L.report_exception

let of_lexbuf (lexbuf : Lexing.lexbuf) =
  { loc_start = lexbuf.lex_start_p
  ; loc_end   = lexbuf.lex_curr_p
  ; loc_ghost = false
  }

let print ppf t =
  Caml.Format.fprintf ppf "File \"%s\", line %d, characters %d-%d:"
    t.loc_start.pos_fname
    t.loc_start.pos_lnum
    (t.loc_start.pos_cnum - t.loc_start.pos_bol)
    (t.loc_end.pos_cnum   - t.loc_start.pos_bol)

type nonrec 'a loc = 'a loc =
  { txt : 'a
  ; loc : t
  }

module Error = struct
  module Helpers = Selected_ast.Ast.Ast_mapper

  type t = Helpers.location_error

  let make = Helpers.make_error_of_message
  let createf ~loc fmt =
    Printf.ksprintf
      (fun str -> Helpers.make_error_of_message ~loc ~sub:[] str) fmt

  let message = Helpers.get_error_message
  let set_message = Helpers.set_error_message

  let register_error_of_exn = Helpers.register_error_of_exn

  let of_exn = Helpers.error_of_exn

  let to_extension = Helpers.extension_of_error
end

exception Error of Error.t

let () =
  Caml.Printexc.register_printer (function
    | Error e -> Some (Error.message e)
    | _ -> None)
