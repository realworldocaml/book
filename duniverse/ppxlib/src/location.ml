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

let init lexbuf fname =
  let open Lexing in
  lexbuf.lex_curr_p <- {
    pos_fname = fname;
    pos_lnum = 1;
    pos_bol = 0;
    pos_cnum = 0;
  }

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

let compare_pos p1 p2 =
  let open Lexing in
  let column p =
    (* Manual extract:
       The difference between pos_cnum and pos_bol is the character offset
       within the line (i.e. the column number, assuming each character is
       one column wide). *)
    p.pos_cnum - p.pos_bol
  in
  match Int.compare p1.pos_lnum p2.pos_lnum with
  | 0 -> Int.compare (column p1) (column p2)
  | n -> n

let min_pos p1 p2 =
  if compare_pos p1 p2 <= 0 then p1 else p2

let max_pos p1 p2 =
  if compare_pos p1 p2 >= 0 then p1 else p2

let compare loc1 loc2 =
  match compare_pos loc1.loc_start loc2.loc_start with
  | 0 -> compare_pos loc1.loc_end loc2.loc_end
  | n -> n

module Error = struct
  include Ppxlib_ast.Location_error

  let createf ~loc fmt =
    Printf.ksprintf
      (fun str -> make ~loc ~sub:[] str) fmt
end

exception Error of Error.t

let () =
  Caml.Printexc.register_printer (function
    | Error e -> Some (Error.message e)
    | _ -> None)
