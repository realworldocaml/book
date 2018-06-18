open Astring

module Raw_script = struct

  type part = { name : string; content : string; }

  let dump_part ppf t =
    Fmt.pf ppf "{@[name: %S;@ content: %S@]}" t.name t.content

  let pp_part ppf t =
    Fmt.pf ppf "[@@@@@@part %S];;\n%s" t.name t.content

  type t = part list

  let of_file ~filename =
    let lines = File.read_lines filename in
    let add_part parts name lines =
      { name ; content = String.concat ~sep:"\n" (List.rev lines) } :: parts
    in
    let is_part line =
      match List.map String.trim (String.cuts line ~sep:"\"") with
      | ["(* part"; name; "*)"]
      | ["[@@@part"; name; "]"]
      | ["[@@@part"; name; "];;"]
        -> Some name
      | _ -> None
    in
    let rec split_parts parts name lines = function
      | [] -> add_part parts name lines
      | line :: rest ->
        match is_part line with
        | None ->
          split_parts parts name (line :: lines) rest
        | Some name' ->
          let parts = add_part parts name lines in
          split_parts parts name' [] rest
    in
    split_parts [] "" [] lines

end

module Chunk = struct
  include Ocaml_topexpect.Chunk
  let dump ppf t = Ppx_sexp_conv_lib.Sexp.pp_hum ppf (sexp_of_t t)
  let pp ppf t =
    let code = code t in
    Fmt.pf ppf "%s\n" code;
    let responses = responses t in
    List.iter (fun (k, s) -> match k with
        | OCaml -> Fmt.pf ppf "[%%%%expect ocaml {|%s|}];;" s
        | Raw   -> Fmt.string ppf s
      ) responses
end

module Part = Ocaml_topexpect.Part

module Mlt = struct

  include Ocaml_topexpect.Document

  let of_file ~filename =
    let contents = File.read filename in
    let lexbuf = Ocaml_topexpect.Lexbuf.v ~fname:filename contents in
    let phrases = Ocaml_topexpect.Phrase.read_all lexbuf in
    Ocaml_topexpect.Phrase.document lexbuf ~matched:true phrases

end

module Cram = struct

  type t = Cram.t
  let to_html x = Cram.to_html x
  let part = Cram.part
  let is_empty t = t = []
  let dump ppf t = Cram.pp ppf t
  let pp ppf t = Cram.pp ppf t

  let of_file ~filename =
    let lines = File.read filename in
    let lexbuf = Lexing.from_string lines in
    lexbuf.lex_curr_p <-
      { pos_fname = filename
      ; pos_cnum  = 0
      ; pos_lnum  = 1
      ; pos_bol   = 0
      };
    Cram.parse_lexbuf lexbuf

end
