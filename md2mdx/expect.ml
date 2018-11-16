open Astring

module Raw_script = struct

  type part = { filename: string; name : string; content : string; }

  let dump_part ppf t =
    Fmt.pf ppf "{@[name: %S;@ content: %S@]}" t.name t.content

  let pp_part ppf t =
    Fmt.pf ppf "[@@@@@@part %S];;\n%s" t.name t.content

  type t = part list

  let of_file ~filename =
    let lines = File.read_lines filename in
    let add_part parts name lines =
      { filename ; name ; content = String.concat ~sep:"\n" (List.rev lines) }
      :: parts
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

module Part = Ocaml_topexpect.Part

module Chunk = struct
  module C = Ocaml_topexpect.Chunk

  type t = { filename: string; v: C.t list }

  let filename t = t.filename
  let v t = t.v

  let of_part ~filename p = { filename; v = Part.chunks p }

  let dump ppf t =
    Fmt.pf ppf "@[<hov>%s:@ %a@]" t.filename
      Fmt.(list ~sep:cut Ppx_sexp_conv_lib.Sexp.pp_hum)
      (List.map C.sexp_of_t t.v)

  let remove_semi_semi c = match List.rev c with
    | h::t ->
      let len = String.length h in
      if len >= 2 && h.[len-1] = ';' && h.[len-2] = ';' then
        match String.trim (String.with_range h ~len:(String.length h - 2)) with
        | "" -> List.rev t
        | h  -> List.rev (h::t)
      else
        c
    | _ -> c

  let trim_list l =
    let aux = function
      | "" :: t -> t
      | t -> t
    in
    aux (List.rev (aux (List.rev l)))

  let pp_code ppf code =
    List.iteri (fun i s ->
        if i=0 then Fmt.pf ppf "# %s\n" s
        else if s = "" then Fmt.string ppf "\n"
        else Fmt.pf ppf "  %s\n" s
      ) code

  let pp_chunk ppf t =
    let code = C.code t in
    let code = String.cuts ~sep:"\n" code in
    let code = remove_semi_semi code in
    pp_code ppf code;
    let responses = C.responses t in
    List.iter (fun (k, s) ->
        let s = String.cuts ~sep:"\n" s in
        let s = trim_list s in
        let s = match s with "" :: t -> t | t -> t in
        let pp_lines ppf l =
          Fmt.pf ppf "%a\n" Fmt.(list ~sep:(unit "\n") string) l
        in
        match k with
        | C.OCaml -> pp_lines ppf s
        | C.Raw   -> pp_lines ppf s
      ) responses

  let pp ppf t = Fmt.(list ~sep:(unit "")) pp_chunk ppf t.v
end

module Mlt = struct

  include Ocaml_topexpect.Document

  let of_file ~filename =
    let contents = File.read filename in
    let lexbuf = Ocaml_topexpect.Lexbuf.v ~fname:filename contents in
    let phrases = Ocaml_topexpect.Phrase.read_all lexbuf in
    Ocaml_topexpect.Phrase.document lexbuf ~matched:true phrases

end

module Cram = struct

  type t = {
    filename: string;
    v: Cram.t;
  }

  let v t = t.v
  let to_html x = Cram.to_html x.v
  let is_empty t = t.v = []
  let dump ppf t = Cram.pp ppf t.v
  let pp ppf t = Cram.pp ppf t.v
  let filename t = t.filename

  let part s x = match Cram.part s x.v with
    | None   -> None
    | Some v -> Some { x with v }

  let of_file ~filename =
    let lines = File.read filename in
    let lexbuf = Lexing.from_string lines in
    lexbuf.lex_curr_p <-
      { pos_fname = filename
      ; pos_cnum  = 0
      ; pos_lnum  = 1
      ; pos_bol   = 0
      };
    { filename; v = Cram.parse_lexbuf lexbuf }

end
