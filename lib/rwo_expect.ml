open Core
open Async

module Raw_script = struct
  type part =
    { name : string; content : string; }
    [@@deriving sexp]

  type t = part list
    [@@deriving sexp]

  let of_file ~filename =
    Monitor.try_with_or_error (fun () -> Reader.file_lines filename)
    >>|? fun lines ->
    let add_part parts name lines =
      { name ; content = String.concat ~sep:"\n" (List.rev lines) } :: parts
    in
    let is_part line =
      match List.map ~f:String.strip (String.split line ~on:'"') with
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

let program_path = "ocaml-expect"

module Chunk = struct
  type t = Toplevel_expect_test_types.Chunk.t =
    { ocaml_code : string; toplevel_response : string; }
    [@@deriving sexp]

  let code c = c.ocaml_code
  let warnings (_ : t) : string =  ""
  let response c = c.toplevel_response
  let stdout (_ : t) = ""
  let evaluated (_ : t) = true
end

module Part = struct
  type t = Toplevel_expect_test_types.Part.t =
    { name : string; chunks : Chunk.t list; }
    [@@deriving sexp]
end

module Document = struct
  type t = Toplevel_expect_test_types.Document.t =
    { parts : Part.t list; matched : bool; }
    [@@deriving sexp]

  let parts t = t.parts

  let of_file ~filename =
    Process.run ~prog:program_path ~args:["-sexp"; filename] ()
    >>|? fun str -> t_of_sexp (Sexp.of_string (String.strip str))

  let output_corrected {parts; matched = _} chan =
    let rec iter_last ~f = function
      | x :: xs ->
        f ~last:(List.is_empty xs) x;
        iter_last ~f xs
      | [] -> ()
    in
    let output_string = Out_channel.output_string chan in
    let output_chunk ~last {Chunk. ocaml_code; toplevel_response} =
      output_string ocaml_code;
      if toplevel_response = "" || toplevel_response = "\n" then (
        if not last then
          output_string "[%%expect]\n";
      ) else (
        output_string "[%%expect{|";
        output_string toplevel_response;
        output_string "|}]\n";
      );
    in
    let output_part ~last {Part. name; chunks} =
      if name <> "" then
        output_string (Printf.sprintf "[@@@part %S];;\n" name);
      if last then
        iter_last ~f:output_chunk chunks
      else
        List.iter ~f:(output_chunk ~last:false) chunks
    in
    iter_last ~f:output_part parts

end

