open Core.Std
open Async.Std

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
      match List.map (String.split line ~on:'"') ~f:String.strip with
      | ["(* part"; name; "*)"] | ["[@@@part"; name; "]"] -> Some name
      | _ -> None
    in
    let rec split_parts parts name lines = function
      | [] -> add_part parts name lines
      | line :: rest ->
        match is_part line with
        | None ->
          split_parts parts name (line :: lines) rest
        | Some name ->
          let parts = add_part parts name lines in
          split_parts parts name [] rest
    in
    split_parts [] "" [] lines

end

let program_path =
  Filename.(concat (dirname Sys.executable_name) "rwo-expect")

let () =
  if not (Caml.Sys.file_exists program_path) then
    failwithf "Rwo_expect: cannot find %S program" program_path ()

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
    >>|? Sexp.of_string
    >>|? t_of_sexp
end

