open Sexplib.Conv

module Chunk = struct
  type kind = OCaml | Raw
    [@@deriving sexp]

  type response = (kind * string)
    [@@deriving sexp]

  type t =
    { ocaml_code : string;
      toplevel_responses : response list; }
    [@@deriving sexp]

  let v ~ocaml_code ~toplevel_responses = {ocaml_code; toplevel_responses}
  let code c = c.ocaml_code
  let warnings (_ : t) : string =  ""
  let responses c = c.toplevel_responses
  let stdout (_ : t) = ""
  let evaluated (_ : t) = true
end

module Part = struct
  type t =
    { name : string;
      chunks : Chunk.t list; }
    [@@deriving sexp]

  let v ~name ~chunks = { name; chunks }
  let name {name;_} = name
  let chunks {chunks;_} = chunks
end

module Document = struct
  type t =
    { parts : Part.t list; matched : bool; }
    [@@deriving sexp]

  let v ~parts ~matched = {parts; matched}
  let parts {parts;_} = parts
  let matched {matched;_} = matched
end
