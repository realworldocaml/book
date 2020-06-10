open Compat

type t = Normal | Cram | Mli

let pp fs = function
  | Normal -> Fmt.string fs "normal"
  | Cram -> Fmt.string fs "cram"
  | Mli -> Fmt.string fs "mli"

let equal x y = x = y

let infer ~file =
  match Filename.extension file with
  | ".t" -> Some Cram
  | ".md" -> Some Normal
  | ".mli" -> Some Mli
  | _ -> None

let of_string = function
  | "markdown" | "normal" -> Some Normal
  | "cram" -> Some Cram
  | "mli" -> Some Mli
  | _ -> None
