open Compat

type t = Normal | Cram

let cram_default_header = Some "sh" (* FIXME: bash? *)

let pp fs = function
  | Normal -> Fmt.string fs "normal"
  | Cram -> Fmt.string fs "cram"

let equal x y = x = y

let infer ~file =
  match Filename.extension file with
  | ".t" -> Some Cram
  | ".md" -> Some Normal
  | _ -> None

let of_string = function
  | "markdown" | "normal" -> Some Normal
  | "cram" -> Some Cram
  | _ -> None
