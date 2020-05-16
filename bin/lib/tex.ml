open Core

type command = {
  name : string;
  info : string option;
}

and item = [
| `Command of command
| `Data of string
] [@@deriving sexp]

type t = item list [@@deriving sexp]

let to_string t =
  let loop item = match item with
    | `Command {name; info = None} -> "\\" ^ name
    | `Command {name; info = Some info} -> "\\" ^ name ^ "{" ^ info ^ "}"
    | `Data s -> s
  in
  List.map ~f:loop t
  |> String.concat ~sep:"\n"

(******************************************************************************)
(* Constructors                                                               *)
(******************************************************************************)
let cmd ?(info=None) command = `Command{name=command; info}

let input file = cmd "input" ~info:(Some file)
let newpage = cmd "newpage"
let part s = cmd "part" ~info:(Some s)
