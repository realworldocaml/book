open Core
open Async

type t = {
  filename : string;
  commands : string list;
} [@@deriving sexp]

let of_file filename =
  Reader.file_lines filename >>|
  List.filter ~f:(fun x -> not (String.for_all x ~f:Char.is_whitespace))
  >>| fun commands ->
  {filename; commands}

module Evaluated = struct

  type command = {
    command : string;
    output : string;
    exit_code : int;
  } [@@deriving sexp]

  type t = {
    filename : string;
    commands : command list;
  } [@@deriving sexp]

  let to_string t =
    List.map t.commands ~f:(fun x -> ["$ " ^ x.command; "\n"; x.output]) |>
    List.concat |>
    String.concat ~sep:""

  let check_all_zero t =
    List.map t.commands ~f:(fun x ->
        if x.exit_code = 0 then
          Ok ()
        else
          error "expected exit code = 0"
            (t.filename, x.command, x.exit_code)
            [%sexp_of: string * string * int]
      )
    |> Or_error.combine_errors_unit
    |> function
    | Error _ as e -> e
    | Ok () -> Ok t

end

let eval t =
  let working_dir = Filename.dirname t.filename in
  let basename = Filename.basename t.filename in
  Sys.getcwd() >>= fun curr_dir ->
  let final () = Sys.chdir curr_dir in
  Sys.chdir working_dir >>= fun () ->
  try_with
    (
      fun () ->
        Deferred.List.map t.commands ~f:(fun command ->
            let temp_file = Filename.temp_file ~in_dir:"." basename ".out" in
            let cmd = sprintf "%s >%s 2>&1" command temp_file in
            Sys.command cmd >>= fun exit_code ->
            Reader.file_contents temp_file >>= fun output ->
            Unix.unlink temp_file >>= fun () ->
            return {Evaluated.command; output; exit_code}
          )
    ) >>= function
  | Error exn -> (final() >>| fun () -> Or_error.of_exn exn)
  | Ok commands -> (
      final() >>| fun () ->
      Ok {Evaluated.filename=t.filename; commands}
    )

let eval_file filename =
  of_file filename >>= eval
