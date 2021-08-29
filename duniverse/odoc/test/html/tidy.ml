
let muted_warnings = [
  (* NOTE: see https://github.com/ocaml/odoc/issues/188 *)
  "NESTED_EMPHASIS";

  (* NOTE: see https://github.com/ocaml/odoc/pull/185#discussion_r217906131 *)
  "MISSING_STARTTAG";
  "DISCARDING_UNEXPECTED";

  (* NOTE: see https://github.com/ocaml/odoc/issues/186 *)
  "ANCHOR_NOT_UNIQUE";
]


let is_present_in_path =
  Sys.command "which tidy > /dev/null" = 0 && Sys.command "tidy -show-config < /dev/null | grep '^mute' > /dev/null" = 0


(* Returns a list of errors and warnings. *)
let validate file =
  if not (Sys.file_exists file) then
    invalid_arg ("tidy: file `" ^ file ^ "` does not exist");
  let muted_warnings = String.concat "," muted_warnings in
  let options = String.concat " " [
      "-quiet";
      "--mute " ^ muted_warnings;
      "--mute-id yes";
      "--show-errors 200";
      "-errors";
      "-ashtml"
    ] in
  let cmd = Printf.sprintf "tidy %s %s" options file in
  let (_, _, stderr) as proc = Unix.open_process_full cmd [||]  in

  let errors_and_warnings =
    let rec loop acc =
      match input_line stderr with
      | message -> loop (message :: acc)
      | exception End_of_file -> List.rev acc
    in
    loop []
  in

  match Unix.close_process_full proc with
  (* All input files were processed successfully. *)
  | WEXITED 0 -> []

  (* There were warnings. *)
  | WEXITED 1

  (* There were errors. *)
  | WEXITED 2 ->
    errors_and_warnings

  | _ ->
    let msg = "Unexpected process termination while running: " ^ cmd in
    raise (Failure msg)
