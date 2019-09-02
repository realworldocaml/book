
let process file lexbuf =
  match Octavius.parse lexbuf with
  | Octavius.Ok t ->
    Format.printf "%a@." Octavius.print t
  | Octavius.Error { error; location } ->
    let msg = Octavius.Errors.message error in
    Format.fprintf Format.err_formatter "@[<2>octavius:%s:%d.%d-%d.%d:@ %s@]@."
      file location.start.line location.start.column
        location.finish.line location.finish.column msg

let () =
  if Array.length Sys.argv <> 2 then begin
    Format.eprintf "Usage: %s FILE@." Sys.argv.(0);
    exit 1
  end;
  let file = Sys.argv.(1) in
  if not (Sys.file_exists file) then begin
    Format.eprintf "File \"%s\" does not exist@." file;
    exit 1
  end;
  let ic = open_in file in
  let lexbuf = Lexing.from_channel ic in
  process file lexbuf;
  close_in ic
