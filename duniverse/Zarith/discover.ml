let () = Printexc.record_backtrace true

let () =
  if Array.length Sys.argv <> 2 then (
    print_endline "Usage: ./configure_env.exe %{cc}";
    exit 1)

let uname () =
  let ic = Unix.open_process_in "uname -s" in
  let s = input_line ic in
  String.trim s

module Var : sig
  val os : string
  val is_homebrew_amr64 : bool
end = struct
  let is_homebrew_amr64 = Sys.file_exists "/opt/homebrew/bin/brew"

  let normalise raw =
    match String.lowercase_ascii raw with "darwin" | "osx" -> "macos" | s -> s

  let os = normalise (match Sys.os_type with "Unix" -> uname () | s -> s)
end

let cc = Sys.argv.(1)

let ldflags =
  match Unix.getenv "LDFLAGS" with exception Not_found -> "" | s -> s

let cflags =
  match Unix.getenv "CFLAGS" with exception Not_found -> "" | s -> s

let flags =
  match Var.os with
  | "openbsd" | "freebsd" ->
      Printf.sprintf
        "LDFLAGS=\"%s -L/usr/local/lib\" CFLAGS=\"%s -I/usr/local/include\""
        ldflags cflags
  | "macos" when Var.is_homebrew_amr64 ->
      Printf.sprintf
        "LDFLAGS=\"%s -L/opt/homebrew/lib\" CFLAGS=\"%s \
         -I/opt/homebrew/include\""
        ldflags cflags
  | "macos" ->
      Printf.sprintf
        "LDFLAGS=\"%s -L/opt/local/lib -L/usr/local/lib\" CFLAGS=\"%s \
         -I/opt/local/include -I/usr/local/include\""
        ldflags cflags
  | _ -> ""

let () = Printf.printf "CC=\"%s\" %s%!" cc flags
