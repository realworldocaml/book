open Core.Std

(** Handles a single request coming from stdin *)
let handle_one_stdin handler =
  printf ">>> %!"; (* prompt *)
  match In_channel.input_line stdin with
  | None -> `Stop (* terminate on end-of-stream, so Ctrl-D will exit *)
  | Some line ->
    if String.strip line = "" then `Continue
    else match Or_error.try_with (fun () -> Sexp.of_string line) with
    | Error err ->
      eprintf "Couldn't parse query: %s\n%!" (Error.to_sexp_hum err |! Sexp.to_string_hum);
      `Continue
    | Ok query_sexp ->
      let resp = Service.Handler.handle_request handler query_sexp in
      Sexp.output_hum stdout (<:sexp_of<Sexp.t Or_error.t>> resp);
      Out_channel.newline stdout;
      `Continue
;;

let handle_stdin services =
  let handler = Service.Handler.create services in
  let rec loop () =
    match handle_one_stdin handler with
    | `Stop -> ()
    | `Continue -> loop ()
  in
  loop ()
;;

module Counter : Service.S = struct
  type t = int ref

  let name = "update-counter"
  let create () = ref 0

  let handle_request t sexp =
    match Or_error.try_with (fun () -> int_of_sexp sexp) with
    | Error _ as err -> err
    | Ok x ->
      t := !t + x;
      Ok (sexp_of_int !t)
end

module List_dir : Service.S = struct

  type t = unit

  let name = "ls"
  let create () = ()

  let handle_request () sexp =
    match Or_error.try_with (fun () -> string_of_sexp sexp) with
    | Error _ as err -> err
    | Ok dir -> Ok (Array.sexp_of_t String.sexp_of_t (Sys.readdir dir))
end

let () =
  handle_stdin [(module List_dir : Service.S); (module Counter : Service.S)]
