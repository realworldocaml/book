open Core.Std
open Async.Std

let log_delays thunk =
  let start = Time.now () in
  let print_time () =
    let diff = Time.diff (Time.now ()) start in
    Caml.Printf.printf ".%!";
    printf "%s, " (Time.Span.to_string diff)
  in
  let d = thunk () in
  Clock.every (sec 0.1) ~stop:d print_time;
  d >>| fun () -> print_time (); printf "\n"

type how_to_wait =
  | After
  | Busyloop
  | Busyloop_in_thread
  | Noalloc_busyloop_in_thread
  | Noalloc_busyloop_in_thread_2
with sexp

let busyloop () =
  let x = ref None in
  for i = 1 to 50_000_000 do x := Some i done

let noalloc_busyloop () =
  for _i = 1 to 50_000_000 do () done

let noalloc_busyloop_2 () =
  let rec loop i =
    if i = 0 then () else loop (i-1)
  in
  loop 50_000_000

let wait_and_log how_to_wait =
  let until = match how_to_wait with
    | After                        -> (fun () -> after (sec 1.))
    | Busyloop                     -> (fun () -> busyloop (); return ())
    | Busyloop_in_thread           -> (fun () -> In_thread.run busyloop)
    | Noalloc_busyloop_in_thread   -> (fun () -> In_thread.run noalloc_busyloop)
    | Noalloc_busyloop_in_thread_2 -> (fun () -> In_thread.run noalloc_busyloop_2)
  in
  log_delays until

let how_to_wait =
  Command.Spec.Arg_type.create
    (fun s -> Sexp.of_string s |> how_to_wait_of_sexp)

let () =
  Command.async_basic
    ~summary:"run logger without busy loop"
    Command.Spec.(
      empty
      +> anon ("how-to-wait" %: how_to_wait)
    )
    (fun how () -> wait_and_log how)
  |> Command.run
