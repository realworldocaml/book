open! Import
module Sys = Sys0

type t = Caml.Printexc.raw_backtrace

let elide = ref am_testing
let elided_message = "<backtrace elided in test>"

let get ?(at_most_num_frames = Int.max_value) () =
  Caml.Printexc.get_callstack at_most_num_frames
;;

let to_string t =
  if !elide then elided_message else Caml.Printexc.raw_backtrace_to_string t
;;

let to_string_list t = String.split_lines (to_string t)
let sexp_of_t t = Sexp.List (List.map (to_string_list t) ~f:(fun x -> Sexp.Atom x))

module Exn = struct
  let set_recording = Caml.Printexc.record_backtrace
  let am_recording = Caml.Printexc.backtrace_status
  let most_recent () = Caml.Printexc.get_raw_backtrace ()

  (* We turn on backtraces by default if OCAMLRUNPARAM doesn't explicitly mention them. *)
  let maybe_set_recording () =
    let ocamlrunparam_mentions_backtraces =
      match Sys.getenv "OCAMLRUNPARAM" with
      | None -> false
      | Some x -> List.exists (String.split x ~on:',') ~f:(String.is_prefix ~prefix:"b")
    in
    if not ocamlrunparam_mentions_backtraces then set_recording true
  ;;

  (* the caller set something, they are responsible *)

  let with_recording b ~f =
    let saved = am_recording () in
    set_recording b;
    Exn.protect ~f ~finally:(fun () -> set_recording saved)
  ;;
end

let initialize_module () = Exn.maybe_set_recording ()
