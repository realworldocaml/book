open! Import
module Printexc = Caml.Printexc

(* Small helper to find out who is the caller of a function *)

type t = Printexc.location option

let get ~skip =
  let skip = __FILE__ :: skip in
  let stack = Printexc.get_callstack 16 in
  let len = Printexc.raw_backtrace_length stack in
  let rec loop pos =
    if pos = len then None
    else
      match
        Printexc.get_raw_backtrace_slot stack pos
        |> Printexc.convert_raw_backtrace_slot |> Printexc.Slot.location
      with
      | None -> None
      | Some loc ->
          if List.mem ~set:skip loc.filename then loop (pos + 1) else Some loc
  in
  loop 0
