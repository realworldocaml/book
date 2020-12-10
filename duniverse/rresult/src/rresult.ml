(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
   %%NAME%% %%VERSION%%
  ---------------------------------------------------------------------------*)

type ('a, 'b) result = ('a, 'b) Result.result = Ok of 'a | Error of 'b

module R = struct

  let err_error = "result value is (Error _)"
  let err_ok = "result value is (Ok _)"

  (* Results *)

  type ('a, 'b) t = ('a, 'b) result
  let ok v = Ok v
  let error e = Error e
  let get_ok = function Ok v -> v | Error _ -> invalid_arg err_error
  let get_error = function Error e -> e | Ok _ -> invalid_arg err_ok
  let reword_error reword = function
  | Ok _ as r -> r
  | Error e -> Error (reword e)

  let return = ok
  let fail = error

  (* Composing results *)

  let bind v f = match v with Ok v -> f v | Error _ as e -> e
  let map f v = match v with Ok v -> Ok (f v) | Error _ as e -> e
  let join r = match r with Ok v -> v | Error _ as e -> e
  let ( >>= ) = bind
  let ( >>| ) v f = match v with Ok v -> Ok (f v) | Error _ as e -> e

  module Infix = struct
    let ( >>= ) = ( >>= )
    let ( >>| ) = ( >>| )
  end

  (* Error messages *)

  let pp_lines ppf s = (* hints new lines *)
    let left = ref 0 and right = ref 0 and len = String.length s in
    let flush () =
      Format.pp_print_string ppf (String.sub s !left (!right - !left));
      incr right; left := !right;
    in
    while (!right <> len) do
      if s.[!right] = '\n' then (flush (); Format.pp_force_newline ppf ()) else
      incr right;
    done;
    if !left <> len then flush ()

  type msg = [ `Msg of string ]
  let msg s = `Msg s
  let msgf fmt =
    let kmsg _ = `Msg (Format.flush_str_formatter ()) in
    Format.kfprintf kmsg Format.str_formatter fmt

  let pp_msg ppf (`Msg msg) = pp_lines ppf msg

  let error_msg s = Error (`Msg s)
  let error_msgf fmt =
    let kerr _ = Error (`Msg (Format.flush_str_formatter ())) in
    Format.kfprintf kerr Format.str_formatter fmt

  let reword_error_msg ?(replace = false) reword = function
  | Ok _ as r -> r
  | Error (`Msg e) ->
      let (`Msg e' as v) = reword e in
      if replace then Error v else error_msgf "%s\n%s" e e'

  let error_to_msg ~pp_error = function
  | Ok _ as r -> r
  | Error e -> error_msgf "%a" pp_error e

  let error_msg_to_invalid_arg = function
  | Ok v -> v
  | Error (`Msg m) -> invalid_arg m

  let open_error_msg = function Ok _ as r -> r | Error (`Msg _) as r -> r
  let failwith_error_msg = function Ok v -> v | Error (`Msg m) -> failwith m

  (* Trapping unexpected exceptions *)

  type exn_trap = [ `Exn_trap of exn * Printexc.raw_backtrace ]
  let pp_exn_trap ppf (`Exn_trap (exn, bt)) =
    Format.fprintf ppf "%s@\n" (Printexc.to_string exn);
    pp_lines ppf (Printexc.raw_backtrace_to_string bt)

  let trap_exn f v = try Ok (f v) with
  | e ->
      let bt = Printexc.get_raw_backtrace () in
      Error (`Exn_trap (e, bt))

  let error_exn_trap_to_msg = function
  | Ok _ as r -> r
  | Error trap ->
      error_msgf "Unexpected exception:@\n%a" pp_exn_trap trap

  let open_error_exn_trap = function
  | Ok _ as r -> r | Error (`Exn_trap _) as r -> r

  (* Pretty-printing *)

  let pp ~ok ~error ppf = function Ok v -> ok ppf v | Error e -> error ppf e
  let dump ~ok ~error ppf = function
  | Ok v -> Format.fprintf ppf "@[<2>Ok@ @[%a@]@]" ok v
  | Error e -> Format.fprintf ppf "@[<2>Error@ @[%a@]@]" error e

  (* Predicates *)

  let is_ok = function Ok _ -> true | Error _ -> false
  let is_error = function Ok _ -> false | Error _ -> true

  let equal ~ok ~error r r' = match r, r' with
  | Ok v, Ok v' -> ok v v'
  | Error e, Error e' -> error e e'
  | _ -> false

  let compare ~ok ~error r r' = match r, r' with
  | Ok v, Ok v' -> ok v v'
  | Error v, Error v' -> error v v'
  | Ok _, Error _ -> -1
  | Error _, Ok _ -> 1

  (* Converting *)

  let to_option = function Ok v -> Some v | Error e -> None
  let of_option ~none = function None -> none () | Some v -> Ok v
  let to_presult = function Ok v -> `Ok v | Error e -> `Error e
  let of_presult = function `Ok v -> Ok v | `Error e -> Error e

  (* Ignoring errors *)

  let ignore_error ~use = function Ok v -> v | Error e -> use e
  let kignore_error ~use = function Ok _ as r -> r | Error e -> use e
end

include R.Infix

(*---------------------------------------------------------------------------
   Copyright (c) 2015 Daniel C. Bünzli

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
