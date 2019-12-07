(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open OUnit2
open Markup__Common
module Text = Markup__Text
module Error = Markup__Error
module Kstream = Markup__Kstream

let sprintf = Printf.sprintf

let wrong_k message = fun _ -> assert_failure message

let with_text_limit n f =
  let limit = !Text.length_limit in
  Text.length_limit := n;

  try f (); Text.length_limit := limit
  with exn -> Text.length_limit := limit; raise exn

let expect_error :
  ?allow_recovery:int -> location -> Error.t -> (Error.parse_handler -> unit) ->
    unit
    = fun ?(allow_recovery = 0) l error f ->
  let errors = ref 0 in
  let report l' error' _ k =
    errors := !errors + 1;

    if !errors > 1 + allow_recovery then
      sprintf "got additional error '%s'" (Error.to_string ~location:l' error')
      |> assert_failure;

    if !errors = 1 && (l' <> l || error' <> error) then
      sprintf "got error \"%s\"\nexpected \"%s\""
        (Error.to_string ~location:l' error')
        (Error.to_string ~location:l error)
      |> assert_failure;

    k ()
  in

  f report;

  if !errors = 0 then
    sprintf "no error\nexpected \"%s\"" (Error.to_string ~location:l error)
    |> assert_failure

let expect_sequence ?(prefix = false) id to_string sequence =
  let assert_failure s = assert_failure (id ^ "\n" ^ s) in

  let sequence = ref sequence in
  let invalid = ref false in

  let receive s throw =
    if !invalid then ()
    else
      match !sequence with
      | [] ->
        if not prefix then begin
          invalid := true;
          sprintf "got \"%s\"\nexpected no more output" (to_string s)
          |> assert_failure
        end

      | first::rest ->
        if s = first then
          sequence := rest
        else begin
          invalid := true;
          sprintf "got \"%s\"\nexpected \"%s\"" (to_string s) (to_string first)
          |> assert_failure
        end;

        match rest, prefix with
        | [], true -> throw Exit
        | _ -> ()
  in

  let ended () =
    if !invalid then ()
    else
      match !sequence with
      | [] -> ()
      | first::_ ->
        sprintf "got end\nexpected \"%s\"" (to_string first)
        |> assert_failure
  in

  receive, ended

let iter iterate s =
  Kstream.iter iterate s (function
    | Exit -> ()
    | exn -> raise exn)
    ignore

type 'a general_signal = S of 'a | E of Error.t

let expect_signals ?prefix signal_to_string id signals =
  let to_string = function
    | l, c, S s -> sprintf "line %i, column %i: %s" l c (signal_to_string s)
    | l, c, E e -> sprintf "line %i, column %i: %s" l c (Error.to_string e)
  in

  let receive, ended = expect_sequence ?prefix id to_string signals in

  let report (l, c) e throw k = receive (l, c, E e) throw; k () in
  let signal ((l, c), s) throw k = receive (l, c, S s) throw; k () in

  report, signal, ended

let expect_strings id strings =
  let to_string = function
    | S s -> s
    | E e -> Error.to_string e
  in

  let receive, ended = expect_sequence id to_string strings in

  let report _ e throw k = receive (E e) throw; k () in
  let string s throw k = receive (S s) throw; k () in

  report, string, ended
