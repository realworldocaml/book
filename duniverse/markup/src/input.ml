(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Common
open Kstream

let preprocess is_valid_char report source =
  let first_char = ref true in
  let line = ref 1 in
  let column = ref 1 in

  let get_location () = !line, !column in

  let stream =
    (fun throw empty k ->
      let newline () =
        let location = !line, !column in
        line := !line + 1;
        column := 1;
        k (location, 0x0A)
      in

      let symbol c =
        let location = !line, !column in
        column := !column + 1;
        k (location, c)
      in

      let rec iterate () =
        next source throw empty (function
          | 0xFEFF when !first_char -> first_char := false; iterate ()

          | 0x0D ->
            next source throw newline (function
              | 0x0A -> newline ()
              | c -> push source c; newline ())

          | 0x0A -> newline ()

          | c when not (is_valid_char c) ->
            report (!line, !column)
              (`Bad_token (format_char c, "input", "out of range"))
              throw (fun () ->
            symbol c)

          | c -> symbol c)
      in
      iterate ())
    |> make
  in

  stream, get_location
