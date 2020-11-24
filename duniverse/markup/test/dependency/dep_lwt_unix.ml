(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

let (|>) x f = f x

let () =
  Markup.of_list [] |> Markup_lwt_unix.to_channel Lwt_io.stdout |> Lwt_main.run
