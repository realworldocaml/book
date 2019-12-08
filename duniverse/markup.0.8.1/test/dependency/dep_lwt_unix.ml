(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

let (|>) x f = f x

let () =
  Markup.of_list [] |> Markup_lwt_unix.to_channel Lwt_io.stdout |> Lwt_main.run
