(* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. *)

let () =
  Markup.of_list [1; 2; 3] |> Markup_lwt.to_list |> ignore
