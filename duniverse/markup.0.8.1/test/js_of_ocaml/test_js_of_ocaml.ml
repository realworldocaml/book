(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

let () =
  Markup.of_list [1; 2; 3] |> Markup_lwt.to_list |> ignore
