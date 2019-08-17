(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

let (|>) x f = f x

let () =
  [1; 2; 3] |> Markup.of_list |> Markup_lwt.to_list |> ignore
