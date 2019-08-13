(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

(* This is run to generate entities.ml. It is not part of the normal build
   process, as entities.ml is checked into the source repository. It is only
   needed when the layout of the data structure in entities.ml is changed. *)

open! Yojson.Basic.Util

let () =
  print_endline
    ("(* Copyright © 2014 W3C® (MIT, ERCIM, Keio, Beihang). This software or " ^
     "document\n   includes material copied from or derived from W3C " ^
     "Recommendation HTML5\n   " ^
     "[https://www.w3.org/TR/2014/REC-html5-20141028/]. *)");
  print_newline ();

  print_endline "(* Generated automatically from entities.json. *)";
  print_newline ();

  print_string "let entities : ";
  print_string "(string * [ `One of int | `Two of int * int ]) array";
  print_string " = [|\n  ";

  Yojson.Basic.from_file "src/entities.json"
  |> to_assoc
  |> List.map (fun (k, v) ->
    let k = String.sub k 1 (String.length k - 2) in
    let v = v |> member "codepoints" |> to_list |> List.map to_int |> function
      | [c] -> Printf.sprintf "`One 0x%05X" c
      | [c; c'] -> Printf.sprintf "`Two (0x%05X, 0x%05X)" c c'
      | _ -> failwith "expected one or two code points"
    in
    Printf.sprintf "\"%s\", %s" k v)
  |> String.concat ";\n  "
  |> print_endline;

  print_endline "|]"
