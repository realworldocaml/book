(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



let make () =
  object
    method header = ""
    method footer = ""
    method summary _ = ""
    method file_header f = Printf.sprintf "file %S\n" f
    method file_footer _ = ""
    method file_summary _ = ""
    method point ofs nb =
      Printf.sprintf "  point at offset %6d: %6d\n"
        ofs
        nb
  end
