(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



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
