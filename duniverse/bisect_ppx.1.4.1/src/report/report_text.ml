(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



let make summary_only =
  object (self)
    method header = ""
    method footer = ""
    method summary s = "Coverage summary: " ^ (self#sum s)
    method file_header f = if not summary_only then Printf.sprintf "File '%s': " f else ""
    method file_footer _ = ""
    method file_summary s = if not summary_only then self#sum s else ""
    method point _ _ = ""
    method private sum s =
      let numbers x y =
        if y > 0 then
          let p = ((float_of_int x) *. 100.) /. (float_of_int y) in
          Printf.sprintf "%d/%d (%.2f%%)" x y p
        else
          "none" in
      Report_utils.(numbers s.visited s.total) ^ "\n"
  end
