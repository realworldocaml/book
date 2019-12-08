(* This Source Code Form is subject to the terms of the Mozilla Public License,
   v. 2.0. If a copy of the MPL was not distributed with this file, You can
   obtain one at http://mozilla.org/MPL/2.0/. *)



let make sep =
  object (self)
    method header = ""
    method footer = ""
    method summary s = "-" ^ sep ^ (self#sum s)
    method file_header f = f ^ sep
    method file_footer _ = ""
    method file_summary s = self#sum s
    method point _ _ = ""
    method private sum s =
      Printf.sprintf "%d%s%d\n" s.Report_utils.visited sep s.Report_utils.total
  end
