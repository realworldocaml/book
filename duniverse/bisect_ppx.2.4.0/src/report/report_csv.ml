(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



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
