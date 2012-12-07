(* Call all the transformation functions we need over the Pandoc XML to our XML *)
open Xml_tree

let _ =
  let i = Xmlm.make_input (`Channel stdin) in
  let o = Xmlm.make_output (`Channel stdout) in
  let (dtd,it) = in_tree i in
  let ot = Rewrite_link_to_xref.t (Add_parts.t it) in
  out_tree o (dtd, ot)
