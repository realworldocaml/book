(*
rewrite
<link linkend="packaging-and-build-systems">xref</link>
to
<xref linkend="packaging-and-build-systems" />
*)

open Xml_tree

let transform it =
  let rec aux = function
    | Element ( (("","link"),[("","linkend"),v]), [Data "xref"]) ->
       Element ( (("","xref"),[("","linkend"),v]), [])
    | Element (tag, children) ->
        Element (tag, List.map aux children)
    | x -> x
  in
  aux it

let _ =
  let i = Xmlm.make_input (`Channel stdin) in
  let o = Xmlm.make_output (`Channel stdout) in
  let (dtd,it) = in_tree i in
  let ot = transform it in
  out_tree o (dtd, ot)
