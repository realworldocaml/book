(*
rewrite
<link linkend="packaging-and-build-systems">xref</link>
to
<xref linkend="packaging-and-build-systems" />
*)

open Xml_tree

let t it =
  let rec aux = function
    | Element ( (("","link"),[("","linkend"),v]), [Data "xref"]) ->
       Element ( (("","xref"),[("","linkend"),v]), [])
    | Element (tag, children) ->
        Element (tag, List.map aux children)
    | x -> x
  in
  aux it
