(*
rewrite
<link linkend="packaging-and-build-systems">xref</link>
to
<xref linkend="packaging-and-build-systems" />
*)

type tree = 
  | Element of Xmlm.tag * tree list
  | Data of string

let in_tree i = 
  let el tag children = Element (tag, children) in
  let data d = Data d in
  Xmlm.input_doc_tree ~el ~data i

let out_tree o t = 
  let frag = function
  | Element (tag, childs) -> `El (tag, childs) 
  | Data d -> `Data d in
  Xmlm.output_doc_tree frag o t

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
  let (_,it) = in_tree i in
  let ot = transform it in
  out_tree o (None, ot)
