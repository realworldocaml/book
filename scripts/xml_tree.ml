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

let mk_tag ?(attrs=[]) tag_name contents =
  let attrs : Xmlm.attribute list = List.map (fun (k,v) -> ("",k),v) attrs in
  let tag = ("", tag_name), attrs in
  Element (tag, contents)
