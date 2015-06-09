open Core.Std

let in_tree i =
  let el tag children = `El (tag, children) in
  let data d = `Data d in
  Xmlm.input_doc_tree ~el ~data i

let read_document chan : (Xmlm.dtd * Cow.Xml.t) =
  let i = Xmlm.make_input (`Channel chan) in
  let (dtd,doc) = in_tree i in
  (dtd, [doc])

let out_tree o t = 
  let frag = function
  | `El (tag, childs) -> `El (tag, childs) 
  | `Data d -> `Data d in
  Xmlm.output_doc_tree frag o t

let write_document chan dtd doc =
  let o = Xmlm.make_output ~decl:false (`Channel chan) in
  match doc with
  |[] -> ()
  |[hd] -> out_tree o (dtd, hd)
  |hd::tl ->
     out_tree o (dtd, hd);
     List.iter ~f:(fun t -> out_tree o (None, t)) tl

let mk_tag ?(attrs=[]) tag_name contents =
  let attrs : Xmlm.attribute list = List.map ~f:(fun (k,v) -> ("",k),v) attrs in
  let tag = ("", tag_name), attrs in
  `El (tag, contents)

let rec map ~tag ~f (i:Cow.Xml.t) : Cow.Xml.t =
  List.concat (
    List.map i ~f:(
      function
      | `El ((("",t),attr),c) when t=tag -> f attr c
      | `El (p,c) -> [`El (p, (map ~tag ~f c))]
      | `Data x -> [`Data x]
    )
  )

let rec iter ~tag ~f (i:Cow.Xml.t) : unit =
  List.iter i ~f:(
    function
    | `El ((("",t),attr),c) when t=tag -> f attr c
    | `El (_,c) -> iter ~tag ~f c
    | `Data _ -> ()
  )

let to_string c =
  match c with
  | `Data  x -> x
  | `El _ -> failwith "Xml_tree.filter_string: encounter tag in string"
