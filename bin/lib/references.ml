open Core

let is_reference = function
  | `Data _ -> false
  | `Element {Html.name="a"; attrs; _} -> (
    match List.Assoc.find ~equal:String.equal attrs "data-type" with
    | Some "xref" -> true
    | Some _
    | None -> false
  )
  | `Element _ -> false

let create_reference_prefix toc url =
  let filename =
    (* remove anchors *)
    match String.rsplit2 ~on:'#' url with
    | None        -> url
    | Some (f, _) -> f
  in
  let name =
    try Filename.chop_extension filename
    with Invalid_argument _ -> filename
  in
  match Toc.find ~name toc with
  | None   -> failwithf "invalid cross-reference: %s (%s)" url name ()
  | Some c -> `Data ("Chapter " ^ string_of_int c.number  ^ ", ")

let add_reference toc chapter_file = function
  | `Element {Html.name; attrs; childs} as orginal_element -> (
    let (_, prefix) = Filename.split chapter_file in
    let url = List.Assoc.find_exn ~equal:String.equal attrs "href" in
    if String.is_prefix url ~prefix then
      orginal_element
    else
      let childs = create_reference_prefix toc url :: childs in
      `Element {Html.name; attrs; childs}
  )
  | _ -> assert false
