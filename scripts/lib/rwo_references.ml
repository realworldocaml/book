open Core

module Html = Rwo_html

let is_reference = function
  | `Data _ -> false
  | `Element {Html.name="a"; attrs; _} -> (
    match List.Assoc.find ~equal:String.equal attrs "data-type" with
    | Some "xref" -> true
    | Some _
    | None -> false
  )
  | `Element _ -> false

let create_reference_prefix url = (
  let chapter = string_of_int(int_of_string (String.slice url 0 2)) in
  `Data ("Chapter " ^ chapter ^ ", "))

let add_reference chapter_file = function
  | `Element {Html.name; attrs; childs} as orginal_element -> (
    let (_, prefix) = Filename.split chapter_file in
    let url = List.Assoc.find_exn ~equal:String.equal attrs "href" in
    if String.is_prefix url ~prefix then
      orginal_element
    else
    `Element {Html.name; attrs; childs = ([create_reference_prefix url] @ childs)}
  )
  | _ -> assert false
