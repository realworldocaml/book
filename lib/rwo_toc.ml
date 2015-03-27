open Core.Std
open Async.Std
module Html = Rwo_html
let (/) = Filename.concat

type part_info = {
  number : int;
  title : string;
}

type section = {
  id : string;
  title : string;
}

type sections = (section * (section * section list) list) list

type chapter = {
  number : int;
  filename : string;
  title : string;
  part_info : part_info option;
  sections : sections;
}

type part = {
  info : part_info option;
  chapters : chapter list
}

type t = part list

(** Return part info for the given chapter number, if the chapter is
    in a part. *)
let part_info_of_chapter chapter_num : part_info option =
  let parts = [
    (
      {number=1; title="Language Concepts"},
      [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12]
    );
    (
      {number=2; title="Tools and Techniques"},
      [13; 14; 15; 16; 17; 18]
    );
    (
      {number=3; title="The Runtime System"},
      [19; 20; 21; 22; 23; 24 ]
    );
  ]
  in
  List.fold parts ~init:None ~f:(fun accum (info,chapters) ->
    match accum with
    | Some _ as x -> x
    | None ->
      if List.mem chapters chapter_num
      then Some info
      else None
  )

let get_title file (t:Html.t) : string =
  let rec item_to_string = function
    | Nethtml.Data x -> x
    | Nethtml.Element ("span", _, childs) -> items_to_string childs
    | _ -> failwithf "%s: can't extract title string from h1 element" file ()
  and items_to_string items =
    String.concat ~sep:"" (List.map items ~f:item_to_string)
  in
  Html.get_all_nodes "h1" t
  |> function
    | [] ->
      failwithf "%s: cannot get title, no h1 element found" file ()
    | (Nethtml.Element ("h1", _, childs))::_ -> items_to_string childs
    | _ -> assert false

let get_sections file html =

  let title_to_id s =
    String.filter s ~f:(fun c -> Char.is_alphanum c || c = ' ')
    |> String.map ~f:(function ' ' -> '-' | c -> c)
  in

  let get_sections_helper data_type (item:Html.item)
      : (section * Html.item) list
      =
    let title_elem = match data_type with
      | "chapter" -> "h1"
      | "sect1" -> "h1"
      | "sect2" -> "h2"
      | "sect3" -> "h3"
      | _ ->
        failwithf "%s: unsupported section data-type = %s"
          file data_type ()
    in
    let rec loop accum = function
      | [] -> accum
      | (Nethtml.Element("section",attrs,childs) as item)::rest -> (
        if List.mem attrs ("data-type",data_type) then (
          match Html.filter_whitespace childs with
          | Nethtml.Element(name,_,[Nethtml.Data title])::_ -> (
            if name = title_elem then
              let id = match List.Assoc.find attrs "id" with
                | Some x -> x
                | None -> title_to_id title
              in
              loop (({title;id},item)::accum) rest
            else
              failwithf "%s: <section data-type=\"%s\"> must have <%s> as \
                         first child" file data_type title_elem ()
          )
          | _ ->
            failwithf "%s: <section data-type=\"%s\"> must have <%s> as \
                       first child" file data_type title_elem ()
        )
        else
          failwithf "%s: expected <section> with data-type=\"%s"
            file data_type ()
      )
      | _::rest ->
        loop accum rest
    in
    match item with
    | Nethtml.Data _ -> assert false (* only applied to [Element]s *)
    | Nethtml.Element(_,_,childs) -> (loop [] childs |> List.rev)
  in

  let body = match Html.get_all_nodes "body" html with
    | item::[] -> item
    | [] -> failwithf "%s: <body> element not found" file ()
    | _::_::_ -> failwithf "%s: multiple <body> elements found" file ()
  in

  let chapter_section = match get_sections_helper "chapter" body with
    | (_,item)::[] -> item
    | [] ->
      failwithf "%s: <section data-type=\"chapter\"> element not found"
        file ()
    | _::_::_ ->
      failwithf "%s: multiple <section data-type=\"chapter\"> elements found"
        file ()
  in

  get_sections_helper "sect1" chapter_section
  |> List.map ~f:(fun (sect,item) ->
    sect,
    (
      get_sections_helper "sect2" item
      |> List.map ~f:(fun (sect,item) ->
        sect,
        (
          get_sections_helper "sect3" item
          |> List.map ~f:fst
        )
      )
    )
  )
;;

let flatten_sections sections =
  List.fold sections ~init:[] ~f:(fun accum (section,childs) ->
    List.fold ~init:(section::accum) childs ~f:(fun accum (section,childs) ->
      List.fold ~init:(section::accum) childs ~f:(fun accum section ->
        section::accum
      )
    )
  )

let is_chapter_file file : bool =
  Filename.basename file
  |> String.split ~on:'-'
  |> List.hd_exn
  |> fun x ->
    try ignore (Int.of_string x); true
    with _ -> false

let get_chapters ?(repo_root=".") () : chapter list Deferred.t =
  let book_dir = repo_root/"book" in
  let number basename =
    String.split basename ~on:'-'
    |> List.hd_exn
    |> Int.of_string
  in
  Sys.readdir book_dir >>= fun a ->
  return (Array.to_list a) >>= fun l ->
  return (List.filter l ~f:is_chapter_file) >>=
  Deferred.List.map ~f:(fun basename ->
    let in_file = book_dir/basename in
    Html.of_file in_file >>| fun html ->
    let number = number basename in
    {
      number;
      filename = basename;
      title = get_title (book_dir/basename) html;
      part_info = part_info_of_chapter number;
      sections = get_sections in_file html;
    }
  ) >>|
  List.sort ~cmp:(fun a b -> Int.compare a.number b.number)

let get_next_chapter chapters curr_chapter : chapter option =
  List.find chapters ~f:(fun x -> curr_chapter.number = x.number - 1)

let of_chapters (chapters : chapter list) : part list =
  List.fold_right chapters ~init:[] ~f:(fun x accum ->
    match accum with
    | [] -> (* not building a part yet *)
      [{info = part_info_of_chapter x.number; chapters = [x]}]
    | p::rest ->
      let prev_part = p.info in
      let curr_part = (part_info_of_chapter x.number) in
      if prev_part = curr_part then
        {p with chapters = x::p.chapters}::rest
      else
        {info = curr_part; chapters = [x]}::p::rest
  )

let get ?repo_root () =
  get_chapters ?repo_root () >>|
  of_chapters
