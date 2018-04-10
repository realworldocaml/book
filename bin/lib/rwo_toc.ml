open Core
open Async
module Html = Rwo_html
module Import = Rwo_import
module Util = Rwo_util
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

let get_title file (t:Html.t) : string =
  let rec item_to_string = function
    | `Data x -> x
    | `Element {Html.name="span"; attrs=_; childs} -> items_to_string childs
    | _ -> failwithf "%s: can't extract title string from h1 element" file ()
  and items_to_string items =
    String.concat ~sep:"" (List.map items ~f:item_to_string)
  in
  Html.get_all_nodes "h1" t
  |> function
    | [] ->
      failwithf "%s: cannot get title, no h1 element found" file ()
    | (`Element {Html.name="h1"; attrs=_; childs})::_ -> items_to_string childs
    | _ -> assert false

let get_sections file (html: Html.t) =

  (* Convert arbitrary HTML to a section title, under the reasonable
      assumption that we don't put overly complex HTML within section
      titles. *)
  let html_to_title html =
    let rec loop accum = function
      | [] -> accum
      | (`Data x)::html -> loop (x::accum) html
      | (`Element {Html.childs;_})::html -> loop (loop accum childs) html
    in
    loop [] html |> List.rev |> String.concat ~sep:""
  in

  (* Convert section title to a valid HTML ID. *)
  let title_to_id s =
    String.filter s ~f:(fun c -> Char.is_alphanum c || c = ' ')
    |> String.map ~f:(function ' ' -> '-' | c -> c)
  in

  let sections_of_items data_type items =

    let title_elem = match data_type with
      | "chapter" -> "h1"
      | "sect1" -> "h2"
      | "sect2" -> "h3"
      | "sect3" -> "h4"
      | _ ->
        failwithf "%s: unsupported section data-type = %s"
          file data_type ()
    in
    let rec loop accum = function
      | [] -> accum
      | (`Element{Html.name="section";attrs;childs} as item)::rest -> (
          if List.mem ~equal:Rwo_util.string_pair_equal attrs ("data-type",data_type)
          then (
            match Html.filter_whitespace childs with
            | `Element{Html.name; attrs=_; childs}::_ -> (
                if name = title_elem then
                  let title = html_to_title childs in
                  let id = match List.Assoc.find ~equal:String.equal attrs "id" with
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
      | _ :: rest -> loop accum rest
    in
    List.rev (loop [] items)
  in
  let sections_of_childs data_type = function
    | `Data _ -> assert false (* only applied to [Element]s *)
    | `Element {Html.childs;_} -> sections_of_items data_type childs
  in

  let chapter_section = match sections_of_items "chapter" html with
    | (_,item)::[] -> item
    | [] ->
      failwithf "%s: <section data-type=\"chapter\"> element not found"
        file ()
    | _::_::_ ->
      failwithf "%s: multiple <section data-type=\"chapter\"> elements found"
        file ()
  in

  sections_of_childs "sect1" chapter_section
  |> List.map ~f:(fun (sect,item) ->
      sect,
      sections_of_childs "sect2" item
      |> List.map ~f:(fun (sect,item) ->
          sect,
          sections_of_childs "sect3" item
          |> List.map ~f:fst
        ))

let flatten_sections sections =
  List.fold sections ~init:[] ~f:(fun accum (section,childs) ->
    List.fold ~init:(section::accum) childs ~f:(fun accum (section,childs) ->
      List.fold ~init:(section::accum) childs ~f:(fun accum section ->
        section::accum
      )
    )
  )

module Toc = struct
  type part = {
    title   : string;
    chapters: string list;
  } [@@deriving sexp]

  type t = [ `part of part | `chapter of string] list [@@deriving sexp]

  let html_file f =
    try (Filename.chop_extension f) ^ ".html"
    with Invalid_argument _ -> f

  let html =
    let aux = function
      | `chapter f -> `chapter (html_file f)
      | `part p    -> `part {p with chapters = List.map ~f:html_file p.chapters}
    in
    List.map ~f:aux

  let read dir =
    let f = dir / "toc.scm" in
    Reader.file_contents f >>| fun contents ->
    let s = Sexplib.Sexp.scan_sexps (Lexing.from_string contents) in
    html (t_of_sexp (Sexplib.Sexp.List s))

end

let of_toc book_dir toc =
  let chapter part_info number basename =
    let file = book_dir/basename in
    Html.of_file file >>| fun html ->
    let title = get_title file html in
    let sections = get_sections file html in
    { number; filename = basename; part_info; sections; title }
  in
  let part ~parts ~chapters title files =
    let part_info = Some { title; number = parts } in
    Deferred.List.mapi ~f:(fun i f -> chapter part_info (i+chapters) f) files
    >>| fun chapters ->
    { info = part_info; chapters }
  in
  let rec aux ~parts ~chapters acc = function
    | [] -> return (List.rev acc)
    | `chapter c :: t ->
      chapter None chapters c >>= fun c ->
      let p = { info = None; chapters = [c] } in
      aux ~parts ~chapters:(chapters+1) (p :: acc) t
    | `part (p:Toc.part) :: t ->
      part ~parts ~chapters p.title p.Toc.chapters >>= fun p ->
      let chapters = chapters + List.length p.chapters in
      aux ~parts:(parts+1) ~chapters (p :: acc) t
  in
  aux ~parts:1 ~chapters:0 [] toc

let get_next_chapter chapters curr_chapter : chapter option =
  List.find chapters ~f:(fun x -> curr_chapter.number = x.number - 1)

let of_chapters (chapters : chapter list) : part list =
  List.fold_right chapters ~init:[] ~f:(fun x accum ->
    match accum with
    | [] -> (* not building a part yet *)
      [{info = x.part_info; chapters = [x]}]
    | p::rest ->
      let prev_part = p.info in
      let curr_part = x.part_info in
      if prev_part = curr_part then
        {p with chapters = x::p.chapters}::rest
      else
          {info = curr_part; chapters = [x]}::p::rest
    )

let get ?(repo_root=".") () =
  let book_dir = repo_root/"book" in
  Toc.read book_dir >>= fun toc ->
  of_toc book_dir toc

let flatten_chapters t =
  let rec aux acc = function
    | [] -> List.rev acc
    | { chapters; _ } :: t -> aux (List.rev_append chapters acc) t
  in
  aux [] t

let get_chapters ?repo_root () =
  get ?repo_root () >>| flatten_chapters

let imported_files ?(repo_root=".") () =
  get_chapters ~repo_root () >>= fun chapters ->
  Deferred.List.map chapters ~f:(fun chapter ->
      Html.of_file ("book"/chapter.filename) >>| fun html ->
      Import.find_all html |> fun l ->
      List.map l ~f:(fun x -> repo_root/"book"/x.Import.href)
    ) >>| fun ll ->
  List.concat ll |> fun l ->
  List.dedup_and_sort ~compare:String.compare l

let code_files ?(repo_root=".") () =
  Util.find_files (repo_root/"examples"/"code") >>|
  List.filter ~f:(function
      (* ignore auto-generated files *)
      | "./book/code/async/test.txt"
      | "./book/code/imperative-programming/numbers.txt" -> false
      | _ -> true
    )
