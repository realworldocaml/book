open Core
open Async

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
  name : string;
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

(* Convert section title to a valid HTML ID. *)
let title_to_id s =
  String.filter s ~f:(fun c -> Char.is_alphanum c || Char.(c = ' '))
  |> String.map ~f:(function ' ' -> '-' | c -> c)

let split_into_sections ~filename items =
  let rec loop accum = function
    | [] -> accum
    | (`Element{Html.name="section";attrs;childs} as item)::rest ->
      (match Html.filter_whitespace childs with
       | `Element{childs; _}::_ ->
         let title = html_to_title childs in
         let id = match List.Assoc.find ~equal:String.equal attrs "id" with
           | Some x -> x
           | None   -> title_to_id title
         in
         loop (({title;id},item)::accum) rest
       | _ ->
         failwithf "%s: <section> must have a header as first child" filename ())
    | _ :: rest -> loop accum rest
  in
  List.rev (loop [] items)

let sections_of_childs ~filename = function
  | `Data _ -> assert false (* only applied to [Element]s *)
  | `Element {Html.childs;_} -> split_into_sections ~filename childs

let section_of_chapter ~filename items =
  match split_into_sections ~filename items with
  | (_,item)::[] -> item
  | []           -> failwithf "%s: <section> element not found" filename ()
  | _::_::_      -> failwithf "%s: multiple <section> elements found" filename ()

let get_sections ~filename (html: Html.t) =
  section_of_chapter ~filename html
  |> sections_of_childs ~filename
  |> List.map ~f:(fun (sect, item) ->
      sect,
      sections_of_childs ~filename item
      |> List.map ~f:(fun (sect, item) ->
          sect,
          sections_of_childs ~filename item
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
    let file = book_dir / basename ^ ".html" in
    Html.of_file file >>| fun html ->
    let title = get_title file html in
    let sections = get_sections ~filename:file html in
    { number; name = basename; part_info; sections; title }
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
      if Poly.(prev_part = curr_part) then
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

let code_files ?(repo_root=".") () =
  Util.find_files (repo_root/"examples"/"code") >>|
  List.filter ~f:(function
      (* ignore auto-generated files *)
      | "./book/code/async/test.txt"
      | "./book/code/imperative-programming/numbers.txt" -> false
      | _ -> true
    )

let find ~name (t:t) =
  let rec aux = function
    | []   -> None
    | h::t ->
      match List.find h.chapters ~f:(fun c -> String.(c.name = name)) with
      | Some _ as x -> x
      | None -> aux t
  in
  aux t
