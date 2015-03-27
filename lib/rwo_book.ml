open Core.Std
open Async.Std
module Code = Rwo_code
module Html = Rwo_html
module Import = Rwo_import
let (/) = Filename.concat

(******************************************************************************)
(* Parts, Chapters, and Sections                                              *)
(******************************************************************************)
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

let chapters ?(repo_root=".") () : chapter list Deferred.t =
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

let next_chapter chapters curr_chapter : chapter option =
  List.find chapters ~f:(fun x -> curr_chapter.number = x.number - 1)

let group_chapters_by_part (chapters : chapter list) : part list =
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


(******************************************************************************)
(* HTML fragments                                                             *)
(******************************************************************************)
let head_item : Html.item =
  let open Html in
  head [
    meta ~a:["charset","utf-8"] [];
    meta ~a:[
      "name","viewport";
      "content","width=device-width, initial-scale=1.0"
    ] [];
    title [data "Real World OCaml"];
    link ~a:["rel","stylesheet"; "href","css/app.css"] [];
    script ~a:["src","js/min/modernizr-min.js"] [];
    script ~a:["src","//use.typekit.net/gfj8wez.js"] [];
    script [data "try{Typekit.load();}catch(e){}"];
  ]

let title_bar,title_bar_frontpage =
  let open Html in
  let nav = nav [
    a ~a:["href","index.html"] [data "Home"];
    a ~a:["href","toc.html"] [data "Table of Contents"];
    a ~a:["href","faqs.html"] [data "FAQs"];
    a ~a:["href","install.html"] [data "Install"];
    a ~a:["href","https://ocaml.janestreet.com/ocaml-core/"]
      [data "API Docs"];
  ]
  in
  let h1 = h1 [data "Real World OCaml"] in
  let h4 = h4 [data "Functional programming for the masses"] in
  let h5 = h5 [data "2"; sup [data "nd"]; data " Edition (in progress)"] in
  let title_bar =
    div ~a:["class","title-bar"] [
      div ~a:["class","title"] [h1; h5; nav]
    ]
  in
  let title_bar_frontpage =
    div ~a:["class","splash"] [
      div ~a:["class","image"] [];
      div ~a:["class","title"] [h1; h4; h5; nav]
    ]
  in
  title_bar,title_bar_frontpage


let footer_item : Html.item =
  let open Html in
  let links = [
    "http://twitter.com/realworldocaml", "@realworldocaml";
    "http://twitter.com/yminsky", "@yminsky";
    "http://twitter.com/avsm", "@avsm";
    "https://plus.google.com/111219778721183890368", "+hickey";
    "https://github.com/realworldocaml", "GitHub";
    "http://www.goodreads.com/book/show/16087552-real-world-ocaml", "goodreads";
  ]
  |> List.map ~f:(fun (href,text) -> li [a ~a:["href",href] [data text]])
  |> ul
  in
  footer [
    div ~a:["class","content"] [
      links;
      p [data "Copyright 2012-2014 \
         Jason Hickey, Anil Madhavapeddy and Yaron Minsky."];
    ]
  ]

let toc chapters : Html.item list =
  let open Html in
  let parts = group_chapters_by_part chapters in
  List.map parts ~f:(fun {info;chapters} ->
    let ul = ul ~a:["class","toc-full"] (List.map chapters ~f:(fun chapter ->
      li [
        a ~a:["href",chapter.filename] [
          h2 [data (
            if chapter.number = 0
            then sprintf "%s" chapter.title
            else sprintf "%d. %s" chapter.number chapter.title
          )]
        ];
        ul ~a:["class","children"] (
          List.map chapter.sections ~f:(fun (sect1,sect2s) ->
            let href = sprintf "%s#%s" chapter.filename sect1.id in
            li [
              a ~a:["href",href] [h5 [data sect1.title]];
              ul ~a:["class","children"] (
                List.map sect2s ~f:(fun (sect2,sect3s) ->
                  let href = sprintf "%s#%s" chapter.filename sect2.id in
                  li [
                    a ~a:["href",href] [data sect2.title];
                    ul ~a:["class","children"] (
                      List.map sect3s ~f:(fun sect3 ->
                        let href = sprintf "%s#%s" chapter.filename sect3.id in
                        li [a ~a:["href",href] [data sect3.title]]
                      ) );
                  ]
                ) );
            ]
          ) );
      ]
    ) )
    in
    match info with
    | None -> [ul]
    | Some x -> [h5 [data (sprintf "Part %d: %s" x.number x.title)]; ul]
  )
  |> List.concat

let next_chapter_footer next_chapter : Html.item option =
  let open Html in
  match next_chapter with
  | None -> None
  | Some x -> Some (
    a ~a:["class","next-chapter"; "href", x.filename] [
      div ~a:["class","content"] [
        h1 [
          small [data (sprintf "Next: Chapter %02d" x.number)];
          data x.title
        ]
      ]
    ]
  )

(** Process the given [html], adding or replacing elements to satisfy
    our main template. *)
let main_template ?(next_chapter=None) ?(title_bar=title_bar) html : Html.t =
  let rec f item = match item with
    | Nethtml.Data _ -> item
    | Nethtml.Element ("head", _, _) -> head_item
    | Nethtml.Element ("html",attrs,childs) ->
      Nethtml.Element (
        "html",
        (("class","no-js")::("lang","en")::attrs),
        (List.map childs ~f)
      )
    | Nethtml.Element ("body",attrs,childs) ->
      let childs = List.map childs ~f in
      let main_content = Html.(
        div ~a:["class","wrap"] [
          div ~a:["class","left-column"] [];
          article ~a:["class","main-body"] childs;
        ])
      in
      Html.body ~a:attrs (List.filter_map ~f:ident [
        Some title_bar;
        Some main_content;
        next_chapter_footer next_chapter;
        Some footer_item;
        Some (Html.script ~a:["src","js/jquery.min.js"] []);
        Some (Html.script ~a:["src","js/min/app-min.js"] []);
      ]
      )
    | Nethtml.Element (name,attrs,childs) ->
      Nethtml.Element (name, attrs, List.map ~f childs)
  in
  List.map ~f html


(******************************************************************************)
(* Make Pages                                                                 *)
(******************************************************************************)
let make_frontpage ?(repo_root=".") () : Html.t Deferred.t =
  return (
  main_template ~title_bar:title_bar_frontpage
    Html.[
      html [head []; body [data "empty for now"]]
    ]
  )
;;

let make_toc_page ?(repo_root=".") () : Html.t Deferred.t =
  chapters ~repo_root () >>| fun chapters ->
  main_template Html.[
    html [head []; body (toc chapters)]
  ]
;;

let make_chapter ?run_pygmentize repo_root chapters chapter_file
    : Html.t Deferred.t
    =
  let import_base_dir = Filename.dirname chapter_file in
  let chapter = List.find_exn chapters ~f:(fun x ->
    x.filename = Filename.basename chapter_file)
  in
  let next_chapter = next_chapter chapters chapter in

  (* OCaml code blocks *)
  let code : Code.phrase list Code.t ref = ref Code.empty in

  let update_code lang href : unit Deferred.t =
    match Code.file_is_mem !code href with
    | true -> return ()
    | false ->
      Code.add_file_exn
        ~lang
        ~run:(Code.run_file_exn ~repo_root ~lang)
        !code href
      >>| fun new_code ->
      code := new_code
  in

  let import_node_to_html (i:Import.t) : Html.t Deferred.t =
    let href = import_base_dir/i.href in
    update_code i.data_code_language href
    >>= fun () -> return (Code.find_exn !code ~file:href ?part:i.part)
    >>= fun contents ->
    Code.phrases_to_html ?run_pygmentize i.data_code_language contents
    >>| fun x -> [x]
  in

  let rec loop html =
    (Deferred.List.map html ~f:(fun item ->
      if Import.is_import_html item then
        import_node_to_html (ok_exn (Import.of_html item))
      else match item with
      | Nethtml.Data _ -> return [item]
      | Nethtml.Element (name, attrs, childs) -> (
        Deferred.List.map childs ~f:(fun x -> loop [x])
        >>| List.concat
        >>| fun childs -> [Nethtml.Element (name, attrs, childs)]
      )
     )
    )
    >>| List.concat
  in
  Html.of_file chapter_file >>= fun html ->
  loop html >>| fun html ->
  main_template ~next_chapter html
;;


(******************************************************************************)
(* Main Functions                                                             *)
(******************************************************************************)
type src = [
| `Chapter of string
| `Frontpage
| `Toc_page
| `FAQs
| `Install
]

let make ?run_pygmentize ?(repo_root=".") ~out_dir = function
  | `Frontpage -> (
    let out_file = out_dir/"index.html" in
    make_frontpage ~repo_root () >>= fun html ->
    return (Html.to_string html) >>= fun contents ->
    Writer.save out_file ~contents
  )
  | `Toc_page -> (
    let out_file = out_dir/"toc.html" in
    make_toc_page ~repo_root () >>= fun html ->
    return (Html.to_string html) >>= fun contents ->
    Writer.save out_file ~contents
  )
  | `Chapter in_file -> (
    let out_file = out_dir/(Filename.basename in_file) in
    chapters ~repo_root () >>= fun chapters ->
    make_chapter ?run_pygmentize repo_root chapters in_file >>= fun html ->
    return (Html.to_string html) >>= fun contents ->
    Writer.save out_file ~contents
  )
  | `FAQs -> (
    let base = "faqs.html" in
    let in_file = repo_root/"book"/base in
    let out_file = out_dir/base in
    Html.of_file in_file >>= fun html ->
    return (main_template html) >>= fun html ->
    return (Html.to_string html) >>= fun contents ->
    Writer.save out_file ~contents
  )
  | `Install -> (
    let base = "install.html" in
    let in_file = repo_root/"book"/base in
    let out_file = out_dir/base in
    Html.of_file in_file >>= fun html ->
    return (main_template html) >>= fun html ->
    return (Html.to_string html) >>= fun contents ->
    Writer.save out_file ~contents
  )


(******************************************************************************)
(* indexterm nodes                                                            *)
(******************************************************************************)
let indexterm_to_idx docs =
  let open Html in
  let rec loop item = match item with
    | Data _ -> item
    | Element ("a", attrs, [Data "&nbsp;"]) -> (
      if List.mem attrs ("data-type", "indexterm") then (
        match
          check_attrs attrs ~required:["data-type"; "data-primary"]
        with
        | Error _ ->
          item (* no point in recursing to single Data child *)
        | Ok () ->
          let data = sprintf "%s%s"
            (List.Assoc.find_exn attrs "data-primary")
            (
              try ("/" ^ List.Assoc.find_exn attrs "data-secondary")
              with Not_found -> ""
            )
          in
          let attrs = List.filter attrs ~f:(fun (x,_) ->
            match x with
            | "data-type" | "data-primary" | "data-secondary" -> false
            | _ -> true
          )
          in
          Element("idx", attrs, [Data data])
      )
      else
        item (* no point in recursing to single Data child *)
    )
    | Element (name, attrs, children) ->
      Element(name, attrs, List.map children ~f:loop)
  in
  List.map docs ~f:loop

let idx_to_indexterm t =
  let open Html in
  let rec loop item = match item with
    | Data _ -> item
    | Element ("idx", attrs, [Data data]) -> (
      match String.split data ~on:'/' with
      | x::[] ->
        Element (
          "a",
          ["data-type","indexterm"; "data-primary",x]@attrs,
          [Data "&nbsp;"]
        )
      | x::y ->
        Element (
          "a",
          [
            "data-type", "indexterm";
            "data-primary", x;
            "data-secondary", String.concat ~sep:"/" y;
          ]@attrs,
          [Data "&nbsp;"]
        )
      | _ ->
        failwithf
          "<idx> node's child must be slash separated string but got %s"
          data ()
    )
    | Element ("idx", _, _) ->
      failwith "<idx> node should have single Data child"
    | Element (name, attrs, childs) ->
      Element (name, attrs, List.map childs ~f:loop)
  in
  List.map t ~f:loop
