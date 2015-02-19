open Core.Std
open Rwo_core2
open Async.Std
module Code = Rwo_code
module Html = Rwo_html
let (/) = Filename.concat

(******************************************************************************)
(* <link rel="import"> nodes                                                  *)
(******************************************************************************)
type import = {
  data_code_language : Rwo_code.lang;
  href : string;
  part : float option;
  childs : Rwo_html.item list;
} with sexp

let is_import_node = function
  | Nethtml.Data _ -> false
  | Nethtml.Element ("link", attrs, _) -> (
    match List.Assoc.find attrs "rel" with
    | Some "import" -> true
    | Some _
    | None -> false
  )
  | Nethtml.Element (_,_,_) -> false

let parse_import item =
  let open Result.Monad_infix in
  if not (is_import_node item) then
    error "attempting to parse non-import node as an import node"
      item Html.sexp_of_item
  else (
    match item with
    | Nethtml.Element ("link", attrs, childs) -> (
      let find x = List.Assoc.find attrs x in
      Html.check_attrs attrs
        ~required:["data-code-language"; "href"; "rel"]
        ~allowed:(`Some ["part"])
      >>= fun () ->

      (
        try Ok (find "part" |> Option.map ~f:Float.of_string)
        with exn -> error "invalid part" exn sexp_of_exn
      ) >>= fun part ->

      Code.lang_of_string (Option.value_exn (find "data-code-language"))
      >>= fun data_code_language ->

      Ok {
        data_code_language;
        href = Option.value_exn (find "href");
        part;
        childs;
      }
    )
    | Nethtml.Element (_,_,_)
    | Nethtml.Data _ ->
      assert false
  )
;;

let import_to_item x =
  [
    Some ("rel", "import");
    Some ("data-code-language", Code.lang_to_string x.data_code_language);
    Some ("href", x.href);
    (Option.map x.part ~f:(fun x -> "part", Float.to_string x));
  ]
  |> List.filter_map ~f:ident
  |> fun a -> Html.link ~a []


(******************************************************************************)
(* <p></p><pre></pre> sections                                                *)
(******************************************************************************)
type code_item = [
| `Output of string
| `Prompt of string
| `Input of string
| `Data of string
]

type code_block = code_item list

type pre = {
  data_type : string;
  data_code_language : string option;
  code_block : code_block;
}

type p = {
  a_href : string;
  a_class : string;
  em_data : string;
  data1 : string option;
  part : float option;
  a2 : Rwo_html.item option;
  a3 : Rwo_html.item option;
}

type code_section = {p:p; pre:pre}

(** Replace ampersand encoded HTML sub-strings with their ASCII
    counterparts. *)
let decode_html_string (s:string) : string =
  s
  |> String.substr_replace_all ~pattern:"&amp;" ~with_:"&"
  |> String.substr_replace_all ~pattern:"&lt;" ~with_:"<"
  |> String.substr_replace_all ~pattern:"&gt;" ~with_:">"

(** Extract data from an HTML node. Be tolerant and accept some
    non-DATA nodes. *)
let rec item_to_string (item:Html.item) : string Or_error.t =
  let open Result.Monad_infix in
  match item with
  | Nethtml.Data x -> Ok x
  | Nethtml.Element ("em", [], childs) -> (
    Result.List.map childs ~f:item_to_string
    >>| String.concat ~sep:""
  )
  | Nethtml.Element _ ->
    error "cannot treat node as data" item Html.sexp_of_item

(** Like [item_to_string but for list of items. *)
let items_to_string (items : Html.item list) : string Or_error.t =
  let open Result.Monad_infix in
  Result.List.map items ~f:item_to_string
  >>| String.concat ~sep:""

let parse_p_before_pre (item:Html.item) : p Or_error.t =
  let open Result.Monad_infix in
  let open Nethtml in

  (** Return DATA under <em>. *)
  let parse_em item : string Or_error.t =
    match item with
    | Element ("em", ["class","hyperlink"], [Data em_data]) ->
      Ok em_data
    | _ ->
      error "unexpected form for <em> node" item Html.sexp_of_item
  in

  (** Return href and class attribute values of main <a>, and the data
      under the <em> child of it. *)
  let parse_main_a item : (string * string * string) Or_error.t =
    match item with
    | Element ("a", attrs, [em]) -> (
      let find x = Option.value_exn (List.Assoc.find attrs x) in
      Html.check_attrs attrs ~required:["href";"class"] ~allowed:(`Some [])
      >>= fun () ->
      parse_em em >>= fun em_data ->
      Ok (find "href", find "class", em_data)
    )
    | _ ->
      error "unexpected form for main <a> node" item Html.sexp_of_item
  in

  (** The 2nd data field, i.e. the one after the main <a>, optionally
      contains the part number. Parse it out. *)
  let parse_data2 (x : string option) : float option Or_error.t =
    match x with
    | None -> Ok None
    | Some x -> (
      try
        Ok (Some (Scanf.sscanf (String.strip x) "(part %f)" ident))
      with
        exn ->
          error "error parsing part number from data2 node"
            (x, exn) <:sexp_of< string * exn >>
    )
  in

  let values = match item with
    | Element ("p", [], main_a::[]) ->
      Ok (main_a, None, None, None, None)
    | Element ("p", [], main_a::(Data data2)::[]) ->
      Ok (main_a, None, Some data2, None, None)
    | Element ("p", [], (Data data1)::main_a::[]) ->
      Ok (main_a, Some data1, None, None, None)
    | Element ("p", [], (Data data1)::main_a::(Data data2)::[]) ->
      Ok (main_a, Some data1, Some data2, None, None)
    | Element ("p", [], main_a::(Data data2)::((Element("a",_,_)) as a2)::[]) ->
      Ok (main_a, None, Some data2, Some a2, None)
    | Element (
      "p", [], main_a::(Data data2)::((Element("a",_,_)) as a2)::((Element("a",_,_)) as a3)::[]
    ) ->
      Ok (main_a, None, Some data2, Some a2, Some a3)
    | _ ->
      error "unexpected format of <p> before <pre>"
        item Html.sexp_of_item
  in
  values >>= fun (main_a,data1,data2,a2,a3) ->
  parse_data2 data2 >>= fun part ->
  parse_main_a main_a >>= fun (a_href,a_class,em_data) ->
  Ok {a_href;a_class;em_data;data1;part;a2;a3}


(** Parse <code> element, requiring exactly the given attributes. *)
let parse_code_helper expected_attrs item : string Or_error.t =
  match item with
  | Nethtml.Data _ ->
    error "expected <code> but got DATA"
      (expected_attrs,item) <:sexp_of< Html.attributes * Html.item >>
  | Nethtml.Element ("code", attrs, childs) -> (
    if attrs = expected_attrs then
      Result.map (items_to_string childs) ~f:decode_html_string
    else
      error "expected attributes differ from the ones found"
        (expected_attrs, item)
        <:sexp_of< Html.attributes * Html.item >>
  )
  | Nethtml.Element (_,_,_) ->
    error "expected <code> but got other type of node"
      (expected_attrs,item) <:sexp_of< Html.attributes * Html.item >>

(** Parse <code> element. *)
let parse_code item : string Or_error.t =
  parse_code_helper [] item

(** Parse <code class="prompt"> element. *)
let parse_code_prompt item : [> `Prompt of string] Or_error.t =
  parse_code_helper ["class","prompt"] item
  |> Result.map ~f:(fun x -> `Prompt x)

(** Parse <code class="computeroutput"> element. *)
let parse_code_computeroutput item : [> `Output of string] Or_error.t =
  parse_code_helper ["class","computeroutput"] item
  |> Result.map ~f:(fun x -> `Output x)

(** Parse <strong><code>DATA</code></strong> element.*)
let parse_strong_code item : [> `Input of string] Or_error.t =
  match item with
  | Nethtml.Data x ->
    error "expected <strong> but got DATA" x sexp_of_string
  | Nethtml.Element ("strong", [], [elem]) -> (
    match parse_code elem with
    | Ok x -> Ok (`Input x)
    | Error _ as e -> Or_error.tag e "within <strong>"
  )
  | Nethtml.Element ("strong", [], _) ->
    Or_error.error_string "expected exactly one child of <strong>"
  | Nethtml.Element ("strong", attrs, _::[]) ->
    error "unexpected attributes in <strong>" attrs Html.sexp_of_attributes
  | Nethtml.Element (name, _, _) ->
    error "expected <strong> but got other type of node" name sexp_of_string

let parse_data item : [> `Data of string] Or_error.t =
  let open Result.Monad_infix in
  item_to_string item
  >>| decode_html_string
  >>| fun x -> `Data x

let code_items_to_code_block code_items =
  let open Result.Monad_infix in
  let rec loop (b:code_block) = function
    | [] -> Ok b
    | ((`Input _ as x) | (`Output _ as x) | (`Data _ as x))::l ->
      loop (x::b) l
    | (`Prompt _ as x)::(`Input _ as y)::l ->
      loop (y::x::b) l
    | (`Prompt _)::(`Output x)::_ ->
      error "prompt followed by output" x sexp_of_string
    | (`Prompt _)::(`Data x)::_ ->
      error "prompt followed by data" x sexp_of_string
    | (`Prompt x)::(`Prompt y)::_ ->
      error "two successive code prompts"
        (x,y) <:sexp_of< string * string >>
    | (`Prompt _)::[] ->
      Or_error.error_string "prompt not followed by anything"
  in
  loop [] code_items
  >>| List.rev

let parse_code_block (items : Html.item list) : code_block Or_error.t =
  let open Result.Monad_infix in
  (Result.List.fold items ~init:[] ~f:(fun accum item ->
    match parse_code_prompt item with
    | Ok x -> Ok (x::accum)
    | Error _ as e1 ->
      match parse_strong_code item with
      | Ok x -> Ok (x::accum)
      | Error _ as e2 ->
        match parse_code_computeroutput item with
        | Ok x -> Ok (x::accum)
        | Error _ as e3 ->
          match parse_data item with
          | Ok x -> Ok (x::accum)
          | Error _ as e4 ->
            Or_error.(
              tag (combine_errors [e1;e2;e3;e4])
                "expected one of these to not happen but all happened"
            )
   )
  )
  >>| List.rev
  >>= code_items_to_code_block


(** Parse <pre> element. *)
let parse_pre item =
  let open Result.Monad_infix in
  let required = ["data-type"] in
  let allowed = `Some ["data-code-language"] in
  match item with
  | Nethtml.Data x ->
    error "expected <pre> but got DATA" x sexp_of_string
  | Nethtml.Element ("pre", attrs, childs) -> (
    let find x = List.Assoc.find attrs x in
    Html.check_attrs attrs ~required ~allowed >>= fun () ->
    parse_code_block childs >>| fun code_block ->
    {
      data_type = Option.value_exn (find "data-type");
      data_code_language = find "data-code-language";
      code_block;
    }
  )
  | Nethtml.Element (name, _, _) ->
    error "expected <pre> but got other type of node" name sexp_of_string


(** Find all <pre> nodes in [html]. Also find filename to which code
    should be extracted by searching for sibling <p> node that has
    this information. *)
let map_code_sections (html:Html.t) ~f =
  let open Nethtml in
  let open Result.Monad_infix in
  let rec loop = function
    | ((Element ("p",_,_)) as p_node)
      ::((Element ("pre",_,_)) as pre_node)
      ::rest
       -> (
         parse_p_before_pre p_node >>= fun p ->
         parse_pre pre_node >>= fun pre ->
         loop rest >>= fun rest ->
         Ok ((f {p;pre})::rest)
       )
    | [] ->
      Ok []
    | (Data _ as x)::rest -> (
      loop rest >>= fun rest ->
      Ok (x::rest)
    )
    | (Element (name,attrs,childs))::rest -> (
      loop childs >>= fun childs ->
      loop rest >>= fun rest ->
      Ok ((Element(name,attrs,childs))::rest)
    )
  in
  loop html

(* Notes:

   - x.pre.data_type ignored because it is always "programlisting"

   - x.pre.code_block ignored because it is not part of [import]
   type. Must be printed to external file referred to in [href].
*)
let code_section_to_import (x:code_section) : import Or_error.t =
  let open Result.Monad_infix in
  (
    match x.pre.data_code_language, x.p.em_data, x.p.data1 with
    | (None, "Scheme", None) -> Ok `Scheme
    | (None, "Syntax", None) -> Ok `OCaml_syntax
    | (Some "bash", "Shell script", None) -> Ok `Bash
    | (Some "c", "C", None) -> Ok `C
    | (Some "c", "C:", None) -> Ok `C
    | (Some "console", "Terminal", None) -> Ok `Console
    | (Some "gas", "Assembly", None) -> Ok `Gas
    | (Some "java", "Java", None) -> Ok `Java
    | (Some "java", "objects/Shape.java", Some "Java: ") -> Ok `Java
    | (Some "json", "JSON", None) -> Ok `JSON
    | (Some "ocaml", "OCaml", None) -> Ok `OCaml
    | (Some "ocaml", "OCaml", Some "OCaml: ") -> Ok `OCaml
    | (Some "ocaml", "OCaml utop", None) -> Ok `OCaml_toplevel
    | (Some "ocaml", "Syntax", None) -> Ok `OCaml_syntax
    | (Some "ocaml", "Syntax", Some "\n      ") -> Ok `OCaml_syntax
    | (Some "ocaml", "guided-tour/recursion.ml", Some "OCaml: ") ->
      Ok `OCaml
    | (Some "ocaml",
       "variables-and-functions/abs_diff.mli",
       Some "OCaml: "
    ) ->
      Ok `OCaml
    | (Some "scheme", "Scheme", None) ->
      Ok `Scheme
    | _ ->
      error "unable to determine language"
        (x.pre.data_code_language, x.p.em_data, x.p.data1)
        <:sexp_of< string option * string * string option >>
  ) >>| fun data_code_language ->
  {
    data_code_language;
    href = String.chop_prefix_exn x.p.a_href
      ~prefix:"https://github.com/realworldocaml/examples/tree/v1/code/"
    ;
    part = x.p.part;
    childs = List.filter_map ~f:ident [x.p.a2; x.p.a3];
  }

module ImportMap = Map.Make(
  struct
    type t = import
    with sexp

    (* Ignore [data_code_language] and [childs]. *)
    let compare (x:t) (y:t) =
      compare (x.href, x.part) (y.href, y.part)
  end
)

let extract_code_from_1e_exn chapter =
  let base = sprintf "ch%02d" chapter in
  let in_file = "book"/(base ^ ".html") in
  let out_file = "tmp"/"book"/(base ^ ".html") in
  let code_dir = "tmp"/"book"/"code" in
  let imports : code_block ImportMap.t ref = ref ImportMap.empty in

  let code_block_to_string code_block lang part : string =
    let part = match part with
      | None | Some 0. -> ""
      | Some part -> (match lang with
        | `OCaml_toplevel -> sprintf "\n#part %f\n" part
        | `OCaml -> sprintf "\n\n(* part %f *)\n" part
        | _ ->
          ok_exn (error "unexpected part number with this language"
                    (part, lang) <:sexp_of< float * Code.lang >>
          )
      )
    in

    let f = function
      | `Output x | `Prompt x -> (match lang with
        | `OCaml_toplevel -> ""
        | _ -> x
      )
      | `Input x | `Data x -> x
    in
    List.map code_block ~f
    |> String.concat ~sep:""
    |> fun x -> part ^ x ^ "\n"
  in

  let write_imports () : unit Deferred.t =
    Map.to_alist !imports
    |> Deferred.List.iter ~f:(fun (i,blk) ->
      let out_file = code_dir/i.href in
      let contents = code_block_to_string blk i.data_code_language i.part in
      Unix.mkdir ~p:() (Filename.dirname out_file) >>= fun () ->
      Writer.with_file ~append:true out_file ~f:(fun wrtr ->
        return (Writer.write wrtr contents))
    )
  in

  let f (x:code_section) : Html.item =
    let i:import = ok_exn (code_section_to_import x) in
    (if Map.mem !imports i then
        printf "WARNING: duplicate import for file %s part %f\n"
          i.href (match i.part with Some x -> x | None -> (-1.))
     else
        imports := Map.add !imports ~key:i ~data:x.pre.code_block;
    );
    import_to_item i
  in

  Html.of_file in_file >>= fun t ->
  return (ok_exn (map_code_sections t ~f)) >>= fun t ->
  write_imports () >>= fun () ->
  Writer.save out_file ~contents:(Html.to_string t)
;;


(******************************************************************************)
(* Parts and Chapters                                                         *)
(******************************************************************************)
type part_info = {
  number : int;
  title : string;
}

type chapter = {
  number : int;
  filename : string;
  title : string;
  part_info : part_info option;
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
      [19; 20; 21; 22; 23]
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
    Html.of_file (book_dir/basename) >>| fun html ->
    let number = number basename in
    {
      number;
      filename = basename;
      title = get_title (book_dir/basename) html;
      part_info = part_info_of_chapter number;
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
    let ul = ul ~a:["class","toc"] (List.map chapters ~f:(fun x ->
      li [
        a ~a:["href",x.filename] [
          h2 [
            small [data (sprintf "Chapter %02d" x.number)];
            data x.title;
          ]
        ]
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
  chapters ~repo_root () >>| fun chapters ->
  main_template ~title_bar:title_bar_frontpage
    Html.[
      html [head []; body (toc chapters)]
    ]
;;

let make_chapter repo_root chapters chapter_file
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

  let import_node_to_html (i:import) : Html.t Deferred.t =
    let href = import_base_dir/i.href in
    update_code i.data_code_language href
    >>| fun () ->
    Code.find_exn !code ~file:href ?part:i.part
    |> Code.phrases_to_html i.data_code_language
    |> fun x -> [x]
  in

  let rec loop html =
    (Deferred.List.map html ~f:(fun item ->
      if is_import_node item then
        import_node_to_html (ok_exn (parse_import item))
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
| `FAQs
| `Install
]

let make ?(repo_root=".") ~out_dir = function
  | `Frontpage -> (
    let out_file = out_dir/"index.html" in
    make_frontpage ~repo_root () >>= fun html ->
    return (Html.to_string html) >>= fun contents ->
    Writer.save out_file ~contents
  )
  | `Chapter in_file -> (
    let out_file = out_dir/(Filename.basename in_file) in
    chapters ~repo_root () >>= fun chapters ->
    make_chapter repo_root chapters in_file >>= fun html ->
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
