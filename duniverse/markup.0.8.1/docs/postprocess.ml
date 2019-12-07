(* This file is part of Markup.ml, released under the BSD 2-clause license. See
   doc/LICENSE for details, or visit https://github.com/aantron/markup.ml. *)

open Soup

let (|>) x f = f x

type transform =
  | Rename of string
  | TableOfContents
  | UpTo of string
  | Class of string
  | WithType of string
  | Meta of (string * string option)

let transforms =
  ["Markup.html",
    [Rename "index.html"; TableOfContents; Class "index";
     Meta ("Markup.ml - Error-recovering HTML and XML parsers for OCaml",
           Some ("Streaming, error-recovering, standards-based HTML(5) and " ^
                 "XML parsers with an interface designed for ease of use."))];

   "Markup.Error.html",
     [UpTo "index.html"; Meta ("Error - Markup.ml", None)];

   "Markup.Encoding.html",
     [UpTo "index.html"; Meta ("Encoding - Markup.ml", None)];

   "Markup.Ns.html",
     [UpTo "index.html"; Meta ("Ns - Markup.ml", None)];

   "Markup.ASYNCHRONOUS.html",
     [UpTo "index.html"; Class "asynchronous";
      Meta ("ASYNCHRONOUS - Markup.ml", None)];

   "Markup_lwt.html",
     [UpTo "index.html"; Class "asynchronous"; WithType "Lwt";
      Meta ("Markup_lwt - Markup.ml", None)];

   "Markup_lwt_unix.html",
     [UpTo "index.html"; Meta ("Markup_lwt_unix - Markup.ml", None)];

   "Markup.ASYNCHRONOUS.Encoding.html",
     [UpTo "Markup.ASYNCHRONOUS.html";
      Meta ("ASYNCHRONOUS.Encoding - Markup.ml", None)]]

let rec find_map f = function
  | [] -> None
  | x::l ->
    match f x with
    | None -> find_map f l
    | Some _ as v -> v

let lookup f file =
  try transforms |> List.assoc file |> find_map f
  with Not_found -> None

let should_rename file =
  lookup (function
    | Rename name -> Some name
    | _ -> None)
    file

let new_name file =
  match should_rename file with
  | None -> file
  | Some name -> name

let should_make_toc file =
  try
    transforms
    |> List.assoc file
    |> List.mem TableOfContents
  with Not_found -> false

let should_make_up file =
  lookup (function
    | UpTo name -> Some name
    | _ -> None)
    file

let should_add_class file =
  lookup (function
    | Class name -> Some name
    | _ -> None)
    file

let should_add_with_type file =
  lookup (function
    | WithType name -> Some name
    | _ -> None)
    file

let html_directory = "doc/html"
let read_output_file name = Filename.concat html_directory name |> read_file
let write_output_file name text =
  write_file (Filename.concat html_directory name) text

let read_fragment name = Filename.concat "doc" name |> read_file

let clean_up_head soup name =
  soup $$ "head link:not([rel=stylesheet])" |> iter delete;

  let address = "http://aantron.github.io/markup.ml" in

  let canonical =
    match new_name name with
    | "index.html" -> address
    | name -> address ^ "/" ^ name
  in

  let title, description =
    let result =
      lookup (function
        | Meta v -> Some v
        | _ -> None)
        name
    in
    match result with
    | None -> failwith ("no metadata for " ^ name)
    | Some v -> v
  in

  let meta_content =
    "<title>" ^ title ^ "</title>\n<link rel='canonical' href='" ^
    canonical ^ "'>\n" ^ "<meta name='author' content='Anton Bachin'>\n" ^
    "<meta name='viewport' content='width=device-width'>"
  in

  let meta_content =
    match description with
    | None -> meta_content
    | Some text ->
      meta_content ^ "\n<meta name='description' content='" ^ text ^ "'>"
  in

  soup $ "title" |> delete;
  meta_content |> parse |> children |> iter (append_child (soup $ "head"))

let clean_up_header soup =
  soup $ ".navbar" |> delete;
  soup $ "hr" |> delete;
  read_fragment "header.html" |> parse |> replace (soup $ "h1");
  read_fragment "footer.html" |> parse |> append_child (soup $ "body")

let clean_up_content soup =
  soup $$ "body > br" |> iter delete;
  soup $$ "a:contains(\"..\")" |> iter unwrap;

  begin match soup $? "table.indextable" with
  | None -> ()
  | Some table ->
    table |> R.previous_element |> delete
  end;

  soup $$ "a[href]" |> iter (fun a ->
    let link = R.attribute "href" a in
    let prefix = "Markup.html" in
    if String.length link >= String.length prefix &&
       String.sub link 0 (String.length prefix) = prefix then
      let suffix =
        String.sub link (String.length prefix)
          (String.length link - String.length prefix)
      in
      set_attribute "href" ("index.html" ^ suffix) a);

  soup $$ "a:not(.protect):contains(\"Markup.\")" |> iter (fun a ->
    match a $? ".constructor" with
    | None ->
      let text = R.leaf_text a in
      let prefix = "Markup." in
      let text =
        String.sub text (String.length prefix)
          (String.length text - String.length prefix)
      in
      clear a;
      create_text text |> append_child a

    | Some element ->
      delete element;
      let inner_html =
        a $ ".code" |> children |> fold (fun s n -> s ^ (to_string n)) "" in
      let inner_html = String.sub inner_html 1 (String.length inner_html - 1) in
      a $ ".code" |> clear;
      inner_html |> parse |> children |> iter (append_child (a $ ".code")));

  soup $$ "pre"
  |> filter (fun e -> e $? ".type" <> None)
  |> filter (fun e -> e $? "br" <> None)
  |> filter (fun e -> e $? "+ .info" <> None)
  |> iter (fun e -> e $ "+ .info" |> add_class "multiline-member");

  let rec reassemble_lists () =
    match soup $? "ul + ul" with
    | None -> ()
    | Some ul ->
      let ul = R.previous_element ul in
      let rec consume () =
        match ul $? "+ ul" with
        | None -> ()
        | Some ul' ->
          R.child_element ul' |> append_child ul;
          delete ul';
          consume ()
      in
      consume ();
      reassemble_lists ()
  in
  reassemble_lists ();

  soup $$ "ul" |> iter (fun ul -> ul |> R.previous_element |> delete);

  soup $$ "pre > .type"
  |> filter (fun e -> e $? "br" <> None)
  |> iter (fun e ->
    create_text "       " |> prepend_child e;
    create_element "br" |> prepend_child e);

  let uncolor class_ content =
    soup $$ ("span." ^ class_)
    |> filter at_most_one_child
    |> filter (fun e -> leaf_text e = Some content)
    |> iter unwrap
  in

  uncolor "constructor" "Error";
  uncolor "constructor" "Encoding";
  uncolor "constructor" "Markup";
  uncolor "constructor" "Markup_lwt";
  uncolor "constructor" "Markup_lwt_unix";
  uncolor "constructor" "Markup_async";
  uncolor "constructor" "ASYNCHRONOUS";
  uncolor "constructor" "Pervasives";
  uncolor "constructor" "Lwt_io";
  uncolor "keyword" "false";
  uncolor "keyword" "parser";

  soup $$ "span[id]" |> iter (fun span ->
    set_name "a" span;
    set_attribute "href" ("#" ^ (R.attribute "id" span)) span);

  soup $$ "h2[id]" |> iter (fun h2 ->
    let href = "#" ^ (R.attribute "id" h2) in
    let a =
      create_element
        ~attributes:["href", href] ~inner_text:(R.leaf_text h2) "a";
    in
    clear h2;
    append_child h2 a)

let add_with_type soup type_name =
  let extra =
    " <span class='keyword'>with type</span> 'a io = 'a " ^
    "<span class='constructor'>" ^ type_name ^ "</span>.t"
  in

  parse extra |> children
  |> iter (append_child (soup $ "pre:contains(\"ASYNCHRONOUS\")"))

let add_table_of_contents soup =
  let sections =
    soup $$ "h2"
    |> to_list
    |> List.map (fun h2 -> R.id h2, R.leaf_text h2)
  in

  let toc = create_element ~class_:"toc" "div" in
  create_element ~inner_text:"Module contents" "p" |> append_child toc;
  let links = create_element ~class_:"links" "div" in
  append_child toc links;

  ("", "[Top]")::sections |> List.iter (fun (id, title) ->
    create_element ~attributes:["href", "#" ^ id] ~inner_text:title "a"
    |> append_child links;
    create_element "br" |> append_child links);

  create_element "br" |> insert_after (toc $ "a");

  create_element "br" |> append_child toc;
  create_element "br" |> append_child toc;

  create_element
    ~attributes:["href", "https://github.com/aantron/markup.ml"]
    ~classes:["github"; "hide-narrow"] ~inner_text:"GitHub"
    "a"
  |> append_child toc;

  toc $ "a" |> set_attribute "class" "hide-narrow";

  append_child (soup $ ".info") toc

let add_up_link soup to_ =
  let toc =
    match soup $? ".toc" with
    | Some element -> element
    | None ->
      let toc = create_element ~class_:"toc" "div" in
      append_child (soup $ ".info") toc;
      toc
  in

  let container = create_element ~class_:"hide-narrow" "div" in
  create_element ~inner_text:"[Up]" ~attributes:["href", to_] "a"
  |> append_child container;
  create_element "br" |> append_child container;
  create_element "br" |> append_child container;

  container |> prepend_child toc

let () =
  html_directory
  |> Sys.readdir
  |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".html")
  |> List.iter begin fun file ->
    let soup = file |> read_output_file |> parse in

    clean_up_head soup file;

    clean_up_header soup;
    clean_up_content soup;

    begin match should_add_with_type file with
    | None -> ()
    | Some type_name -> add_with_type soup type_name
    end;

    if should_make_toc file then
      add_table_of_contents soup;

    begin match should_make_up file with
    | None -> ()
    | Some target -> add_up_link soup target
    end;

    begin match should_add_class file with
    | None -> ()
    | Some class_ -> soup $ "body" |> add_class class_
    end;

    begin match should_rename file with
    | None -> soup |> to_string |> write_output_file file
    | Some name ->
      Sys.remove (Filename.concat html_directory file);
      soup |> to_string |> write_output_file name
    end
  end
