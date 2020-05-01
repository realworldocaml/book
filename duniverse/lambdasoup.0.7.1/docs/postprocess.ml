open Soup

let fmt = Printf.sprintf

let read_file =
  let directory = "docs" in
  fun filename -> filename |> Filename.concat directory |> read_file

let () =
  (* Read ocamldoc output from STDIN. *)
  let soup = read_channel stdin |> parse in

  (* Replace the <h1> element with a new header from header.html. *)
  read_file "header.html" |> parse |> replace (soup $ "h1");

  (* Remove the nav bar, horizontal rule, and some line breaks. *)
  soup $ ".navbar" |> delete;
  soup $ "hr" |> delete;
  soup $$ "body > br" |> iter delete;

  (* Remove unnecessary links to self and the (hopefully) obvious module R.
     Replace them with their content. *)
  soup $$ "a:contains(\"..\")" |> iter unwrap;
  soup $ "a:contains(\"R\")" |> unwrap;
  soup $ "a:contains(\"Infix\")" |> unwrap;

  (* Add a footer to the body from footer.html. *)
  read_file "footer.html" |> parse |> append_child (soup $ "body");

  (* Generate a table of contents after the module information text. For wide
     (desktop) screens, CSS will move this to the top-left corner in fixed
     position. *)
  let table_of_contents =
    (* List all the sections - get their ids and labels. *)
    let sections =
      soup $$ "h2"
      |> to_list
      |> List.map (fun h2 -> R.id h2, R.leaf_text h2)
    in

    (* Create a div to hold the entire table of contents. This is the element
       that is conditionally positioned. *)
    let div = create_element ~class_:"toc" "div" in

    (* Give the TOC a heading. This is only displayed at narrow widths. *)
    create_element ~inner_text:"Module contents" "p" |> append_child div;

    (* Generate a nested div to hold only the links. This element has a
       multi-column style applied to it on narrow displays. *)
    let links = create_element ~class_:"links" "div" in
    append_child div links;

    (* Iterate over the pairs of section id/section label. Add an anchor to the
       div just created for each section. Include a [Top] link first. *)
    ("", "[Top]")::sections |> List.iter (fun (id, title) ->
      create_element ~attributes:["href", "#" ^ id] ~inner_text:title "a"
      |> append_child links;
      create_element "br" |> append_child links);

    (* Separate the [Top] link from the rest by a line break. *)
    create_element "br" |> insert_after (div $ "a");

    (* Add some blank lines before the GitHub link. *)
    create_element "br" |> append_child div;
    create_element "br" |> append_child div;

    (* Add the GitHub link at the bottom of the table of contents. *)
    create_element
      ~attributes:["href", "https://github.com/aantron/lambdasoup"]
      ~classes:["github"; "hide-narrow"] ~inner_text:"GitHub"
      "a"
    |> append_child div;

    (* Hide the [Top] link if the display gets narrow. Since the table of
       contents becomes statically (normally) positioned at narrow widths, the
       top link will scroll off screen when scrolling away from the top, and
       thus become useless for returning to the top. *)
    div $ "a" |> set_attribute "class" "hide-narrow";

    (* Finally, evaluate to the TOC container div. *)
    div
  in

  (* Place the table of contents at the end of the module description. *)
  append_child (soup $$ ".info" |> R.nth 2) table_of_contents;

  (* Find every multi-line signature member declaration, and add a class to its
     info block. This class allow special styling with CSS, such as a wider top
     margin. *)
  soup $$ "pre"
  |> filter (fun e -> e $? ".type" <> None)
  |> filter (fun e -> e $? "br" <> None)
  |> iter (fun e -> e $ "+ .info" |> add_class "multiline-member");

  (* Find every section that has additional text after the title, and add a
     class to the last element of such text. This is used by CSS to increase
     spacing. *)
  soup $$ "h2" |> iter (fun h2 ->
    let e = h2 $ "~ pre:not(.codepre)" |> R.previous_element in
    if name e = "h2" then ()
    else add_class "end-of-section-text" e);

  (* Clean up links in the head. *)
  soup $$ "head link:not([rel=stylesheet])" |> iter delete;

  (* Replace the title tag with a bunch of metadata from file. *)
  read_file "meta.html" |> parse |> replace (soup $ "title");

  (* Fix up internal cross-references by dropping the module prefix, and
     correcting the destination. *)
  soup $$ "a[href^=Soup.html#]"
  |> iter (fun a ->
    let href = a |> R.attribute "href" in
    String.sub href 9 (String.length href - 9)
    |> fun v -> set_attribute "href" v a;

    let text = texts a |> String.concat "" in
    let starts_with_module =
      try String.sub text 0 5 = "Soup."
      with Invalid_argument _ -> false
    in

    if starts_with_module then
      let value_name = String.sub text 5 (String.length text - 5) in
      clear (R.child a);
      create_text value_name |> append_child (a |> R.child_element));

  (* Insert clickable anchors. *)
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
    append_child h2 a);

  soup $$ "h2 ~ pre > span.keyword:contains('module')" |> iter (fun span ->
    let pre = R.parent span in
    let name = R.nth 3 (children pre) |> R.leaf_text |> String.trim in
    let anchor = fmt "MODULE%s" name in
    let replacement =
      fmt
        "<a href='#%s' id='%s'><span class='keyword'>module</span> %s</a> : %s"
        anchor anchor name
        ("<code class='code'><span class='keyword'>sig</span></code>" ^
         " .. " ^
         "<code class='code'><span class='keyword'>end</span></code>")
    in
    clear pre;
    parse replacement |> children |> iter (append_child pre));

  (* Convert the soup back to HTML and write to STDOUT. The Makefile redirects
     that to the right output file. *)
  soup |> to_string |> write_channel stdout
