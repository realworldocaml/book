open Tyxml

let this_title = Html.txt "Your Cool Web Page"

let image_box = [%html
  "<div id=image_box></div>"
]

let links_box = [%html {|
  <ul class=links_bar id=links_bar>
    <li id=home_click >My Musings</li>
    <li id=about_click >About Me</li>
    <li id=blog_posts_click >Blog</li>
    <li id=hackathons_click >Hackathons</li>
  </ul>
|}]

let common_footer = [%html {|
  <footer id="footer_box">
    <p>
      This site was made with <a href=http://ocaml.org>OCaml</a> and <a href=https://www.gnu.org/software/emacs/>emacs</a>
    </p>
  </footer>
|}]

let home_content = [%html
  "<div><h2>Hello Coder</h2></div>"
]

let main_payload = [%html
  "<div id=payload>"[home_content]"</div>"
]

let common_nav = Html.nav [links_box]

let content_box = [%html
  "<div id=content_box>"[
    common_nav;
    main_payload;
    common_footer;
  ]"</div>"
]

let main_script = [%html
  "<script src=main.js> </script>"
]

let home_page_doc = [%html
  {|<html>
    <head>
     <title>|}this_title{|</title>
     <link rel=stylesheet href="home.css" />
    </head>
    <body>|} [ image_box; content_box; main_script ] {|</body>
    </html>
|}]

(** The set of pages in your website. *)
let pages = [("index.html", home_page_doc)]

(** Small code to emit all the pages. *)
let emit_page (name, page) =
  Printf.printf "Generating: %s\n" name ;
  let file_handle = open_out name in
  let fmt = Format.formatter_of_out_channel file_handle in
  Format.fprintf fmt "%a@." (Html.pp ~indent:true ()) page;
  close_out file_handle

let () = List.iter emit_page pages
