open Tyxml;

let this_title = "Your Cool Web Page";

let image_box = <div id="image_box" />;

let links_box =
  <ul className="links_bar" id="links_bar">
    <li id="home_click"> "My Musings" </li>
    <li id="about_click"> "About Me" </li>
    <li id="blog_posts_click"> "Blog" </li>
    <li id="hackathons_click"> "Hackathons" </li>
  </ul>;

let common_footer =
  <footer id="footer_box">
    <p>
      "This site was made with"
      <a href="http://ocaml.org"> "OCaml" </a>
      "and"
      <a href="https://www.gnu.org/software/emacs/"> "emacs" </a>
    </p>
  </footer>;

let home_content = <div> <h2> "Hello Coder" </h2> </div>;

let main_payload = <div id="payload"> home_content </div>;

let common_nav = Html.nav([links_box]);

let content_box =
  <div id="content_box"> common_nav main_payload common_footer </div>;

let main_script = <script src="main.js"></script>;

let home_page_doc =
  <Home_page title=this_title>
    image_box content_box main_script
  </Home_page>;

// The set of pages in your website.
let pages = [("index.html", home_page_doc)];

// Small code to emit all the pages.
let emit_page = ((name, page)) => {
  Printf.printf("Generating: %s\n", name);
  let file_handle = open_out(name);
  let fmt = Format.formatter_of_out_channel(file_handle);
  Format.fprintf(fmt, "%a@.", Html.pp(~indent=true, ()), page);
  close_out(file_handle);
};

let () = List.iter(emit_page, pages);
