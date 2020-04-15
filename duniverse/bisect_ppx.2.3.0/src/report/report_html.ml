(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



module Common = Bisect_common

type theme = [
  | `Light
  | `Dark
  | `Auto
]

let theme_class = function
  | `Light -> " class=\"light\""
  | `Dark -> " class=\"dark\""
  | `Auto -> ""

let output_file content filename =
  Common.try_out_channel
    false
    filename
    (fun channel -> output_string channel content)

let split_filename name =
  let dirname =
    match Filename.dirname name with
    | "" -> ""
    | dir when dir = Filename.current_dir_name -> ""
    | dir -> dir ^ Filename.dir_sep
  in
  let basename = Filename.basename name in
  dirname, basename

let percentage stats =
  let a, b = Report_utils.(stats.visited, stats.total) in
  let a, b = float_of_int a, float_of_int b in
  if b = 0. then 100. else (100. *. a) /. b

let output_html_index verbose title theme filename l =
  verbose "Writing index file...";

  let stats =
    List.fold_left
      (fun acc (_, _, s) -> Report_utils.add acc s)
      (Report_utils.make ())
      l in

  Common.try_out_channel
    false
    filename
    (fun channel ->
      Report_utils.output_strings
        [  "<!DOCTYPE html>" ;
           "<html lang=\"en\"$(theme)>" ;
           "  <head>" ;
           "    <title>$(title)</title>" ;
           "    <link rel=\"stylesheet\" type=\"text/css\" href=\"coverage.css\" />" ;
           "    <meta charset=\"utf-8\" />" ;
           "  </head>" ;
           "  <body>" ;
           "    <div id=\"header\">" ;
           "      <h1>$(title)</h1>" ;
           "      <h2>$(percentage)%</h2>" ;
           "    </div>" ;
           "    <div id=\"files\">"]
        [ "title", title ;
          "theme", theme_class theme;
          "percentage",
            Printf.sprintf "%.02f"
              (floor ((percentage stats) *. 100.) /. 100.) ]
        channel;

      let per_file (name, html_file, stats) =
        let dirname, basename = split_filename name in
        let relative_html_file =
          if Filename.is_relative html_file then
            html_file
          else
            let prefix_length = String.length Filename.dir_sep in
            String.sub
              html_file prefix_length (String.length html_file - prefix_length)
        in
        Report_utils.output_strings
          ["      <div>";
           "        <span class=\"meter\">";
           "          <span class=\"covered\" style=\"width: $(p)%\"></span>";
           "        </span>";
           "        <span class=\"percentage\">$(p)%</span>";
           "        <a href=\"$(link)\">";
           "          <span class=\"dirname\">$(dir)</span>$(name)";
           "        </a>";
           "      </div>"]
          ["p", Printf.sprintf "%.00f" (floor (percentage stats));
           "link", relative_html_file;
           "dir", dirname;
           "name", basename]
          channel in
      List.iter per_file l;

      Report_utils.output_strings
        [ "    </div>" ;
          "  </body>" ;
          "</html>" ]
        []
        channel)

let escape_line tab_size line offset points =
  let buff = Buffer.create (String.length line) in
  let ofs = ref offset in
  let pts = ref points in

  let marker_if_any content =
    match !pts with
    | (o, n) :: tl when o = !ofs ->
        Printf.bprintf buff "<span data-count=\"%i\">%s</span>" n content;
        pts := tl
    | _ -> Buffer.add_string buff content in
  String.iter
    (fun ch ->
      let s =
        match ch with
        | '<' -> "&lt;"
        | '>' -> "&gt;"
        | '&' -> "&amp;"
        | '\t' -> String.make tab_size ' '
        | c -> Printf.sprintf "%c" c
      in
      marker_if_any s;
      incr ofs)
    line;
  Buffer.contents buff

let output_html
    verbose tab_size title theme in_file out_file resolver visited points =

  verbose (Printf.sprintf "Processing file '%s'..." in_file);
  match resolver in_file with
  | None ->
    verbose "... file not found";
    None
  | Some resolved_in_file ->
    let cmp_content = Hashtbl.find points in_file |> Common.read_points in
    verbose (Printf.sprintf "... file has %d points" (List.length cmp_content));
    let len = Array.length visited in
    let stats = Report_utils.make () in
    let pts = ref (List.map
                     (fun p ->
                       let nb =
                         if Common.(p.identifier) < len then
                           visited.(Common.(p.identifier))
                         else
                           0 in
                       Report_utils.update stats (nb > 0);
                       (Common.(p.offset), nb))
                     cmp_content) in
    let dirname, basename = split_filename in_file in
    let in_channel, out_channel =
      Report_utils.open_both resolved_in_file out_file in
    let rec make_path_to_report_root acc in_file_path_remaining =
      if in_file_path_remaining = "" ||
         in_file_path_remaining = Filename.current_dir_name ||
         in_file_path_remaining = Filename.dir_sep then
        acc
      else
        let path_component = Filename.basename in_file_path_remaining in
        let parent = Filename.dirname in_file_path_remaining in
        if path_component = Filename.current_dir_name then
          make_path_to_report_root acc parent
        else
          make_path_to_report_root
            (Filename.concat acc Filename.parent_dir_name)
            parent
    in
    let path_to_report_root =
      make_path_to_report_root "" (Filename.dirname in_file) in
    let style_css = Filename.concat path_to_report_root "coverage.css" in
    let coverage_js = Filename.concat path_to_report_root "coverage.js" in
    let highlight_js =
      Filename.concat path_to_report_root "highlight.pack.js" in
    let index_html = Filename.concat path_to_report_root "index.html" in
    (try
      let lines, line_count =
        let rec read number acc =
          let start_ofs = pos_in in_channel in
          try
            let line = input_line in_channel in
            let end_ofs = pos_in in_channel in
            let before, after =
              Report_utils.split (fun (o, _) -> o < end_ofs) !pts in
            pts := after;
            let line' = escape_line tab_size line start_ofs before in
            let visited, unvisited =
              List.fold_left
                (fun (v, u) (_, nb) ->
                  ((v || (nb > 0)), (u || (nb = 0))))
                (false, false)
                before
            in
            read (number + 1) ((number, line', visited, unvisited)::acc)

          with End_of_file -> List.rev acc, number - 1
        in
        read 1 []
      in

      let class_of_visited = function
        | true, false -> "class=\"visited\""
        | false, true -> "class=\"unvisited\""
        | true, true -> "class=\"some-visited\""
        | false, false -> ""
      in

      (* Head and header. *)
      Report_utils.output_strings
        [  "<!DOCTYPE html>" ;
           "<html lang=\"en\"$(theme)>" ;
           "  <head>" ;
           "    <title>$(title)</title>" ;
           "    <link rel=\"stylesheet\" href=\"$(style_css)\" />" ;
           "    <script src=\"$(highlight_js)\"></script>";
           "    <script>hljs.initHighlightingOnLoad();</script>";
           "    <meta charset=\"utf-8\" />" ;
           "  </head>" ;
           "  <body>" ;
           "    <div id=\"header\">" ;
           "      <h1>" ;
           "        <a href=\"$(index_html)\">" ;
           "          <span class=\"dirname\">$(dir)</span>$(name)" ;
           "        </a>" ;
           "      </h1>" ;
           "      <h2>$(percentage)%</h2>" ;
           "    </div>" ;
           "    <div id=\"navbar\">" ; ]
        [ "dir", dirname ;
          "name", basename ;
          "title", title ;
          "theme", theme_class theme;
          "percentage", Printf.sprintf "%.02f" (percentage stats);
          "style_css", style_css;
          "highlight_js", highlight_js;
          "index_html", index_html ]
        out_channel;

      (* Navigation bar items. *)
      lines |> List.iter (fun (number, _, visited, unvisited) ->
        if unvisited then begin
          let offset =
            (float_of_int number) /. (float_of_int line_count) *. 100. in
          let origin, offset =
            if offset <= 50. then
              "top", offset
            else
              "bottom", (100. -. offset)
          in
          Report_utils.output_strings
            ["      <span $(visited) style=\"$(origin):$(offset)%\"></span>"]
            ["visited", class_of_visited (visited, unvisited);
             "origin", origin;
             "offset", Printf.sprintf "%.02f" offset;
             "n", string_of_int number]
            out_channel
        end);

      Report_utils.output_strings
        ["    </div>";
         "    <div id=\"report\">";
         "      <div id=\"lines-layer\">";
         "        <pre>"]
        []
        out_channel;

      (* Line highlights. *)
      lines |> List.iter (fun (number, _, visited, unvisited) ->
        Report_utils.output_strings
          ["<a id=\"L$(n)\"></a><span $(visited)> </span>"]
          ["n", string_of_int number;
           "visited", class_of_visited (visited, unvisited)]
          out_channel);

      Report_utils.output_strings
        ["</pre>";
         "      </div>";
         "      <div id=\"text-layer\">";
         "        <pre id=\"line-numbers\">"]
        []
        out_channel;

      let width = string_of_int line_count |> String.length in

      (* Line numbers. *)
      lines |> List.iter (fun (number, _, _, _) ->
        let formatted = string_of_int number in
        let padded =
          (String.make (width - String.length formatted) ' ') ^  formatted in

        Report_utils.output_strings
          ["<a href=\"#L$(n)\">$(padded)</a>"]
          ["n", formatted;
           "padded", padded]
          out_channel);

      let syntax =
        if Filename.check_suffix basename ".re" then
          "reasonml"
        else
          "ocaml"
      in

      output_string out_channel "</pre>\n";
      Printf.fprintf out_channel "<pre><code class=\"%s\">" syntax;

      (* Code lines. *)
      lines |> List.iter (fun (_, markup, _, _) ->
        output_string out_channel markup;
        output_char out_channel '\n');

      Report_utils.output_strings
        ["</code></pre>";
         "      </div>";
         "    </div>";
         "    <script src=\"$(coverage_js)\"></script>";
         "  </body>";
         "</html>"]
        ["coverage_js", coverage_js]
        out_channel

    with e ->
      close_in_noerr in_channel;
      close_out_noerr out_channel;
      raise e);

    close_in_noerr in_channel;
    close_out_noerr out_channel;
    Some stats

let output verbose dir tab_size title theme resolver data points =
  let files =
    Hashtbl.fold
      (fun in_file visited acc ->
        let out_file = (Filename.concat dir in_file) ^ ".html" in
        let maybe_stats =
          output_html verbose tab_size title theme in_file out_file resolver
            visited points
        in
        match maybe_stats with
        | None -> acc
        | Some stats -> (in_file, (in_file ^ ".html"), stats) :: acc)
      data
      [] in
  output_html_index
    verbose title theme
    (Filename.concat dir "index.html") (List.sort compare files);
  output_file Assets.js (Filename.concat dir "coverage.js");
  output_file Assets.highlight_js (Filename.concat dir "highlight.pack.js");
  output_file Assets.css (Filename.concat dir "coverage.css")
