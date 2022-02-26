(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



let theme_class = function
  | `Light -> {| class="light"|}
  | `Dark -> {| class="dark"|}
  | `Auto -> ""

let split_filename name =
  let dirname =
    match Filename.dirname name with
    | "" -> ""
    | dir when dir = Filename.current_dir_name -> ""
    | dir -> dir ^ Filename.dir_sep
  in
  let basename = Filename.basename name in
  dirname, basename

let percentage (visited, total) =
  if total = 0 then
    100.
  else
    100. *. (float_of_int visited) /. (float_of_int total)

type index_file = (string * string * (int * int))

type index_element =
  | File of index_file
  | Directory of (string * index_element list * (int * int))

let output_html_index ~tree title theme filename files =
  Util.info "Writing index file...";

  let add_stats (visited, total) (visited', total') =
    (visited + visited', total + total') in

  let sum_stats files =
    List.fold_left
      (fun stats (_, _, stats') -> add_stats stats stats')
      (0, 0)
      files
  in

  (*
    [subdirectory_of dir ~directory file] returns [Some subdirectory] if file is
    a contained in the subdirectory [subdirectory] of [directory].

    If [file] is not contained in [directory] or if [file] is
    contained directly in [directory] then [None] is returned.
   *)
  let subdirectory_of ~directory file =
    let len_file = String.length file in
    let len_directory = String.length directory in
    if len_file < len_directory then None
    else if (String.sub file 0 len_directory <> directory) then None
    else
      let lfind_string_opt ~needle haystack =
        let rec aux i =
          if String.(length haystack - i < length needle) then None
          else if String.(sub haystack i (length needle)) = needle then Some i
          else aux (i+1)
        in aux 0
      in
      let file_rel = String.sub file len_directory (len_file - len_directory) in
      match lfind_string_opt ~needle:Filename.dir_sep file_rel with
      | Some i -> Some (String.sub file_rel 0 i)
      | None -> None
  in

  (* [partition_files ~directory files] returns two lists:

       - the first is a list of pairs [(sub_directory, sub_files)],
         each corresponding to a [sub_directory] of [~directory] and the [sub_files]
         contained (recursively) therein
       - the second is a list of files, corresponding to the set of immediate sub-files of
         [~directory]

     This function assumes that [files] are sorted lexicographically.
   *)
  let partition_files ~directory files =
    let immediate_child_of ~directory name =
      let directory', _ = split_filename name in
      directory' = directory
    in
    let rec aux sub_dirs =
      function
        [] -> (sub_dirs, [])
      | (name, _, _) as file :: files ->
         match subdirectory_of ~directory name with
         | None ->
            let (sub_files, _) =
              Util.split
                (fun (name, _, _) -> immediate_child_of ~directory name)
                (file :: files)
            in
            (sub_dirs, sub_files)
         | Some root ->
            let sub_dir, files' =
              Util.split
                (fun (name, _, _) -> subdirectory_of ~directory name = Some root)
                (file :: files) in
            aux ((root, sub_dir) :: sub_dirs) files'
    in aux [] files
  in

  let collate : index_file list -> index_element list * (int * int) =
    fun files ->
    let rec collate_aux :
              string -> index_file list ->
              (index_element list * (int * int)) =
      fun directory files ->
      let (sub_dirs, sub_files) = partition_files ~directory files in
      let (dir_elements, dir_stats) =
        List.fold_left
          (fun (lines, stats) (sub_dir, files)  ->
            let directory = directory ^ sub_dir ^ "/" in
            let (sub_dir_elements, sub_dir_stats) = collate_aux directory files in
            let sub_dir_element = Directory (directory, sub_dir_elements, sub_dir_stats) in
            (sub_dir_element :: lines, add_stats stats sub_dir_stats))
          ([], (0, 0)) sub_dirs in
      let sub_files_element = List.map (fun file -> File file) sub_files in
      let dir_stats = add_stats dir_stats (sum_stats sub_files) in
      ((List.rev dir_elements) @ sub_files_element, dir_stats)
    in
    collate_aux "" files
  in

  let channel =
    try open_out filename
    with Sys_error message ->
      Util.fatal "cannot open output file '%s': %s" filename message
  in
  try
    let write format = Printf.fprintf channel format in

    let (files, stats) = collate files in

    let overall_coverage =
      Printf.sprintf "%.02f%%" (floor ((percentage stats) *. 100.) /. 100.) in
    write {|<!DOCTYPE html>
<html lang="en"%s>
  <head>
    <meta charset="utf-8"/>
    <title>%s</title>
    <meta name="description" content="%s coverage overall"/>
    <link rel="stylesheet" type="text/css" href="coverage.css"/>
  </head>
  <body>
    <div id="header">
      <h1>%s</h1>
      <h2>%s</h2>
    </div>
    <div id="files">
|}
      (theme_class theme)
      title
      overall_coverage
      title
      overall_coverage;

    let rec print_line ~tree =
      let write_meter ((visited, total) as stats) =
        let percentage = Printf.sprintf "%.00f" (floor (percentage stats)) in
        write {|        <span class="meter">
          <span class="covered" style="width: %s%%"></span>
        </span>
        <span class="percentage">%s%% <span class="stats">(%d / %d)</span></span>
|}
          percentage
          percentage
          visited total;
      in
      function
      | File (name, html_file, stats) ->
         write {|      <div>
|};
         if tree then
           write {|        <span class="summary-indicator"></span>
|};
         write_meter stats ;
         let dirname, basename = split_filename name in
         let relative_html_file =
           if Filename.is_relative html_file then
             html_file
           else
             let prefix_length = String.length Filename.dir_sep in
             String.sub
               html_file prefix_length (String.length html_file - prefix_length)
         in
         write {|        <a href="%s">
          <span class="dirname">%s</span>%s
        </a>
      </div>
|}
           relative_html_file
           dirname basename;
      | Directory (name, files, stats) when tree ->
         write {|      <details open="">
        <summary>
        <span class="summary-indicator"></span>
        <div class="directory">
|};
         write_meter stats ;
         write {|        <span class="dirname">%s</span>
        </div>
        </summary>
|}
           name;
         List.iter (print_line ~tree) files;
         write {|      </details>
|} ;
      | Directory (_, files, _) ->
         List.iter (print_line ~tree) files;
    in

    List.iter (print_line ~tree) files;

    write {|    </div>
|};
    if tree then
      write {|    <script src="coverage.js"></script>
|};
    write {|  </body>
</html>
|};

    close_out channel

  with
  | Sys_error message ->
    Util.fatal "cannot write output file '%s': %s" filename message
  | exn ->
    close_out_noerr channel;
    raise exn



let escape_line tab_size line offset points =
  let buff = Buffer.create (String.length line) in
  let ofs = ref offset in
  let pts = ref points in

  let marker_if_any content =
    match !pts with
    | (o, n)::tl when o = !ofs ->
      Printf.bprintf buff {|<span data-count="%i">%s</span>|} n content;
      pts := tl
    | _ ->
      Buffer.add_string buff content
  in
  line
  |> String.iter
    begin fun ch ->
      let s =
        match ch with
        | '<' -> "&lt;"
        | '>' -> "&gt;"
        | '&' -> "&amp;"
        | '\t' -> String.make tab_size ' '
        | c -> Printf.sprintf "%c" c
      in
      marker_if_any s;
      incr ofs
    end;
  Buffer.contents buff



(* Individual HTML files corresponding to each source file. *)

let output_for_source_file
    tab_size title theme source_file_on_disk html_file_on_disk
    {Bisect_common.filename; points; counts} =

  let len = Array.length counts in
  let stats = ref (0, 0) in
  let points =
    points
    |> Array.to_list
    |> List.mapi (fun index offset -> (offset, index))
    |> List.sort compare
  in
  let pts =
    ref (points |> List.map (fun (offset, index) ->
      let nb =
        if index < len then
          counts.(index)
        else
          0
      in
      let visited, total = !stats in
      let visited =
        if nb > 0 then
          visited + 1
        else
          visited
      in
      stats := (visited, total + 1);
      (offset, nb)))
  in
  let dirname, basename = split_filename filename in
  Util.mkdirs (Filename.dirname html_file_on_disk);
  let in_channel =
    try open_in source_file_on_disk
    with Sys_error message ->
      Util.fatal "cannot open source file '%s': %s" source_file_on_disk message
  in
  let out_channel =
    try open_out html_file_on_disk
    with Sys_error message ->
      Util.fatal "cannot open output file '%s': %s" html_file_on_disk message
  in
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
    make_path_to_report_root "" (Filename.dirname filename) in
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
          let before, after = Util.split (fun (o, _) -> o < end_ofs) !pts in
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
      | true, false -> {|class="visited"|}
      | false, true -> {|class="unvisited"|}
      | true, true -> {|class="some-visited"|}
      | false, false -> ""
    in

    let write format = Printf.fprintf out_channel format in

    (* Head and header. *)
    let file_coverage = Printf.sprintf "%.02f%%" (percentage !stats) in
    write {|<!DOCTYPE html>
<html lang="en"%s>
  <head>
    <meta charset="utf-8"/>
    <title>%s &mdash; %s</title>
    <meta name="description" content="%s coverage in %s">
    <link rel="stylesheet" href="%s"/>
    <script src="%s"></script>
    <script>hljs.initHighlightingOnLoad();</script>
  </head>
  <body>
    <div id="header">
      <h1>
        <a href="%s">
          <span class="dirname">%s</span>%s
        </a>
      </h1>
      <h2>%s</h2>
    </div>
    <div id="navbar">
|}
      (theme_class theme)
      basename
      title
      file_coverage
      filename
      style_css
      highlight_js
      index_html
      dirname basename
      file_coverage;

    (* Navigation bar items. *)
    lines |> List.iter begin fun (number, _, visited, unvisited) ->
      if unvisited then begin
        let offset =
          (float_of_int number) /. (float_of_int line_count) *. 100. in
        let origin, offset =
          if offset <= 50. then
            "top", offset
          else
            "bottom", (100. -. offset)
        in
        write "      <span %s style=\"%s:%.02f%%\"></span>\n"
          (class_of_visited (visited, unvisited)) origin offset;
      end
    end;

    write {|    </div>
    <div id="report">
      <div id="lines-layer">
        <pre>
|};

    (* Line highlights. *)
    lines |> List.iter (fun (number, _, visited, unvisited) ->
      write "<a id=\"L%i\"></a><span %s> </span>\n"
        number
        (class_of_visited (visited, unvisited)));

    write {|</pre>
      </div>
      <div id="text-layer">
        <pre id="line-numbers">
|};

    let width = string_of_int line_count |> String.length in

    (* Line numbers. *)
    lines |> List.iter (fun (number, _, _, _) ->
      let formatted = string_of_int number in
      let padded =
        (String.make (width - String.length formatted) ' ') ^ formatted in
      write "<a href=\"#L%s\">%s</a>\n" formatted padded);

    let syntax =
      if Filename.check_suffix basename ".re" then
        "reasonml"
      else
        "ocaml"
    in

    write "</pre>\n";
    write "<pre><code class=\"%s\">" syntax;

    (* Code lines. *)
    lines |> List.iter (fun (_, markup, _, _) -> write "%s\n" markup);

    write {|</code></pre>
      </div>
    </div>
    <script src="%s"></script>
  </body>
</html>
|}
      coverage_js

  with e ->
    close_in_noerr in_channel;
    close_out_noerr out_channel;
    raise e);

  close_in_noerr in_channel;
  close_out_noerr out_channel;
  !stats



(* Assets, such as CSS and JavaScript files. *)

let output_string_to_separate_file content filename =
  let channel =
    try open_out filename
    with Sys_error message ->
      Util.fatal "cannot open output file '%s': %s" filename message
  in
  try
    Printf.fprintf channel "%s" content;
    close_out channel
  with
  | Sys_error message ->
    Util.fatal "cannot write output file '%s': %s" filename message
  | exn ->
    close_out_noerr channel;
    raise exn



(* HTML generator entry point. *)

let output
    ~to_directory ~title ~tab_size ~theme ~coverage_files ~coverage_paths
    ~source_paths ~ignore_missing_files ~expect ~do_not_expect ~tree =

  (* Read all the [.coverage] files and get per-source file visit counts. *)
  let coverage =
    Input.load_coverage
      ~coverage_files ~coverage_paths ~expect ~do_not_expect in

  (* Write each of the HTML files corresponding to each source file. *)
  Util.mkdirs to_directory;
  let files =
    Hashtbl.fold begin fun _ file acc ->
      let filename = Bisect_common.(file.filename) in
      let source_file_on_disk =
        Util.find_source_file
          ~source_roots:source_paths ~ignore_missing_files ~filename in
      match source_file_on_disk with
      | None ->
        acc
      | Some source_file_on_disk ->
        let html_file_on_disk =
          (Filename.concat to_directory filename) ^ ".html" in
        let html_file_relative = filename ^ ".html" in
        let stats =
          output_for_source_file
            tab_size title theme source_file_on_disk html_file_on_disk file in
        (filename, html_file_relative, stats)::acc
    end
    coverage
    []
  in

  (* Write the coverage report landing page. *)
  output_html_index
    ~tree
    title
    theme
    (Filename.concat to_directory "index.html")
    (List.sort compare files);

  (* Write the asset files. *)
  output_string_to_separate_file
    Assets.js
    (Filename.concat to_directory "coverage.js");
  output_string_to_separate_file
    Assets.highlight_js
    (Filename.concat to_directory "highlight.pack.js");
  output_string_to_separate_file
    Assets.css
    (Filename.concat to_directory "coverage.css")
