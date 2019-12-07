open! Core
open! Import

let string_of_color c =
  let module C = Patdiff_format.Color in
  match c with
  | C.Black -> "#000000"
  | C.Red -> "#880000"
  | C.Green -> "#008800"
  | C.Yellow -> "#888800"
  | C.Blue -> "#000088"
  | C.Magenta -> "#880088"
  | C.Cyan -> "#008888"
  | C.White | C.Default -> "#ffffff"
  | C.Gray -> "#c0c0c0"
  | C.Bright_black -> "#c0c0c0"
  | C.Bright_red -> "#FF0000"
  | C.Bright_green -> "#00FF00"
  | C.Bright_yellow -> "#FFFF00"
  | C.Bright_blue -> "#0000FF"
  | C.Bright_magenta -> "#FF00FF"
  | C.Bright_cyan -> "#00FFFF"
  | C.Bright_white -> "#FFFFFF"
  | C.RGB6 { r; g; b } ->
    let percent x = float (x * 100) /. 5.0 in
    sprintf "rgb(%f%%,%f%%,%f%%)" (percent r) (percent g) (percent b)
  | C.Gray24 { level } ->
    let percent = float (level * 100) /. 23.0 in
    sprintf "rgb(%f%%,%f%%,%f%%)" percent percent percent
;;

module Style = struct
  let apply text ~styles =
    let module S = Patdiff_format.Style in
    let start_tags, end_tags =
      List.fold styles ~init:([], []) ~f:(fun (s, e) style ->
        match style with
        | S.Bold -> "<span style=\"font-weight:bold\">" :: s, "</span>" :: e
        | S.Reset -> s, e
        | S.Foreground c
        | S.Fg c ->
          sprintf "<span style=\"color:%s\">" (string_of_color c) :: s, "</span>" :: e
        | S.Background c
        | S.Bg c ->
          ( sprintf "<span style=\"background-color:%s\">" (string_of_color c) :: s
          , "</span>" :: e )
        | S.Underline | S.Emph -> "<u>" :: s, "</u>" :: e
        | S.Blink -> "<span style=\"text-decoration:blink\">" :: s, "</span>" :: e
        | S.Inverse -> s, e
        | S.Hide -> "<!-- " :: s, " -->" :: e
        | S.Dim ->
          (* "<span style=\"font-weight:lighter\">"::s, "</span>"::e *)
          ( sprintf
              "<span style=\"color:%s\">"
              (string_of_color Patdiff_format.Color.Gray)
            :: s
          , "</span>" :: e ))
    in
    let lst = start_tags @ [ text ] @ end_tags in
    String.concat ~sep:"" lst
  ;;
end

(* assuming we only insert text in contents and not in attributes, only escaping these
   three characters should be enough. We may want to print differently non printable
   ascii characters too? *)
let html_escape_char = function
  | '<' -> "&lt;"
  | '>' -> "&gt;"
  | '&' -> "&amp;"
  | c -> String.of_char c
;;

let html_escape s = String.concat_map s ~f:html_escape_char

module Rule = struct
  let apply text ~rule ~refined =
    let module R = Patdiff_format.Rule in
    let module A = Patdiff_format.Rule.Annex in
    let apply styles text = Style.apply text ~styles in
    sprintf
      "%s%s%s"
      (apply rule.R.pre.A.styles rule.R.pre.A.text)
      (if refined
       then apply [ Patdiff_format.Style.Reset ] text
       else apply rule.R.styles (html_escape text))
      (apply rule.R.suf.A.styles rule.R.suf.A.text)
  ;;
end

let print_header ~rules ~file_names:(prev_file, next_file) ~print =
  let print_line file rule =
    let get_time s =
      try
        Time.to_string
          (Time.of_span_since_epoch (Time.Span.of_sec (Unix.stat s).Unix.st_mtime))
      with
      | _e -> ""
    in
    let time = get_time file in
    print (Rule.apply (file ^ " " ^ time) ~rule ~refined:false)
  in
  let module Rz = Patdiff_format.Rules in
  print_line prev_file rules.Rz.header_old;
  print_line next_file rules.Rz.header_new
;;

let print ~print_global_header ~file_names ~rules ~print ~location_style hunks =
  print "<pre style=\"font-family:consolas,monospace\">";
  if print_global_header then print_header ~rules ~file_names ~print;
  let f hunk =
    let module Rz = Patdiff_format.Rules in
    Patdiff_format.Location_style.sprint
      location_style
      hunk
      ~prev_filename:(fst file_names)
      ~rule:(Rule.apply ~rule:rules.Rz.hunk ~refined:false)
    |> print;
    let module R = Patience_diff.Range in
    let handle_range = function
      (* Just print the new array elements *)
      | R.Same r ->
        let mr = Array.map r ~f:snd in
        Array.iter mr ~f:print
      | R.Prev r
      | R.Next r
      | R.Unified r -> Array.iter r ~f:print
      | R.Replace (ar1, ar2) ->
        Array.iter ar1 ~f:print;
        Array.iter ar2 ~f:print
    in
    List.iter hunk.ranges ~f:handle_range
  in
  List.iter hunks ~f;
  print "</pre>"
;;
