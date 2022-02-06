(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



type line = {
  number : int;
  hits : int
}

type class_ = {
  name : string;
  line_rate : float;
  lines : line list;
}

type package = {
  name : string;
  line_rate : float;
  classes : class_ list;
}

type cobertura = {
  lines_valid : int;
  lines_covered : int;
  line_rate : float;
  sources : string list;
  package : package;
}

let pp_list pp fmt =
  List.iter (fun x -> pp fmt x; Format.pp_print_string fmt "\n")

let pp_line fmt {number; hits} =
  Format.fprintf fmt "          <line number=\"%d\" hits=\"%d\"/>" number hits

let pp_lines fmt lines =
  let open Format in
  fprintf fmt "        <lines>\n%a        </lines>\n"
    (pp_list pp_line) lines

let pp_class_ fmt {name; line_rate; lines} =
  let open Format in
  let class_infos =
    Format.sprintf "name=\"%s\" filename=\"%s\" line-rate=\"%f\""
      name
      name
      line_rate
  in
  fprintf fmt
    "      <class %s>\n%a      </class>"
    class_infos
    pp_lines lines

let pp_classes fmt classes =
  let open Format in
  fprintf fmt
    "    <classes>\n%a    </classes>\n"
    (pp_list pp_class_) classes

let pp_package fmt {name; line_rate; classes} =
  let open Format in
  let package_infos =
    Format.sprintf {|name="%s" line-rate="%f"|}
      name
      line_rate
  in
  fprintf fmt "  <package %s>\n%a  </package>\n"
    package_infos
    pp_classes classes

let pp_source fmt source =
  Format.fprintf fmt "    <source>%s</source>" source

let pp_sources fmt sources =
  let open Format in
  fprintf fmt
    "  <sources>\n%a  </sources>\n"
    (pp_list pp_source) sources

let pp_cobertura fmt ({sources; package; _} as cobertura) =
  let open Format in
  let cobertura_infos {lines_valid; lines_covered; line_rate; _} =
    sprintf
      {|lines-valid="%d" lines-covered="%d" line-rate="%f"|}
      lines_valid
      lines_covered
      line_rate
  in
  fprintf fmt
    "<?xml version=\"1.0\" ?>\n<coverage %s>\n%a%a</coverage>"
    (cobertura_infos cobertura)
    pp_sources sources
    pp_package package

let line_rate (visited, total) =
  float_of_int visited /. float_of_int total

let update_counts counts line_counts =
  List.fold_left (fun ((visited, total) as acc) -> function
    | None -> acc
    | Some x when x > 0 -> (visited + 1, total + 1)
    | Some _ -> (visited, total + 1))
    counts line_counts

let line line hits =
  {number = line; hits}

let classes ~global_counts resolver coverage : class_ list =
  let class_ {Bisect_common.filename; points; counts} =
    match resolver ~filename with
    | None ->
      None
    | Some resolved_in_file ->
      let line_counts =
        Util.line_counts ~filename:resolved_in_file ~points ~counts in
      global_counts := update_counts !global_counts line_counts;
      let line_rate = line_rate (update_counts (0, 0) line_counts) in

      let i = ref 1 in
      let lines =
        List.fold_left (fun acc x ->
          let line =
            match x with
            | None -> None
            | Some nb ->
              Some (line !i nb)
          in
          let () = incr i in
          match line with
          | None -> acc
          | Some line -> line::acc)
          []
          line_counts
        |> List.rev
      in

      Some {name = filename; line_rate; lines}
  in

  Hashtbl.fold (fun _ file acc ->
    match class_ file with
    | None -> acc
    | Some x -> x::acc)
    coverage
    []

let package ~counts ~resolver ~coverage =
  let classes = classes ~global_counts:counts resolver coverage in
  let line_rate = line_rate !counts in
  {name = "."; line_rate; classes}

let cobertura ~resolver ~coverage =
  let counts = ref (0, 0) in
  let package = package ~counts ~resolver ~coverage in
  let sources = ["."] in
  let rate = line_rate !counts in
  {
    lines_valid = snd !counts;
    lines_covered = fst !counts;
    line_rate = rate;
    package;
    sources;
  }

let output
    ~to_file ~coverage_files ~coverage_paths ~source_paths ~ignore_missing_files
    ~expect ~do_not_expect =

  let coverage =
    Input.load_coverage
      ~coverage_files ~coverage_paths ~expect ~do_not_expect in
  let resolver =
    Util.find_source_file ~source_roots:source_paths ~ignore_missing_files in
  let cobertura = cobertura ~resolver ~coverage in
  let () = Util.mkdirs (Filename.dirname to_file) in
  let oc =
    try open_out to_file
    with Sys_error message ->
      Util.fatal "cannot open output file '%s': %s" to_file message
  in
  try
    let fmt = Format.formatter_of_out_channel oc in
    let () = pp_cobertura fmt cobertura in
    close_out oc
  with
  | Sys_error message ->
    Util.fatal "cannot write output file '%s': %s" to_file message
  | exn ->
    close_out_noerr oc;
    raise exn
