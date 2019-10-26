open Core
open Import

let lines_of_contents contents =
  let lines = Array.of_list (String.split_lines contents) in
  let has_trailing_newline =
    let length = String.length contents in
    if length = 0 || Char.equal contents.[length - 1] '\n'
    then `With_trailing_newline
    else `Missing_trailing_newline
  in
  lines, has_trailing_newline
;;

let%test_unit _ =
  let test contents ~expect =
    [%test_result: string array * [`With_trailing_newline | `Missing_trailing_newline]]
      (lines_of_contents contents)
      ~expect
  in
  test "" ~expect:([||], `With_trailing_newline);
  test "hello" ~expect:([| "hello" |], `Missing_trailing_newline);
  test "hello\nworld" ~expect:([| "hello"; "world" |], `Missing_trailing_newline);
  test "hello\nworld\n" ~expect:([| "hello"; "world" |], `With_trailing_newline)
;;

(* Returns a Hunk.t list, ready to be printed *)
let compare_lines config ~prev ~next =
  let module C = Configuration in
  (* Create the diff *)
  let context = config.C.context in
  let keep_ws = config.C.keep_ws in
  let split_long_lines = config.C.split_long_lines in
  let line_big_enough = config.C.line_big_enough in
  let hunks =
    let transform = if keep_ws then Fn.id else Patdiff_core.remove_ws in
    (* Use external compare program? *)
    match config.C.ext_cmp with
    | None ->
      Patience_diff.String.get_hunks
        ~transform
        ~context
        ~big_enough:line_big_enough
        ~prev
        ~next
    | Some prog ->
      let compare x y =
        let cmd = sprintf "%s %S %S" prog x y in
        match Unix.system cmd with
        | Ok () -> 0
        | Error (`Exit_non_zero 1) -> 1
        | Error _ -> failwithf "External compare %S failed!" prog ()
      in
      let module P = Patience_diff.Make (struct
                       type t = string [@@deriving sexp]

                       let hash = String.hash
                       let compare = compare
                     end)
      in
      P.get_hunks ~transform ~context ~big_enough:line_big_enough ~prev ~next
  in
  let hunks =
    match config.C.float_tolerance with
    | None -> hunks
    | Some tolerance -> Float_tolerance.apply hunks tolerance ~context
  in
  (* Refine if desired *)
  if config.C.unrefined
  then
    (* Turn `Replace ranges into `Prev and `Next ranges.
       `Replace's would otherwise be later interpreted as refined output *)
    Patience_diff.Hunks.unified hunks
  else (
    let rules = config.C.rules in
    let output = config.C.output in
    let produce_unified_lines = config.C.produce_unified_lines in
    let interleave = config.C.interleave in
    let word_big_enough = config.C.word_big_enough in
    Patdiff_core.refine
      ~rules
      ~output
      ~keep_ws
      ~produce_unified_lines
      ~split_long_lines
      ~interleave
      hunks
      ~word_big_enough)
;;

let warn_if_no_trailing_newline
      ~warn_if_no_trailing_newline_in_both
      (prev_file_newline, prev_file)
      (next_file_newline, next_file)
  =
  let warn = eprintf "No newline at the end of %s\n%!" in
  match prev_file_newline, next_file_newline with
  | `With_trailing_newline, `With_trailing_newline -> ()
  | `With_trailing_newline, `Missing_trailing_newline -> warn next_file
  | `Missing_trailing_newline, `With_trailing_newline -> warn prev_file
  | `Missing_trailing_newline, `Missing_trailing_newline ->
    if warn_if_no_trailing_newline_in_both
    then (
      warn prev_file;
      warn next_file)
;;

(* Returns a Hunk.t list, ready to be printed *)
let compare_files (config : Configuration.t) ~prev_file ~next_file =
  let prev = In_channel.read_all prev_file in
  let next = In_channel.read_all next_file in
  Comparison_result.create
    config
    ~prev:{ name = prev_file; text = prev }
    ~next:{ name = next_file; text = next }
    ~compare_assuming_text:(fun config ~prev ~next ->
      let prev_lines, prev_file_newline = lines_of_contents prev.text in
      let next_lines, next_file_newline = lines_of_contents next.text in
      warn_if_no_trailing_newline
        (prev_file_newline, prev.name)
        (next_file_newline, next.name)
        ~warn_if_no_trailing_newline_in_both:config.warn_if_no_trailing_newline_in_both;
      compare_lines config ~prev:prev_lines ~next:next_lines)
;;

let binary_different_message
      ~(config : Configuration.t)
      ~prev_file
      ~prev_is_binary
      ~next_file
      ~next_is_binary
  =
  match config.location_style with
  | Diff ->
    sprintf
      "Files %s%s and %s%s differ"
      prev_file
      (if prev_is_binary then " (binary)" else "")
      next_file
      (if next_is_binary then " (binary)" else "")
  | Omake ->
    String.concat
      [ error_message_start ~file:prev_file ~line:1
      ; "\n"
      ; "  File \""
      ; next_file
      ; "\"\n"
      ; "  binary files differ\n"
      ]
;;

(* Print hunks to stdout *)
let print hunks ~prev_file ~next_file ~config =
  let module C = Configuration in
  if Comparison_result.has_no_diff hunks
  then (
    if config.C.double_check
    then (
      match Unix.system (sprintf "cmp -s %s %s" prev_file next_file) with
      | Ok () -> ()
      | Error (`Exit_non_zero 1) ->
        printf "There are no differences except those filtered by your settings\n%!"
      | Error _ -> ()))
  else if (* Only print if -quiet is not set *)
    not config.C.quiet
  then (
    let output = config.C.output in
    let rules = config.C.rules in
    (* Substitute prev/new_alt for the filenames in the final output *)
    let prev_file = Option.value ~default:prev_file config.C.old_alt in
    let next_file = Option.value ~default:next_file config.C.new_alt in
    match hunks with
    | Binary_same -> assert false
    | Binary_different { prev_is_binary; next_is_binary } ->
      Printf.printf
        "%s\n"
        (binary_different_message
           ~config
           ~prev_file
           ~prev_is_binary
           ~next_file
           ~next_is_binary)
    | Hunks hunks ->
      Patdiff_core.print
        hunks
        ~prev_file
        ~next_file
        ~output
        ~rules
        ~location_style:config.location_style)
;;

let diff_files config ~prev_file ~next_file =
  let hunks = compare_files ~prev_file ~next_file config in
  print hunks ~prev_file ~next_file ~config;
  if Comparison_result.has_no_diff hunks then `Same else `Different
;;

let diff_strings
      ?print_global_header
      (config : Configuration.t)
      ~(prev : Patdiff_core.diff_input)
      ~(next : Patdiff_core.diff_input)
  =
  let lines { Patdiff_core.name = _; text } = String.split_lines text |> Array.of_list in
  let hunks =
    Comparison_result.create
      config
      ~prev
      ~next
      ~compare_assuming_text:(fun config ~prev ~next ->
        compare_lines config ~prev:(lines prev) ~next:(lines next))
  in
  if Comparison_result.has_no_diff hunks
  then `Same
  else
    `Different
      (match hunks with
       | Binary_same -> assert false
       | Binary_different { prev_is_binary; next_is_binary } ->
         binary_different_message
           ~config
           ~prev_file:prev.name
           ~prev_is_binary
           ~next_file:next.name
           ~next_is_binary
       | Hunks hunks ->
         Patdiff_core.output_to_string
           hunks
           ?print_global_header
           ~file_names:(prev.name, next.name)
           ~output:config.output
           ~rules:config.rules
           ~location_style:config.location_style)
;;

(* True if a file is a regular file *)
let is_reg path = (Unix.stat path).Unix.st_kind = Unix.S_REG
let is_dir path = (Unix.stat path).Unix.st_kind = Unix.S_DIR

let rec diff_dirs config ~prev_file ~next_file ~file_filter =
  let module C = Configuration in
  (* Get a list of files for this directory only; do not descend farther
     (We recursively call diff_dirs later if we need to descend.) *)
  let options =
    { Find_files.Options.default with max_depth = Some 1; filter = file_filter }
  in
  let set_of_file file =
    let files = Find_files.find_all ~options file in
    let f (n, _s) = Filename_extended.make_relative ~to_:file n in
    let names = List.map files ~f in
    String.Set.of_list names
  in
  let prev_set = set_of_file prev_file in
  let next_set = set_of_file next_file in
  (* Get unique files *)
  let union = Set.union prev_set next_set in
  let prev_uniques = Set.diff union next_set in
  let next_uniques = Set.diff union prev_set in
  let handle_unique file ~dir ~is_prev =
    printf "Only in %s: %s\n%!" dir file;
    (* Diff unique files against /dev/null, if desired *)
    if not config.C.mask_uniques
    then (
      let path = dir ^/ file in
      if is_reg path
      then (
        let diff = diff_files config in
        let null = "/dev/null" in
        if is_prev
        then ignore (diff ~prev_file:path ~next_file:null)
        else ignore (diff ~prev_file:null ~next_file:path)))
  in
  Set.iter prev_uniques ~f:(handle_unique ~dir:prev_file ~is_prev:true);
  Set.iter next_uniques ~f:(handle_unique ~dir:next_file ~is_prev:false);
  (* Get differences *)
  let inter = Set.inter prev_set next_set in
  let exit_code = ref `Same in
  let diff file =
    let prev_file = prev_file ^/ file in
    let next_file = next_file ^/ file in
    if is_reg prev_file && is_reg next_file
    then (
      let hunks = compare_files ~prev_file ~next_file config in
      if not (Comparison_result.has_no_diff hunks)
      then (
        exit_code := `Different;
        (* Print the diff if not -quiet *)
        if config.C.quiet = false
        then print hunks ~prev_file ~next_file ~config
        else printf "Files %s and %s differ\n%!" prev_file next_file))
    else if is_dir prev_file && is_dir next_file
    then
      if not config.C.shallow
      then (
        match diff_dirs ~prev_file ~next_file config ~file_filter with
        | `Same -> ()
        | `Different -> exit_code := `Different)
      else printf "Common subdirectories: %s and %s\n%!" prev_file next_file
    else (
      exit_code := `Different;
      printf "Files %s and %s are not the same type\n%!" prev_file next_file)
  in
  Set.iter inter ~f:diff;
  if Set.is_empty prev_uniques && Set.is_empty next_uniques
  then !exit_code
  else `Different
;;
