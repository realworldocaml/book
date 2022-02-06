(* This file is part of Bisect_ppx, released under the MIT license. See
   LICENSE.md for details, or visit
   https://github.com/aantron/bisect_ppx/blob/master/LICENSE.md. *)



let verbose =
  ref false

let info arguments =
  Printf.ksprintf (fun s ->
    if !verbose then
      Printf.printf "Info: %s\n%!" s) arguments

let fatal arguments =
  Printf.ksprintf (fun s ->
    Printf.eprintf "Error: %s\n%!" s; exit 1) arguments



let split f list =
  let rec split acc list =
    match list with
    | head::tail ->
      if f head then split (head::acc) tail
      else (List.rev acc), list
    | [] ->
      (List.rev acc), []
  in
  split [] list



let mkdirs directory =
  let rec make directory =
    if not (Sys.file_exists directory) then begin
      make (Filename.dirname directory);
      Unix.mkdir directory 0o755
    end in
  try make directory
  with Unix.(Unix_error (error, _, path)) ->
    fatal "cannot create directory '%s': %s" path (Unix.error_message error)

let workspace_root =
  lazy begin
    let rec loop path =
      let parent = Filename.dirname path in
      let parent_result =
        if parent <> path && not (Filename.is_relative parent) then
          loop parent
        else
          None
      in
      match parent_result with
      | Some _ -> parent_result
      | None ->
        if Sys.file_exists (Filename.concat path "dune-workspace") then
          Some path
        else
          None
    in
    loop (Sys.getcwd ())
  end

let find_dune_workspace_root () =
  Lazy.force workspace_root

let find_source_file ~source_roots ~ignore_missing_files ~filename =
  let fail () =
    let message =
      source_roots
      |> List.map (Printf.sprintf "  - %s")
      |> fun text ->
        (Printf.sprintf "cannot find source file '%s' in:" filename)::text
      |> String.concat "\n"
    in
    if ignore_missing_files then begin
      info "%s" message;
      None
    end
    else
      fatal "%s\nHint: consider passing --ignore-missing-files." message
  in
  let rec search = function
    | head::tail ->
      let f' = Filename.concat head filename in
      if Sys.file_exists f' then
        Some f'
      else
        search tail
    | [] ->
      fail ()
  in
  if Filename.is_implicit filename then
    search source_roots
  else if Sys.file_exists filename then
    Some filename
  else
    fail ()



let line_counts ~filename ~points ~counts =
  let len = Array.length counts in
  let points =
    points
    |> Array.to_list
    |> List.mapi (fun index offset -> (offset, index))
    |> List.sort compare
  in
  let pts =
    points |> List.map (fun (offset, index) ->
      let nb =
        if index < len then
          counts.(index)
        else
          0
      in
      (offset, nb))
  in
  let in_channel =
    try open_in filename
    with Sys_error message ->
      fatal "cannot open source file '%s': %s" filename message
  in
  let line_counts =
    try
      let rec read number acc pts =
        match input_line in_channel with
        | exception End_of_file -> List.rev acc
        | _ ->
          let end_ofs = pos_in in_channel in
          let before, after = split (fun (o, _) -> o < end_ofs) pts in
          let visited_lowest =
            List.fold_left (fun v (_, nb) ->
              match v with
              | None -> Some nb
              | Some nb' -> if nb < nb' then Some nb else Some nb')
              None
              before
          in
          read (number + 1) (visited_lowest::acc) after
      in
      read 1 [] pts
    with exn ->
      close_in_noerr in_channel;
      match exn with
      | Sys_error message ->
        fatal "cannot read source file '%s': %s" filename message
      | _ ->
        raise exn
  in
  close_in_noerr in_channel;
  line_counts
