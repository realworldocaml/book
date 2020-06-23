module Common = Bisect_common

let command a =
  Printf.ksprintf (fun s ->
    let exit_code = Sys.command s in
    if exit_code <> 0 then
      exit exit_code) a

let note_temporary_file file =
  at_exit (fun () ->
    if Sys.file_exists file then
      Sys.remove file)

let read_lines file =
  let channel = open_in file in
  let rec read acc =
    match input_line channel with
    | line -> read (line::acc)
    | exception End_of_file -> close_in channel; List.rev acc
  in
  read []

type 'event ui_mode = {
  mutable scroll : int;
  lines : int;
  render : unit -> unit;
  event : 'event -> unit;
  scrolled : unit -> unit;
}

let () =
  (* Load the source file. *)

  let source_file =
    match Sys.argv.(1) with
    | file -> file
    | exception Invalid_argument _ ->
      prerr_endline "Usage: inspect.exe FILE";
      exit 1
  in
  let source_code = read_lines source_file in


  (* Instrument the source file, and load the result. *)

  let temporary_file_1 = "temporary_1" in
  let temporary_file_2 = "temporary_2" in
  note_temporary_file temporary_file_1;
  note_temporary_file temporary_file_2;

  command "_build/default/.ppx/bisect_ppx/ppx.exe %s > %s"
      source_file temporary_file_1;
  let instrumented_code = read_lines temporary_file_1 in

  let prefix = "sed 's/^[^\"]*\"//'" in
  let suffix = "sed 's/\"[^\"]*$//'" in
  command "grep '\"\\\\' %s | head -n 1 | %s | %s > %s"
    temporary_file_1 prefix suffix temporary_file_2;
  let points =
    read_lines temporary_file_2
    |> List.hd
    |> Scanf.unescaped
    |> Common.read_points
  in

  Sys.remove temporary_file_1;
  Sys.remove temporary_file_2;


  (* Run the UI. *)

  let open Notty in
  let open Notty_unix in

  let t = Term.create () in
  let viewport_height () = snd (Term.size t) - 1 in
  (* Term.cursor t (Some (0, viewport_height ())); *)

  let pad_line width s =
    s ^ (String.make (max 0 (width - String.length s)) ' ')
    |> fun s -> String.sub s 0 (min width (String.length s))
  in

  let render_code code =
    let width = fst (Term.size t) in
    code @ (Array.make (viewport_height ()) " " |> Array.to_list)
    |> List.map (pad_line width)
    |> List.map (I.string A.empty)
    |> I.vcat
  in

  let points =
    let line_start_offsets =
      List.fold_left (fun (offset, acc) line ->
        (String.length line + offset + 1, offset::acc)) (0, []) source_code
      |> snd
      |> List.rev
    in
    let look_up_point_location offset =
      let rec scan line_number line_start_offsets =
        match line_start_offsets with
        | this_offset::next_offset::more ->
          if offset < next_offset then
            (line_number, offset - this_offset)
          else
            scan (line_number + 1) (next_offset::more)
        | [this_offset] ->
          (line_number, offset - this_offset)
        | [] ->
          (line_number, 0)
      in
      scan 0 line_start_offsets
    in
    let look_up_point_character (line, column) =
      try (List.nth source_code line).[column]
      with Invalid_argument _ -> ' '
    in
    points
    |> List.map (fun {Common.offset; identifier} ->
      let location = look_up_point_location offset in
      (identifier, location, look_up_point_character location))
    |> List.mapi (fun index (identifier, loc, c) ->
      ((index, identifier), loc, c))
  in

  let find_point index =
    let (_, identifier), _, _ = List.nth points index in
    let pattern = Printf.sprintf "___bisect_visit___ %i" identifier in
    let regexp = Str.regexp_string pattern in
    let rec scan = fun line_number -> function
      | [] ->
        None
      | line::more ->
        match Str.search_forward regexp line 0 with
        | exception Not_found ->
          scan (line_number + 1) more
        | position ->
          Some (line_number, position, pattern)
    in
    scan 0 instrumented_code
  in

  let points_image =
    let attr = A.(fg lightyellow ++ bg lightblack ++ st underline ++ st bold) in
    let point_image (_, (line, column), c) =
      I.(pad ~l:column ~t:line @@ strf ~attr "%c" c) in
    points
    |> List.map point_image
    |> I.zcat
  in

  let line_highlight_image index =
    List.nth points index
    |> fun (_, (line, _), _) ->
      let bg = pad_line (fst (Term.size t)) (List.nth source_code line) in
      I.(pad ~t:line @@ string A.(bg lightblack) bg)
  in

  let current_point_image index =
    let attr = A.(fg black ++ bg lightyellow ++ st underline ++ st bold) in
    List.nth points index
    |> fun (_, (line, column), c) ->
      I.(pad ~l:column ~t:line @@ strf ~attr "%c" c)
  in

  let source_code_image = ref (render_code source_code) in
  let instrumented_image = ref (render_code instrumented_code) in
  let point_highlight = ref I.empty in

  let input = Buffer.create 256 in

  let status_line typing text =
    let width = fst (Term.size t) in
    let cursor_position =
      if typing then
        Buffer.length input
      else
        fst (Term.size t) - 2
    in
    Term.cursor t (Some (cursor_position, viewport_height ()));
    let text = pad_line width text in
    I.(pad ~t:(viewport_height ()) @@ string A.(fg black ++ bg lightwhite) text)
  in

  let current = ref 0 in

  let rec source_mode = {
    scroll = 0;
    lines = List.length source_code;
    render = begin fun () ->
      let view =
        I.(current_point_image !current
        </> points_image
        </> line_highlight_image !current
        </> !source_code_image)
        |> I.vcrop
          (0 + source_mode.scroll)
          (source_mode.lines - (viewport_height ()) - source_mode.scroll)
      in
      let status_line =
        if Buffer.length input = 0 then
          Printf.ksprintf (status_line false) "Point %i" !current
        else
          status_line true (Buffer.contents input)
      in
      I.(status_line </> view) |> Term.image t
    end;
    event = begin function
      | `Key (`Enter, _) ->
        if Buffer.length input = 0 then begin
          mode := instrumented_mode;
          match find_point !current with
          | None ->
            ()
          | Some (line, column, text) ->
            let attr = A.(fg black ++ bg lightyellow ++ st bold) in
            point_highlight := I.(pad ~t:line ~l:column @@ string attr text);
            show_line line
        end
        else begin
          let index = Buffer.contents input |> int_of_string in
          Buffer.clear input;
          let index = max 0 index in
          let index = min (List.length points - 1) index in
          current := index;
          show_point ()
        end
      | `Key (`Arrow `Right, _) ->
        current := min (List.length points - 1) (!current + 1);
        show_point ()
      | `Key (`Arrow `Left, _) ->
        current := max 0 (!current - 1);
        show_point ()
      | `Key (`ASCII ('0'..'9' as c), _) ->
        Buffer.add_char input c
      | `Key (`Backspace, _) ->
        if Buffer.length input > 0 then
          Buffer.truncate input (Buffer.length input - 1)
      | _ ->
        ()
    end;
    scrolled = begin fun () ->
      let point_visible (_, (line, _), _) =
        line >= source_mode.scroll &&
        line < source_mode.scroll + viewport_height ()
      in
      if point_visible (List.nth points !current) then
        ()
      else
        try
          points
          |> List.filter point_visible
          |> List.sort (fun (_, (line, column), _) (_, (line', column'), _) ->
            match compare line line' with
            | 0 -> compare column column'
            | order -> order)
          |> List.hd
          |> fun ((index, _), _, _) -> current := index
        with Failure _ ->
          ()
    end;
  }
  and instrumented_mode = {
    scroll = 0;
    lines = List.length instrumented_code;
    render = begin fun () ->
      I.(status_line false "Press ENTER to return"
      </>
      (!point_highlight
      </> !instrumented_image
      |> vcrop
        (0 + instrumented_mode.scroll)
        (instrumented_mode.lines -
          (viewport_height ()) - instrumented_mode.scroll)))
      |> Term.image t
    end;
    event = begin function
      | `Key (`Enter, _) -> mode := source_mode
      | _ -> ()
    end;
    scrolled = ignore;
  }
  and mode = ref source_mode
  and clamp line =
    let line = max 0 line in
    let line = min (!mode.lines - viewport_height ()) line in
    !mode.scroll <- line
  and scroll offset =
    clamp (!mode.scroll + offset)
  and show_line line =
    let padding = viewport_height () / 2 in
    if line < !mode.scroll then
      clamp (line - padding)
    else
      if line >= !mode.scroll + viewport_height () then
        clamp (line - padding)
  and show_point () =
    match List.nth points !current with
    | _, (line, _), _ -> show_line line
    | exception Invalid_argument _ -> ()
  in

  (* TODO Go to selected point. *)
  let done_ = ref false in
  while not !done_ do
    !mode.render ();
    match Term.event t with
    | `Key (`ASCII 'q', _) -> done_ := true
    | `Key (`Arrow `Down, _) -> scroll 1; !mode.scrolled ();
    | `Key (`Arrow `Up, _) -> scroll (-1); !mode.scrolled ();
    | `Key (`ASCII ' ', _) -> scroll (viewport_height ()); !mode.scrolled ();
    | `Key (`ASCII 'b', _) -> scroll (-(viewport_height ())); !mode.scrolled ();
    | `Key (`ASCII 'g', []) -> !mode.scroll <- 0; !mode.scrolled ();
    | `Key (`ASCII 'G', []) ->
      !mode.scroll <- !mode.lines - viewport_height (); !mode.scrolled ();
    | event -> !mode.event event
  done
